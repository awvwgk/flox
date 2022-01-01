! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_parser
  use, intrinsic :: iso_fortran_env, only : lox_real => real64
  use flox_constants, only : i8
  use flox_diagnostic, only : lox_diagnostic, label_type, level_syntax_error, level_eof_error
  use flox_ast, only : lox_ast, lox_stmt, lox_expr, &
    & lox_block, lox_expr_stmt, lox_print, lox_var, lox_if, lox_while, lox_fun, lox_return, &
    & lox_assign, lox_logical, lox_binary, lox_grouping, lox_literal, lox_unary, lox_call
  use flox_scanner, only : lox_token, &
    & LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, &
    & COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR, &
    & BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, &
    & IDENTIFIER, STRING, NUMBER, &
    & COMMENT, SPACE, NEWLINE, EOF, INVALID, &
    & line_descriptor
  implicit none
  private

  public :: lox_parser


  type :: lox_parser
    integer(i8) :: id = 0
    logical :: panic = .false.
    type(lox_diagnostic), allocatable :: diag(:)
  contains
    procedure, private :: uid
    procedure :: parse
  end type lox_parser

contains

  !> Create unique identifier
  function uid(self) result(id)
    class(lox_parser), intent(inout) :: self
    integer(i8) :: id
    self%id = self%id + 1_i8
    id = self%id
  end function uid

  !> Parse a whole program with the following grammar rule
  !>
  !>    program -> statement* EOF ;
  subroutine parse(self, tokens, ast)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    type(lox_ast), intent(out) :: ast

    class(lox_stmt), allocatable :: stmt
    integer :: pos

    pos = 0
    call next(tokens, pos)
    do while(pos < size(tokens))
      call declaration(self, tokens, pos, stmt)
      if (self%panic) then
        self%panic = .false.
        if (allocated(stmt)) deallocate(stmt)
        call synchronize(tokens, pos)
        exit
      end if
      if (allocated(stmt)) then
        call ast%add(stmt)
      end if
    end do
  end subroutine parse

  subroutine synchronize(tokens, pos)
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos

    pos = pos + 1
    do while(pos < size(tokens))
      if (.not.match(tokens, pos, [SEMICOLON])) then
        pos = pos + 1
        cycle
      end if
      exit
    end do
    call next(tokens, pos)
  end subroutine synchronize

  !> Parses a declaration with the following grammar rule
  !>
  !>    declaration -> fun_decl
  !>                   | var_decl
  !>                   | statement ;
  !>    fun_decl    -> "fun" function ;
  !>    var_decl    -> "var" IDENTIFIER ( "=" expression )? ";"
  subroutine declaration(self, tokens, pos, stmt)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_stmt), allocatable, intent(out) :: stmt

    if (match(tokens, pos, [IDENTIFIER])) then
      select case(tokens(pos)%val)
      case("fun")
        call next(tokens, pos)
        call fun_decl(self, tokens, pos, stmt)
        return
      case("var")
        call next(tokens, pos)
        call var_decl(self, tokens, pos, stmt)
        return
      end select
    end if

    call statement(self, tokens, pos, stmt)
  end subroutine declaration

  !> Parses a function declaration with the following grammar rule
  !>
  !>    fun_decl    -> "fun" function ;
  !>    function    -> IDENTIFIER "(" parameters? ")" block ;
  !>    parameters  -> IDENTIFIER ( "," IDENTIFIER )* ;
  subroutine fun_decl(self, tokens, pos, stmt)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_stmt), allocatable, intent(out) :: stmt

    integer :: last
    class(lox_stmt), allocatable :: blck
    type(lox_token), allocatable :: name, params(:)

    last = pos
    call consume(self, tokens, pos, IDENTIFIER, "Expect function name")
    if (self%panic) return
    name = tokens(last)

    call consume(self, tokens, pos, LEFT_PAREN, "Expect '(' after function name")
    if (self%panic) return

    params = [lox_token::]
    if (.not.match(tokens, pos, [RIGHT_PAREN])) then
      last = pos
      call consume(self, tokens, pos, IDENTIFIER, "Expect parameter name")
      if (self%panic) return
      params = [tokens(last)]

      do while(match(tokens, pos, [COMMA]))
        call next(tokens, pos)

        last = pos
        call consume(self, tokens, pos, IDENTIFIER, "Expect parameter name")
        if (self%panic) return
        params = [params, tokens(last)]
      end do
    end if

    call consume(self, tokens, pos, RIGHT_PAREN, "Expect ')' after parameters")
    if (self%panic) return

    call consume(self, tokens, pos, LEFT_BRACE, "Expect '{' before function body")
    if (self%panic) return

    call block_statement(self, tokens, pos, blck)
    if (self%panic) return

    call consume(self, tokens, pos, RIGHT_BRACE, "Expect '}' after function body")
    if (self%panic) return

    stmt = lox_fun(self%uid(), name, params, blck)
  end subroutine fun_decl

  !> Parses a variable declaration with the following grammar rule
  !>
  !>    var_decl    -> "var" IDENTIFIER ( "=" expression )? ";"
  subroutine var_decl(self, tokens, pos, stmt)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_stmt), allocatable, intent(out) :: stmt

    integer :: last
    class(lox_expr), allocatable :: expr

    last = pos
    call consume(self, tokens, pos, IDENTIFIER, "Expect variable name")
    if (self%panic) return

    if (match(tokens, pos, [EQUAL])) then
      call next(tokens, pos)
      call expression(self, tokens, pos, expr)
      if (self%panic) return
    end if

    call consume(self, tokens, pos, SEMICOLON, "Expect ';' after variable declaration")
    if (self%panic) return
    stmt = lox_var(self%uid(), tokens(last), expr)
  end subroutine var_decl


  !> Parses a single statement with the following grammar rule
  !>
  !>    statement        -> expr_statement
  !>                        | for_statement
  !>                        | if_statement
  !>                        | while_statement
  !>                        | print_statement
  !>                        | return_statement
  !>                        | block_statement ;
  !>    print_statement  -> "print" expression ";" ;
  !>    return_statement -> "return" expression? ";" ;
  !>    block_statement  -> "{" declaration* "}" ;
  !>    if_statement     -> "if" "(" expression ")" statement ("else" statement)? ;
  !>    while_statement  -> "while" "(" expression ")" statement ;
  !>    for_statement    -> "for" "(" ( var_decl | expr_stmt | ";" )
  !>                       expression? ";" expression? ")" statement ;
  subroutine statement(self, tokens, pos, stmt)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_stmt), allocatable, intent(out) :: stmt

    integer :: last

    if (match(tokens, pos, [IDENTIFIER])) then
      select case(tokens(pos)%val)
      case("for")
        call next(tokens, pos)
        call for_statement(self, tokens, pos, stmt)
        return
      case("if")
        call next(tokens, pos)
        call if_statement(self, tokens, pos, stmt)
        return
      case("while")
        call next(tokens, pos)
        call while_statement(self, tokens, pos, stmt)
        return
      case("print")
        call next(tokens, pos)
        call print_statement(self, tokens, pos, stmt)
        return
      case("return")
        last = pos
        call next(tokens, pos)
        call return_statement(self, tokens, pos, last, stmt)
        return
      end select
    end if

    if (match(tokens, pos, [LEFT_BRACE])) then
      call next(tokens, pos)
      call block_statement(self, tokens, pos, stmt)
      if (self%panic) return

      call consume(self, tokens, pos, RIGHT_BRACE, "Expect '}' after block")
      return
    end if

    call expr_statement(self, tokens, pos, stmt)

  end subroutine statement

  !> Parses a conditional statement with the following grammar rule
  !>
  !>    if_statement    -> "if" "(" expression ")" statement ("else" statement)? ;
  subroutine if_statement(self, tokens, pos, stmt)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_stmt), allocatable, intent(out) :: stmt

    class(lox_expr), allocatable :: cond
    class(lox_stmt), allocatable :: then_stmt, else_stmt

    call consume(self, tokens, pos, LEFT_PAREN, "Expect '(' after 'if'")
    if (self%panic) return

    call expression(self, tokens, pos, cond)
    if (self%panic) return

    call consume(self, tokens, pos, RIGHT_PAREN, "Expect ')' after condition")
    if (self%panic) return

    call statement(self, tokens, pos, then_stmt)
    if (self%panic) return

    if (match(tokens, pos, [IDENTIFIER])) then
      select case(tokens(pos)%val)
      case("else")
        call next(tokens, pos)
        call statement(self, tokens, pos, else_stmt)
        if (self%panic) return
      end select
    end if

    stmt = lox_if(self%uid(), cond, then_stmt, else_stmt)
  end subroutine if_statement

  !> Parses a conditional statement with the following grammar rule
  !>
  !>    for_statement -> "for" "(" ( var_decl | expr_statment | ";" )
  !>                     expression? ";" expression? ")" statement ;
  subroutine for_statement(self, tokens, pos, stmt)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_stmt), allocatable, intent(out) :: stmt

    class(lox_expr), allocatable :: cond, incr
    class(lox_stmt), allocatable :: init, body
    integer :: last

    call consume(self, tokens, pos, LEFT_PAREN, "Expect '(' after 'for'")
    if (self%panic) return

    if (match(tokens, pos, [SEMICOLON])) then
      call next(tokens, pos)
    else if (match(tokens, pos, [IDENTIFIER]) .and. tokens(pos)%val == "var") then
      call next(tokens, pos)
      call var_decl(self, tokens, pos, init)
      if (self%panic) return
    else
      call expr_statement(self, tokens, pos, init)
      if (self%panic) return
    end if

    if (.not.match(tokens, pos, [SEMICOLON])) then
      call expression(self, tokens, pos, cond)
      if (self%panic) return
    else
      associate(true => lox_token(tokens(pos)%first, 1, IDENTIFIER, "true"))
        cond = lox_literal(self%uid(), true)
      end associate
    end if

    call consume(self, tokens, pos, SEMICOLON, "Expect ';' after loop condition in 'for'")
    if (self%panic) return

    if (.not.match(tokens, pos, [RIGHT_PAREN])) then
      call expression(self, tokens, pos, incr)
      if (self%panic) return
    end if

    call consume(self, tokens, pos, RIGHT_PAREN, "Expect ')' after loop increment in 'for'")
    if (self%panic) return

    call statement(self, tokens, pos, body)
    if (self%panic) return

    if (allocated(incr)) then
      block
        class(lox_block), allocatable :: blck
        class(lox_stmt), allocatable :: incs

        incs = lox_expr_stmt(self%uid(), incr)
        allocate(blck)
        call blck%add(body)
        call blck%add(incs)
        call move_alloc(blck, body)
      end block
    end if

    stmt = lox_while(self%uid(), cond, body)

    if (allocated(init)) then
      block
        class(lox_block), allocatable :: blck

        allocate(blck)
        call blck%add(init)
        call blck%add(stmt)
        call move_alloc(blck, stmt)
      end block
    end if
  end subroutine for_statement

  !> Parses a conditional statement with the following grammar rule
  !>
  !>    while_statement -> "while" "(" expression ")" statement ;
  subroutine while_statement(self, tokens, pos, stmt)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_stmt), allocatable, intent(out) :: stmt

    class(lox_expr), allocatable :: cond
    class(lox_stmt), allocatable :: body

    call consume(self, tokens, pos, LEFT_PAREN, "Expect '(' after 'while'")
    if (self%panic) return

    call expression(self, tokens, pos, cond)
    if (self%panic) return

    call consume(self, tokens, pos, RIGHT_PAREN, "Expect ')' after condition")
    if (self%panic) return

    call statement(self, tokens, pos, body)
    if (self%panic) return

    stmt = lox_while(self%uid(), cond, body)
  end subroutine while_statement

  !> Parses a block statement with the following grammar rule
  !>
  !>    block_statement -> "{" declaration* "}" ;
  subroutine block_statement(self, tokens, pos, stmt)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_stmt), allocatable, intent(out) :: stmt

    type(lox_block), allocatable :: blck

    allocate(blck)

    do while(.not.match(tokens, pos, [RIGHT_BRACE]) .and. pos < size(tokens))
      call declaration(self, tokens, pos, stmt)
      if (self%panic) then
        self%panic = .false.
        if (allocated(stmt)) deallocate(stmt)
        call synchronize(tokens, pos)
        exit
      end if
      if (allocated(stmt)) then
        call blck%add(stmt)
      end if
    end do
    call move_alloc(blck, stmt)

  end subroutine block_statement

  !> Parses a single statement with the following grammar rule
  !>
  !>    print_statement -> "print" expression ";" ;
  subroutine print_statement(self, tokens, pos, stmt)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_stmt), allocatable, intent(out) :: stmt

    class(lox_expr), allocatable :: expr

    call expression(self, tokens, pos, expr)
    if (self%panic) return
    call consume(self, tokens, pos, SEMICOLON, "Expect ';' after expression")
    if (self%panic) return

    stmt = lox_print(self%uid(), expr)
  end subroutine print_statement

  !> Parses a single statement with the following grammar rule
  !>
  !>    return_statement -> "print" expression? ";" ;
  subroutine return_statement(self, tokens, pos, last, stmt)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    integer, intent(in) :: last
    class(lox_stmt), allocatable, intent(out) :: stmt

    class(lox_expr), allocatable :: expr

    if (.not.match(tokens, pos, [SEMICOLON])) then
      call expression(self, tokens, pos, expr)
      if (self%panic) return
    end if

    call consume(self, tokens, pos, SEMICOLON, "Expect ';' after return value")
    if (self%panic) return

    stmt = lox_return(self%uid(), tokens(last), expr)
  end subroutine return_statement

  !> Parses a single statement with the following grammar rule
  !>
  !>    expr_statement -> expression ";" ;
  subroutine expr_statement(self, tokens, pos, stmt)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_stmt), allocatable, intent(out) :: stmt

    class(lox_expr), allocatable :: expr

    call expression(self, tokens, pos, expr)
    if (self%panic) return
    call consume(self, tokens, pos, SEMICOLON, "Expect ';' after expression")
    if (self%panic) return

    stmt = lox_expr_stmt(self%uid(), expr)
  end subroutine expr_statement

  !> Parse an expression with the following grammar rule
  !>
  !>    expression -> assignment ;
  recursive subroutine expression(self, tokens, pos, expr)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_expr), allocatable :: expr

    call assignment(self, tokens, pos, expr)
    if (self%panic) return
  end subroutine expression

  !> Parse an assignment with the following grammar rule
  !>
  !>    assignment -> IDENTIFIER "=" assignment
  !>                  | logic_or ;
  recursive subroutine assignment(self, tokens, pos, expr)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_expr), allocatable :: expr

    class(lox_expr), allocatable :: left, right
    integer :: last

    call logic_or(self, tokens, pos, left)
    if (self%panic) return

    if (match(tokens, pos, [EQUAL])) then
      last = pos
      call next(tokens, pos)
      call assignment(self, tokens, pos, right)
      if (self%panic) return

      select type(left)
      type is(lox_literal)
        select case(left%object%ttype)
        case(IDENTIFIER)
          expr = lox_assign(self%uid(), left%object, right)
          return
        case default
          call error(self, left%object, "Cannot assign to literal value.", .false.)
          return
        end select
      end select

      call error(self, tokens(last), "Invalid assignment target", .false.)
    else
      call move_alloc(left, expr)
    end if
  end subroutine assignment

  !> Parse a logical or with the following grammar rule
  !>
  !>    logic_or -> logic_and ("or" logic_and)* ;
  recursive subroutine logic_or(self, tokens, pos, expr)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_expr), allocatable :: expr

    class(lox_expr), allocatable :: left, right
    integer :: last

    call logic_and(self, tokens, pos, left)
    if (self%panic) return

    do while(match(tokens, pos, [IDENTIFIER]) .and. tokens(pos)%val == "or")
      last = pos
      call next(tokens, pos)
      call logic_and(self, tokens, pos, right)
      if (self%panic) return

      expr = lox_logical(self%uid(), left, tokens(last), right)
      call move_alloc(expr, left)
    end do

    call move_alloc(left, expr)
  end subroutine logic_or

  !> Parse a logical and with the following grammar rule
  !>
  !>    logic_and -> equality ("and" equality)* ;
  recursive subroutine logic_and(self, tokens, pos, expr)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_expr), allocatable :: expr

    class(lox_expr), allocatable :: left, right
    integer :: last

    call equality(self, tokens, pos, left)
    if (self%panic) return

    do while(match(tokens, pos, [IDENTIFIER]) .and. tokens(pos)%val == "and")
      last = pos
      call next(tokens, pos)
      call equality(self, tokens, pos, right)
      if (self%panic) return

      expr = lox_logical(self%uid(), left, tokens(last), right)
      call move_alloc(expr, left)
    end do

    call move_alloc(left, expr)
  end subroutine logic_and

  !> Parse an expression with the following grammar rule
  !>
  !>    equality -> comparison ( ( "!=" | "==" ) comparison)* ;
  recursive subroutine equality(self, tokens, pos, expr)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_expr), allocatable, intent(out) :: expr

    class(lox_expr), allocatable :: left, right
    integer :: last

    call comparison(self, tokens, pos, left)
    if (self%panic) return

    do while(match(tokens, pos, [EQUAL_EQUAL, BANG_EQUAL]))
      last = pos
      call next(tokens, pos)
      call comparison(self, tokens, pos, right)
      if (self%panic) return
      expr = lox_binary(self%uid(), left, tokens(last), right)
      call move_alloc(expr, left)
    end do

    call move_alloc(left, expr)
  end subroutine equality

  !> Parse an expression with the following grammar rule
  !>
  !>    comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term)* ;
  recursive subroutine comparison(self, tokens, pos, expr)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_expr), allocatable, intent(out) :: expr

    class(lox_expr), allocatable :: left, right
    integer :: last

    call term(self, tokens, pos, left)
    if (self%panic) return

    do while(match(tokens, pos, [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]))
      last = pos
      call next(tokens, pos)
      call term(self, tokens, pos, right)
      if (self%panic) return
      expr = lox_binary(self%uid(), left, tokens(last), right)
      call move_alloc(expr, left)
    end do

    call move_alloc(left, expr)
  end subroutine comparison

  !> Parse an expression with the following grammar rule
  !>
  !>    term -> factor ( ( "-" | "+" ) factor)* ;
  recursive subroutine term(self, tokens, pos, expr)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_expr), allocatable, intent(out) :: expr

    class(lox_expr), allocatable :: left, right
    integer :: last

    call factor(self, tokens, pos, left)
    if (self%panic) return

    do while(match(tokens, pos, [MINUS, PLUS]))
      last = pos
      call next(tokens, pos)
      call factor(self, tokens, pos, right)
      if (self%panic) return
      expr = lox_binary(self%uid(), left, tokens(last), right)
      call move_alloc(expr, left)
    end do

    call move_alloc(left, expr)
  end subroutine term

  !> Parse an expression with the following grammar rule
  !>
  !>    factor -> unary ( ( "/" | "*" ) unary)* ;
  recursive subroutine factor(self, tokens, pos, expr)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_expr), allocatable, intent(out) :: expr

    class(lox_expr), allocatable :: left, right
    integer :: last

    call unary(self, tokens, pos, left)
    if (self%panic) return

    do while(match(tokens, pos, [STAR, SLASH]))
      last = pos
      call next(tokens, pos)
      call unary(self, tokens, pos, right)
      if (self%panic) return
      expr = lox_binary(self%uid(), left, tokens(last), right)
      call move_alloc(expr, left)
    end do

    call move_alloc(left, expr)
  end subroutine factor

  !> Parse an expression with the following grammar rule
  !>
  !>    unary -> ( "!" | "-" ) unary | fun_call ;
  recursive subroutine unary(self, tokens, pos, expr)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_expr), allocatable, intent(out) :: expr

    class(lox_expr), allocatable :: right
    integer :: last

    if (match(tokens, pos, [BANG, MINUS])) then
      last = pos
      call next(tokens, pos)
      call unary(self, tokens, pos, right)
      if (self%panic) return
      expr = lox_unary(self%uid(), tokens(last), right)
    else
      call fun_call(self, tokens, pos, expr)
      if (self%panic) return
    end if
  end subroutine unary

  !> Parse an expression with the following grammar rule
  !>
  !>     fun_call  -> primary ( "(" arguments? ")" )* ;
  !>     arguments -> expression ( "," expression )* ;
  recursive subroutine fun_call(self, tokens, pos, expr)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_expr), allocatable, intent(out) :: expr

    class(lox_call), allocatable :: fcall
    class(lox_expr), allocatable :: local, arg
    integer :: last

    call primary(self, tokens, pos, local)
    if (self%panic) return

    do while(match(tokens, pos, [LEFT_PAREN]))
      last = pos
      call next(tokens, pos)

      allocate(fcall)
      fcall%paren = tokens(last)
      call move_alloc(local, fcall%callee)

      if (.not.match(tokens, pos, [RIGHT_PAREN])) then
        call expression(self, tokens, pos, arg)
        if (self%panic) return
        call fcall%add(arg)

        do while(match(tokens, pos, [COMMA]))
          call next(tokens, pos)

          call expression(self, tokens, pos, arg)
          if (self%panic) return
          call fcall%add(arg)
        end do
      end if

      call consume(self, tokens, pos, RIGHT_PAREN, "Expect ')' after arguments")
      if (self%panic) return

      call move_alloc(fcall, local)
    end do

    call move_alloc(local, expr)
  end subroutine fun_call

  !> Parse an expression with the following grammar rule
  !>
  !>    primary -> NUMBER | STRING
  !>               | "true" | "false" | "nil"
  !>               | "(" expression ")"
  !>               | IDENTIFIER ;
  recursive subroutine primary(self, tokens, pos, expr)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    class(lox_expr), allocatable, intent(out) :: expr

    class(lox_expr), allocatable :: group
    integer :: last

    if (match(tokens, pos, [IDENTIFIER])) then
      last = pos
      call next(tokens, pos)
      expr = lox_literal(self%uid(), tokens(last))
      return
    end if

    if (match(tokens, pos, [NUMBER, STRING])) then
      last = pos
      call next(tokens, pos)
      expr = lox_literal(self%uid(), tokens(last))
      return
    end if

    if (match(tokens, pos, [LEFT_PAREN])) then
      call next(tokens, pos)
      call expression(self, tokens, pos, group)
      if (self%panic) return
      call consume(self, tokens, pos, RIGHT_PAREN, "expect ')' after expression.")
      if (self%panic) return
      expr = lox_grouping(self%uid(), group)
      return
    end if

    if (match(tokens, pos, [COMMENT, SPACE, NEWLINE])) return
    call error(self, tokens(pos), "Expect expression.", .false.)
  end subroutine primary

  subroutine next(tokens, pos)
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos

    pos = pos + 1
    do while(pos < size(tokens))
      if (match(tokens, pos, [COMMENT, SPACE, NEWLINE])) then
        pos = pos + 1
        cycle
      end if
      exit
    end do
  end subroutine next

  subroutine consume(self, tokens, pos, ttype, message)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(inout) :: pos
    integer, intent(in) :: ttype
    character(len=*), intent(in) :: message

    if (match(tokens, pos, [ttype])) then
      call next(tokens, pos)
    else
      call error(self, tokens(min(pos, size(tokens))), message, pos >= size(tokens))
    end if
  end subroutine consume

  subroutine error(self, token, message, eof)
    class(lox_parser), intent(inout) :: self
    type(lox_token), intent(in) :: token
    character(len=*), intent(in) :: message
    logical, intent(in) :: eof

    integer :: line, first, last

    self%panic = .true.
    if (.not.allocated(self%diag)) allocate(self%diag(0))

    call line_descriptor(token, line, first, last)
    self%diag = [self%diag, &
      & lox_diagnostic(merge(level_eof_error, level_syntax_error, eof), &
      & message, label=[label_type(level_syntax_error, "here", line, first, last, .true.)])]
  end subroutine error

  pure function match(tokens, pos, ttype)
    type(lox_token), intent(in) :: tokens(:)
    integer, intent(in) :: pos
    integer, intent(in) :: ttype(:)
    logical :: match

    match = .false.
    if (pos > 0 .and. pos <= size(tokens)) then
      match = any(tokens(pos)%ttype == ttype)
    end if
  end function match

end module flox_parser
