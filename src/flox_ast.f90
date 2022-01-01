! This file is part of flox.
! SPDX-Identifier: Apache-2.0

!> Main module of the Fortran lox interpreter
module flox_ast
  use flox_constants, only : i8
  use flox_scanner, only : lox_token
  implicit none
  private

  public :: lox_ast
  public :: lox_stmt, lox_block, lox_expr_stmt, lox_print, lox_var, lox_if, lox_while, &
    & lox_fun, lox_return
  public :: lox_expr, lox_assign, lox_logical, lox_binary, lox_grouping, lox_literal, &
    & lox_unary, lox_call
  public :: lox_visitor


  !> Abstract base class for visitor
  type, abstract :: lox_visitor
  contains
    generic :: visit => visit_ast, &
      visit_assign, visit_logical, visit_binary, visit_grouping, visit_literal, visit_unary, &
      visit_call, &
      visit_block, visit_expr_stmt, visit_print, visit_var, visit_if, visit_while, &
      visit_fun, visit_return
    procedure(visit_ast), deferred :: visit_ast
    procedure(visit_assign), deferred :: visit_assign
    procedure(visit_logical), deferred :: visit_logical
    procedure(visit_binary), deferred :: visit_binary
    procedure(visit_grouping), deferred :: visit_grouping
    procedure(visit_literal), deferred :: visit_literal
    procedure(visit_unary), deferred :: visit_unary
    procedure(visit_call), deferred :: visit_call
    procedure(visit_block), deferred :: visit_block
    procedure(visit_expr_stmt), deferred :: visit_expr_stmt
    procedure(visit_print), deferred :: visit_print
    procedure(visit_var), deferred :: visit_var
    procedure(visit_if), deferred :: visit_if
    procedure(visit_while), deferred :: visit_while
    procedure(visit_fun), deferred :: visit_fun
    procedure(visit_return), deferred :: visit_return
  end type lox_visitor

  !> Abstract base class for statements
  type, abstract :: lox_stmt
    integer(i8) :: id
  contains
    !> Accept a visitor
    procedure(accept_stmt), deferred :: accept
    !> Return location of the operator
    procedure :: backtrace => backtrace_stmt
  end type lox_stmt

  !> Abstract base class for expressions
  type, abstract :: lox_expr
    integer(i8) :: id
  contains
    !> Accept a visitor
    procedure(accept_expr), deferred :: accept
    !> Return location of the operator
    procedure :: backtrace => backtrace_expr
  end type lox_expr

  !> Container to hold a statement
  type :: lox_ast_node
    !> Actual statement
    class(lox_stmt), allocatable :: stmt
  end type lox_ast_node

  !> Representation of the abstract syntax tree root
  type :: lox_ast
    !> Number of statements in the tree root
    integer :: nstmt = 0
    !> Statements in this block
    type(lox_ast_node), allocatable :: stmts(:)
  contains
    !> Accept a visitor
    procedure :: accept => accept_ast
    !> Add a statement to the block
    procedure :: add => add_ast
  end type lox_ast

  !> Representation of the block
  type, extends(lox_stmt) :: lox_block
    !> Number of statements in this block
    integer :: nstmt = 0
    !> Statements in this block
    type(lox_ast_node), allocatable :: stmts(:)
  contains
    !> Accept a visitor
    procedure :: accept => accept_block
    !> Add a statement to the block
    procedure :: add => add_block
  end type lox_block

  !> Representation of an expression statement
  type, extends(lox_stmt) :: lox_expr_stmt
    !> Expression to evaluate
    class(lox_expr), allocatable :: expression
  contains
    !> Accept a visitor
    procedure :: accept => accept_expression
  end type lox_expr_stmt

  !> Representation of a print statement
  type, extends(lox_stmt) :: lox_print
    !> Expression to evaluate
    class(lox_expr), allocatable :: expression
  contains
    !> Accept a visitor
    procedure :: accept => accept_print
  end type lox_print

  !> Representation of a variable declaration
  type, extends(lox_stmt) :: lox_var
    !> Name of the variable
    type(lox_token), allocatable :: name
    !> Expression to evaluate
    class(lox_expr), allocatable :: expression
  contains
    !> Accept a visitor
    procedure :: accept => accept_var
    !> Return location of the operator
    procedure :: backtrace => backtrace_var
  end type lox_var

  !> Representation of a conditional statement
  type, extends(lox_stmt) :: lox_if
    !> Expression to evaluate
    class(lox_expr), allocatable :: condition
    !> Block to execute if condition is true
    class(lox_stmt), allocatable :: then_branch
    !> Block to execute if condition is false
    class(lox_stmt), allocatable :: else_branch
  contains
    !> Accept a visitor
    procedure :: accept => accept_if
  end type lox_if

  !> Representation of a while loop
  type, extends(lox_stmt) :: lox_while
    !> Expression to evaluate
    class(lox_expr), allocatable :: condition
    !> Block to execute while condition is true
    class(lox_stmt), allocatable :: body
  contains
    !> Accept a visitor
    procedure :: accept => accept_while
  end type lox_while

  !> Representation of a function declaration
  type, extends(lox_stmt) :: lox_fun
    !> Function name
    type(lox_token), allocatable :: name
    !> Function parameters
    type(lox_token), allocatable :: params(:)
    !> Function body
    class(lox_block), allocatable :: body
  contains
    !> Accept a visitor
    procedure :: accept => accept_fun
  end type lox_fun

  !> Representation of a return statement
  type, extends(lox_stmt) :: lox_return
    !> Token representing the return value
    type(lox_token), allocatable :: keyword
    !> Expression to evaluate
    class(lox_expr), allocatable :: expression
  contains
    !> Accept a visitor
    procedure :: accept => accept_return
  end type lox_return

  !> Representation of an assignment expression
  type, extends(lox_expr) :: lox_assign
    !> Name of the variable
    type(lox_token), allocatable :: name
    !> Expression to evaluate
    class(lox_expr), allocatable :: value
  contains
    !> Accept a visitor
    procedure :: accept => accept_assign
    !> Return location of the operator
    procedure :: backtrace => backtrace_assign
  end type lox_assign

  !> Representation of a logical expression
  type, extends(lox_expr) :: lox_logical
    !> Expression of left operand
    class(lox_expr), allocatable :: left
    !> Token representing the operator
    type(lox_token), allocatable :: operator
    !> Expression of right operand
    class(lox_expr), allocatable :: right
  contains
    !> Accept a visitor
    procedure :: accept => accept_logical
    !> Return location of the operator
    procedure :: backtrace => backtrace_logical
  end type lox_logical

  !> Representation of a binary expression
  type, extends(lox_expr) :: lox_binary
    !> Expression of left operand
    class(lox_expr), allocatable :: left
    !> Token representing the operator
    type(lox_token), allocatable :: operator
    !> Expression of right operand
    class(lox_expr), allocatable :: right
  contains
    !> Accept a visitor
    procedure :: accept => accept_binary
    !> Return location of the operator
    procedure :: backtrace => backtrace_binary
  end type lox_binary

  !> Representation of a grouping expression
  type, extends(lox_expr) :: lox_grouping
    !> Expression in the grouping
    class(lox_expr), allocatable :: expression
  contains
    !> Accept a visitor
    procedure :: accept => accept_grouping
  end type lox_grouping

  !> Representation of a literal expression
  type, extends(lox_expr) :: lox_literal
    !> Value of the literal
    type(lox_token), allocatable :: object
  contains
    !> Accept a visitor
    procedure :: accept => accept_literal
    !> Return location of the literal
    procedure :: backtrace => backtrace_literal
  end type lox_literal

  !> Representation of a unary expression
  type, extends(lox_expr) :: lox_unary
    !> Token representing the operator
    type(lox_token), allocatable :: operator
    !> Expression of the operand
    class(lox_expr), allocatable :: right
  contains
    !> Accept a visitor
    procedure :: accept => accept_unary
    !> Return location of the operator
    procedure :: backtrace => backtrace_unary
  end type lox_unary

  !> Representation of an argument
  type :: lox_arg_expr
    !> Arguments to the function
    class(lox_expr), allocatable :: expr
  end type lox_arg_expr

  !> Representation of a function call
  type, extends(lox_expr) :: lox_call
    !> Name of the function
    class(lox_expr), allocatable :: callee
    !> Token for the opening parenthesis
    type(lox_token), allocatable :: paren
    !> Number of arguments
    integer :: narg = 0
    !> Arguments to the function
    type(lox_arg_expr), allocatable :: args(:)
  contains
    !> Accept a visitor
    procedure :: accept => accept_call
    !> Return location of the operator
    procedure :: backtrace => backtrace_call
    !> Add an argument to the call
    procedure :: add => add_call
  end type lox_call

  abstract interface
    !> Accept a visitor to an abstract statement
    recursive subroutine accept_stmt(self, visitor)
      import :: lox_stmt, lox_visitor
      class(lox_stmt), intent(in) :: self
      class(lox_visitor), intent(inout) :: visitor
    end subroutine accept_stmt

    !> Accept a visitor to an abstract expression
    recursive subroutine accept_expr(self, visitor)
      import :: lox_expr, lox_visitor
      class(lox_expr), intent(in) :: self
      class(lox_visitor), intent(inout) :: visitor
    end subroutine accept_expr

    !> Visit abstract syntax tree root
    recursive subroutine visit_ast(self, ast)
      import :: lox_ast, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_ast), intent(in) :: ast
    end subroutine visit_ast

    !> Visit an assignment expression
    recursive subroutine visit_assign(self, expr)
      import :: lox_assign, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_assign), intent(in) :: expr
    end subroutine visit_assign

    !> Visit a logical expression
    recursive subroutine visit_logical(self, expr)
      import :: lox_logical, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_logical), intent(in) :: expr
    end subroutine visit_logical

    !> Visit a binary expression
    recursive subroutine visit_binary(self, expr)
      import :: lox_binary, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_binary), intent(in) :: expr
    end subroutine visit_binary

    !> Visit a grouping expression
    recursive subroutine visit_grouping(self, expr)
      import :: lox_grouping, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_grouping), intent(in) :: expr
    end subroutine visit_grouping

    !> Visit a literal expression
    recursive subroutine visit_literal(self, expr)
      import :: lox_literal, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_literal), intent(in) :: expr
    end subroutine visit_literal

    !> Visit a unary expression
    recursive subroutine visit_unary(self, expr)
      import :: lox_unary, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_unary), intent(in) :: expr
    end subroutine visit_unary

    !> Visit a function call
    recursive subroutine visit_call(self, expr)
      import :: lox_call, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_call), intent(in) :: expr
    end subroutine visit_call

    !> Visit an expression statement
    recursive subroutine visit_block(self, stmt)
      import :: lox_block, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_block), intent(in) :: stmt
    end subroutine visit_block

    !> Visit an expression statement
    recursive subroutine visit_expr_stmt(self, stmt)
      import :: lox_expr_stmt, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_expr_stmt), intent(in) :: stmt
    end subroutine visit_expr_stmt

    !> Visit a print statement
    recursive subroutine visit_print(self, stmt)
      import :: lox_print, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_print), intent(in) :: stmt
    end subroutine visit_print

    !> Visit a variable declaration
    recursive subroutine visit_var(self, stmt)
      import :: lox_var, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_var), intent(in) :: stmt
    end subroutine visit_var

    !> Visit a conditional statement
    recursive subroutine visit_if(self, stmt)
      import :: lox_if, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_if), intent(in) :: stmt
    end subroutine visit_if

    !> Visit a conditional statement
    recursive subroutine visit_while(self, stmt)
      import :: lox_while, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_while), intent(in) :: stmt
    end subroutine visit_while

    !> Visit a function declaration
    recursive subroutine visit_fun(self, stmt)
      import :: lox_fun, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_fun), intent(in) :: stmt
    end subroutine visit_fun

    !> Visit a return statement
    recursive subroutine visit_return(self, stmt)
      import :: lox_return, lox_visitor
      class(lox_visitor), intent(inout) :: self
      class(lox_return), intent(in) :: stmt
    end subroutine visit_return
  end interface

  interface resize
    module procedure :: resize_ast_node
    module procedure :: resize_arg_expr
  end interface resize

  interface move
    module procedure :: move_ast_node
    module procedure :: move_arg_expr
  end interface move

contains

  subroutine accept_ast(self, visitor)
    class(lox_ast), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_ast

  subroutine accept_assign(self, visitor)
    class(lox_assign), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_assign

  subroutine accept_logical(self, visitor)
    class(lox_logical), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_logical

  subroutine accept_binary(self, visitor)
    class(lox_binary), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_binary

  subroutine accept_grouping(self, visitor)
    class(lox_grouping), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_grouping

  subroutine accept_literal(self, visitor)
    class(lox_literal), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_literal

  subroutine accept_unary(self, visitor)
    class(lox_unary), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_unary

  subroutine accept_call(self, visitor)
    class(lox_call), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_call

  subroutine accept_block(self, visitor)
    class(lox_block), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_block

  subroutine accept_expression(self, visitor)
    class(lox_expr_stmt), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_expression

  subroutine accept_print(self, visitor)
    class(lox_print), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_print

  subroutine accept_var(self, visitor)
    class(lox_var), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_var

  subroutine accept_if(self, visitor)
    class(lox_if), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_if

  subroutine accept_while(self, visitor)
    class(lox_while), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_while

  subroutine accept_fun(self, visitor)
    class(lox_fun), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_fun

  subroutine accept_return(self, visitor)
    class(lox_return), intent(in) :: self
    class(lox_visitor), intent(inout) :: visitor
    call visitor%visit(self)
  end subroutine accept_return

  pure function backtrace_stmt(self) result(token)
    class(lox_stmt), intent(in) :: self
    type(lox_token), allocatable :: token
  end function backtrace_stmt

  pure function backtrace_expr(self) result(token)
    class(lox_expr), intent(in) :: self
    type(lox_token), allocatable :: token
  end function backtrace_expr

  pure function backtrace_var(self) result(token)
    class(lox_var), intent(in) :: self
    type(lox_token), allocatable :: token

    token = self%name
  end function backtrace_var

  pure function backtrace_assign(self) result(token)
    class(lox_assign), intent(in) :: self
    type(lox_token), allocatable :: token

    token = self%name
  end function backtrace_assign

  pure function backtrace_logical(self) result(token)
    class(lox_logical), intent(in) :: self
    type(lox_token), allocatable :: token

    token = self%operator
  end function backtrace_logical

  pure function backtrace_binary(self) result(token)
    class(lox_binary), intent(in) :: self
    type(lox_token), allocatable :: token

    token = self%operator
  end function backtrace_binary

  pure function backtrace_literal(self) result(token)
    class(lox_literal), intent(in) :: self
    type(lox_token), allocatable :: token

    token = self%object
  end function backtrace_literal

  pure function backtrace_unary(self) result(token)
    class(lox_unary), intent(in) :: self
    type(lox_token), allocatable :: token

    token = self%operator
  end function backtrace_unary

  pure function backtrace_call(self) result(token)
    class(lox_call), intent(in) :: self
    type(lox_token), allocatable :: token

    token = self%paren
  end function backtrace_call

  subroutine add_ast(self, stmt)
    class(lox_ast), intent(inout) :: self
    class(lox_stmt), allocatable, intent(inout) :: stmt

    if (.not.allocated(self%stmts)) call resize(self%stmts)
    if (self%nstmt >= size(self%stmts)) call resize(self%stmts)

    self%nstmt = self%nstmt + 1
    associate(new => self%stmts(self%nstmt))
      call move_alloc(stmt, new%stmt)
    end associate
  end subroutine add_ast

  subroutine add_block(self, stmt)
    class(lox_block), intent(inout) :: self
    class(lox_stmt), allocatable, intent(inout) :: stmt

    if (.not.allocated(self%stmts)) call resize(self%stmts)
    if (self%nstmt >= size(self%stmts)) call resize(self%stmts)

    self%nstmt = self%nstmt + 1
    associate(new => self%stmts(self%nstmt))
      call move_alloc(stmt, new%stmt)
    end associate
  end subroutine add_block

  subroutine add_call(self, expr)
    class(lox_call), intent(inout) :: self
    class(lox_expr), allocatable, intent(inout) :: expr

    if (.not.allocated(self%args)) call resize(self%args)
    if (self%narg >= size(self%args)) call resize(self%args)

    self%narg = self%narg + 1
    associate(new => self%args(self%narg))
      call move_alloc(expr, new%expr)
    end associate
  end subroutine add_call

  !> Reallocate list of AST nodes
  subroutine resize_ast_node(list, n)
    !> Instance of the array to be resized
    type(lox_ast_node), allocatable, intent(inout) :: list(:)
    !> Dimension of the final array size
    integer, intent(in), optional :: n

    type(lox_ast_node), allocatable :: tmp(:)
    integer :: this_size, new_size
    integer, parameter :: initial_size = 32

    if (allocated(list)) then
      this_size = size(list, 1)
      call move_alloc(list, tmp)
    else
      this_size = initial_size
    end if

    if (present(n)) then
      new_size = n
    else
      new_size = this_size + this_size/2 + 1
    end if

    allocate(list(new_size))

    if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(list, 1))
      call move(tmp(:this_size), list(:this_size))
      deallocate(tmp)
    end if
  end subroutine resize_ast_node

  !> Move allocation of an AST node
  elemental subroutine move_ast_node(from, to)
    !> Source node to move allocation from
    type(lox_ast_node), intent(inout) :: from
    !> Target node to move allocation to
    type(lox_ast_node), intent(out) :: to

    call move_alloc(from%stmt, to%stmt)
  end subroutine move_ast_node

  !> Reallocate list of argument expressions
  subroutine resize_arg_expr(list, n)
    !> Instance of the array to be resized
    type(lox_arg_expr), allocatable, intent(inout) :: list(:)
    !> Dimension of the final array size
    integer, intent(in), optional :: n

    type(lox_arg_expr), allocatable :: tmp(:)
    integer :: this_size, new_size
    integer, parameter :: initial_size = 8

    if (allocated(list)) then
      this_size = size(list, 1)
      call move_alloc(list, tmp)
    else
      this_size = initial_size
    end if

    if (present(n)) then
      new_size = n
    else
      new_size = this_size + this_size/2 + 1
    end if

    allocate(list(new_size))

    if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(list, 1))
      call move(tmp(:this_size), list(:this_size))
      deallocate(tmp)
    end if
  end subroutine resize_arg_expr

  !> Move allocation of an argument expression
  elemental subroutine move_arg_expr(from, to)
    !> Source node to move allocation from
    type(lox_arg_expr), intent(inout) :: from
    !> Target node to move allocation to
    type(lox_arg_expr), intent(out) :: to

    call move_alloc(from%expr, to%expr)
  end subroutine move_arg_expr

end module flox_ast
