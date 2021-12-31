! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_ast_printer
  use flox_ast, only : lox_visitor, lox_expr, lox_ast, &
    & lox_block, lox_expr_stmt, lox_print, lox_var, lox_if, lox_while, &
    & lox_assign, lox_logical, lox_binary, lox_grouping, lox_literal, lox_unary, lox_call
  implicit none
  private

  public :: lox_ast_printer

  type, extends(lox_visitor) :: lox_ast_printer
    character(len=:), allocatable :: string
  contains
    procedure :: visit_ast
    procedure :: visit_assign
    procedure :: visit_logical
    procedure :: visit_binary
    procedure :: visit_grouping
    procedure :: visit_literal
    procedure :: visit_unary
    procedure :: visit_call
    procedure :: visit_block
    procedure :: visit_expr_stmt
    procedure :: visit_print
    procedure :: visit_var
    procedure :: visit_if
    procedure :: visit_while
  end type lox_ast_printer

contains

  recursive subroutine visit_assign(self, expr)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_assign), intent(in) :: expr

    call parenthesize(self, "= "//expr%name%val, expr%value)
  end subroutine visit_assign

  recursive subroutine visit_logical(self, expr)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_logical), intent(in) :: expr

    call parenthesize(self, expr%operator%val, expr%left, expr%right)
  end subroutine visit_logical

  recursive subroutine visit_binary(self, expr)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_binary), intent(in) :: expr

    call parenthesize(self, expr%operator%val, expr%left, expr%right)
  end subroutine visit_binary

  recursive subroutine visit_grouping(self, expr)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_grouping), intent(in) :: expr

    call parenthesize(self, "group", expr%expression)
  end subroutine visit_grouping

  recursive subroutine visit_literal(self, expr)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_literal), intent(in) :: expr

    self%string = self%string // expr%object%val
  end subroutine visit_literal

  recursive subroutine visit_unary(self, expr)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_unary), intent(in) :: expr

    call parenthesize(self, expr%operator%val, expr%right)
  end subroutine visit_unary

  recursive subroutine visit_call(self, expr)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_call), intent(in) :: expr

    integer :: iarg

    self%string = self%string // "(call "
    call expr%callee%accept(self)
    do iarg = 1, expr%narg
      self%string = self%string // " "
      associate(arg => expr%args(iarg)%expr)
        call arg%accept(self)
      end associate
    end do
    self%string = self%string // ")"
  end subroutine visit_call

  recursive subroutine visit_ast(self, ast)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_ast), intent(in) :: ast

    integer :: istmt

    self%string = "(root"
    do istmt = 1, ast%nstmt
      self%string = self%string // " "
      associate(entry => ast%stmts(istmt)%stmt)
        call entry%accept(self)
      end associate
    end do
    self%string = self%string // ")"
  end subroutine visit_ast

  recursive subroutine visit_block(self, stmt)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_block), intent(in) :: stmt

    integer :: istmt

    self%string = self%string // "(block"
    do istmt = 1, stmt%nstmt
      self%string = self%string // " "
      associate(entry => stmt%stmts(istmt)%stmt)
        call entry%accept(self)
      end associate
    end do
    self%string = self%string // ")"
  end subroutine visit_block

  recursive subroutine visit_expr_stmt(self, stmt)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_expr_stmt), intent(in) :: stmt

    call parenthesize(self, "stmt", stmt%expression)
  end subroutine visit_expr_stmt

  recursive subroutine visit_print(self, stmt)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_print), intent(in) :: stmt

    call parenthesize(self, "print", stmt%expression)
  end subroutine visit_print

  recursive subroutine visit_var(self, stmt)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_var), intent(in) :: stmt

    if (allocated(stmt%expression)) then
      call parenthesize(self, "var "//stmt%name%val, stmt%expression)
    else
      self%string = self%string // "(var " // stmt%name%val // ")"
    end if
  end subroutine visit_var

  recursive subroutine visit_if(self, stmt)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_if), intent(in) :: stmt

    self%string = self%string // "(if "
    call parenthesize(self, "condition", stmt%condition)
    self%string = self%string // " "
    call stmt%then_branch%accept(self)
    if (allocated(stmt%else_branch)) then
      self%string = self%string // " "
      call stmt%else_branch%accept(self)
    end if
    self%string = self%string // ")"
  end subroutine visit_if

  recursive subroutine visit_while(self, stmt)
    class(lox_ast_printer), intent(inout) :: self
    class(lox_while), intent(in) :: stmt

    self%string = self%string // "(while "
    call parenthesize(self, "condition", stmt%condition)
    self%string = self%string // " "
    call stmt%body%accept(self)
    self%string = self%string // ")"
  end subroutine visit_while

  recursive subroutine parenthesize(self, name, expr1, expr2)
    class(lox_ast_printer), intent(inout) :: self
    character(len=*), intent(in) :: name
    class(lox_expr), intent(in) :: expr1
    class(lox_expr), intent(in), optional :: expr2

    self%string = self%string // "(" // name // " "
    call expr1%accept(self)
    if (present(expr2)) then
      self%string = self%string // " "
      call expr2%accept(self)
    end if
    self%string = self%string // ")"
  end subroutine parenthesize

end module flox_ast_printer
