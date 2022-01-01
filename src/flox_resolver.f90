! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_resolver
  use flox_ast, only : lox_visitor, lox_expr, lox_stmt, lox_ast, &
    & lox_block, lox_expr_stmt, lox_print, lox_var, lox_if, lox_while, lox_fun, lox_return, &
    & lox_assign, lox_logical, lox_binary, lox_grouping, lox_literal, lox_unary, lox_call
  use flox_diagnostic, only : lox_diagnostic, label_type, level_semantic_error
  use flox_interpreter, only : lox_interpreter
  use flox_map, only : map_type => cuckoo_hash, new_map => new_cuckoo_hash, container_type
  use flox_scanner, only : lox_token, IDENTIFIER
  implicit none
  private

  public :: lox_resolver, new_resolver

  type, extends(lox_visitor) :: lox_resolver
    type(lox_diagnostic), allocatable :: diag(:)
    integer :: nscope = 0
    type(map_type), allocatable :: scopes(:)
    type(lox_interpreter), pointer :: interpreter => null()
  contains
    procedure :: push
    procedure :: pop
    generic :: resolve => resolve_stmt, resolve_expr
    procedure :: resolve_stmt
    procedure :: resolve_expr
    procedure :: resolve_local
    procedure :: resolve_fun
    procedure :: declare
    procedure :: define

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
    procedure :: visit_fun
    procedure :: visit_return
  end type lox_resolver

contains

  subroutine new_resolver(self, interpreter)
    type(lox_resolver), intent(out) :: self
    type(lox_interpreter), target, intent(in) :: interpreter

    self%interpreter => interpreter
  end subroutine new_resolver

  subroutine declare(self, name)
    class(lox_resolver), intent(inout) :: self
    type(lox_token), intent(in) :: name

    type(container_type), pointer :: ptr

    if (self%nscope == 0) return

    associate(symbols => self%scopes(self%nscope))
      call symbols%insert(name%val, ptr)
      ptr%val = .false.
    end associate
  end subroutine declare

  subroutine define(self, name)
    class(lox_resolver), intent(inout) :: self
    type(lox_token), intent(in) :: name

    type(container_type), pointer :: ptr

    if (self%nscope == 0) return

    associate(symbols => self%scopes(self%nscope))
      call symbols%insert(name%val, ptr)
      ptr%val = .true.
    end associate
  end subroutine define

  subroutine push(self)
    class(lox_resolver), intent(inout) :: self

    if (.not.allocated(self%scopes)) call resize(self%scopes)
    if (self%nscope >= size(self%scopes)) call resize(self%scopes)

    self%nscope = self%nscope + 1
    associate(symbols => self%scopes(self%nscope))
      call new_map(symbols)
    end associate
  end subroutine push

  subroutine pop(self)
    class(lox_resolver), intent(inout) :: self

    call self%scopes(self%nscope)%clear
    self%nscope = self%nscope - 1
  end subroutine pop

  recursive subroutine resolve_stmt(self, stmt)
    class(lox_resolver), intent(inout) :: self
    class(lox_stmt), intent(in) :: stmt
    call stmt%accept(self)
  end subroutine resolve_stmt

  recursive subroutine resolve_expr(self, expr)
    class(lox_resolver), intent(inout) :: self
    class(lox_expr), intent(in) :: expr
    call expr%accept(self)
  end subroutine resolve_expr

  recursive subroutine resolve_local(self, expr, name)
    class(lox_resolver), intent(inout) :: self
    class(lox_expr), intent(in) :: expr
    type(lox_token), intent(in) :: name

    type(container_type), pointer :: ptr
    integer :: iscope

    do iscope = self%nscope, 1, -1
      associate(symbols => self%scopes(iscope))
        call symbols%get(name%val, ptr)
        if (associated(ptr)) then
          call self%interpreter%resolve(expr, self%nscope - iscope)
          return
        end if
      end associate
    end do
  end subroutine resolve_local

  recursive subroutine resolve_fun(self, fun)
    class(lox_resolver), intent(inout) :: self
    class(lox_fun), intent(in) :: fun

    integer :: ipar

    call self%push

    do ipar = 1, size(fun%params)
      call self%declare(fun%params(ipar))
      call self%define(fun%params(ipar))
    end do
    call self%resolve(fun%body)

    call self%pop
  end subroutine resolve_fun

  recursive subroutine visit_ast(self, ast)
    class(lox_resolver), intent(inout) :: self
    class(lox_ast), intent(in) :: ast

    integer :: istmt

    do istmt = 1, ast%nstmt
      associate(entry => ast%stmts(istmt)%stmt)
        call self%resolve(entry)
      end associate
    end do
  end subroutine visit_ast

  recursive subroutine visit_block(self, stmt)
    class(lox_resolver), intent(inout) :: self
    class(lox_block), intent(in) :: stmt

    integer :: istmt

    call self%push
    do istmt = 1, stmt%nstmt
      associate(entry => stmt%stmts(istmt)%stmt)
        call self%resolve(entry)
      end associate
    end do
    call self%pop
  end subroutine visit_block

  recursive subroutine visit_expr_stmt(self, stmt)
    class(lox_resolver), intent(inout) :: self
    class(lox_expr_stmt), intent(in) :: stmt

    call self%resolve(stmt%expression)
  end subroutine visit_expr_stmt

  recursive subroutine visit_print(self, stmt)
    class(lox_resolver), intent(inout) :: self
    class(lox_print), intent(in) :: stmt

    call self%resolve(stmt%expression)
  end subroutine visit_print

  recursive subroutine visit_var(self, stmt)
    class(lox_resolver), intent(inout) :: self
    class(lox_var), intent(in) :: stmt

    type(container_type), pointer :: ptr

    if (self%nscope > 0) then
      associate(symbols => self%scopes(self%nscope))
        call symbols%get(stmt%name%val, ptr)
        if (associated(ptr)) then
          select type(defined => ptr%val)
          type is(logical)
            ! Reading variable in its own initializer
            ! TODO
          end select
        end if
      end associate
    end if

    call self%declare(stmt%name)
    if (allocated(stmt%expression)) then
      call self%resolve(stmt%expression)
    end if
    call self%define(stmt%name)
  end subroutine visit_var

  recursive subroutine visit_if(self, stmt)
    class(lox_resolver), intent(inout) :: self
    class(lox_if), intent(in) :: stmt

    call self%resolve(stmt%condition)
    call self%resolve(stmt%then_branch)
    if (allocated(stmt%else_branch)) then
      call self%resolve(stmt%else_branch)
    end if
  end subroutine visit_if

  recursive subroutine visit_while(self, stmt)
    class(lox_resolver), intent(inout) :: self
    class(lox_while), intent(in) :: stmt

    call self%resolve(stmt%condition)
    call self%resolve(stmt%body)
  end subroutine visit_while

  recursive subroutine visit_fun(self, stmt)
    class(lox_resolver), intent(inout) :: self
    class(lox_fun), intent(in) :: stmt

    call self%declare(stmt%name)
    call self%define(stmt%name)

    call self%resolve_fun(stmt)
  end subroutine visit_fun

  recursive subroutine visit_return(self, stmt)
    class(lox_resolver), intent(inout) :: self
    class(lox_return), intent(in) :: stmt

    if (allocated(stmt%expression)) then
      call self%resolve(stmt%expression)
    end if
  end subroutine visit_return

  recursive subroutine visit_assign(self, expr)
    class(lox_resolver), intent(inout) :: self
    class(lox_assign), intent(in) :: expr

    call self%resolve(expr%value)
    call self%resolve_local(expr, expr%name)
  end subroutine visit_assign

  recursive subroutine visit_logical(self, expr)
    class(lox_resolver), intent(inout) :: self
    class(lox_logical), intent(in) :: expr

    call self%resolve(expr%left)
    call self%resolve(expr%right)
  end subroutine visit_logical

  recursive subroutine visit_binary(self, expr)
    class(lox_resolver), intent(inout) :: self
    class(lox_binary), intent(in) :: expr

    call self%resolve(expr%left)
    call self%resolve(expr%right)
  end subroutine visit_binary

  recursive subroutine visit_grouping(self, expr)
    class(lox_resolver), intent(inout) :: self
    class(lox_grouping), intent(in) :: expr

    call self%resolve(expr%expression)
  end subroutine visit_grouping

  recursive subroutine visit_literal(self, expr)
    class(lox_resolver), intent(inout) :: self
    class(lox_literal), intent(in) :: expr

    if (allocated(expr%object)) then
      select case(expr%object%ttype)
      case(IDENTIFIER)
        call self%resolve_local(expr, expr%object)
      end select
    end if
  end subroutine visit_literal

  recursive subroutine visit_unary(self, expr)
    class(lox_resolver), intent(inout) :: self
    class(lox_unary), intent(in) :: expr

    call self%resolve(expr%right)
  end subroutine visit_unary

  recursive subroutine visit_call(self, expr)
    class(lox_resolver), intent(inout) :: self
    class(lox_call), intent(in) :: expr

    integer :: iarg

    call self%resolve(expr%callee)

    do iarg = 1, expr%narg
      associate(arg => expr%args(iarg)%expr)
        call self%resolve(arg)
      end associate
    end do
  end subroutine visit_call

  !> Reallocate list of entities
  subroutine resize(list, n)
    !> Instance of the array to be resized
    type(map_type), allocatable, intent(inout) :: list(:)
    !> Dimension of the final array size
    integer, intent(in), optional :: n

    type(map_type), allocatable :: tmp(:)
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
      list(:this_size) = tmp(:this_size)
      deallocate(tmp)
    end if
  end subroutine resize

end module flox_resolver
