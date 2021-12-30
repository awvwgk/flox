! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_interpreter
  use, intrinsic :: iso_fortran_env, only : lox_real => real64, output_unit
  use flox_ast, only : lox_visitor, lox_expr, lox_ast, &
    & lox_block, lox_expr_stmt, lox_print, lox_var, &
    & lox_assign, lox_binary, lox_grouping, lox_literal, lox_unary
  use flox_diagnostic, only : lox_diagnostic, label_type, level_runtime_error
  use flox_environment, only : lox_environment, lox_object, lox_error
  use flox_scanner, only : lox_token, line_descriptor, MINUS, BANG, STAR, SLASH, PLUS, &
    & GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, EQUAL_EQUAL, BANG_EQUAL, &
    & IDENTIFIER, STRING, NUMBER
  implicit none
  private

  public :: lox_interpreter, lox_object, repr

  type, extends(lox_object) :: lox_boolean
    logical :: value
  end type lox_boolean

  type, extends(lox_object) :: lox_number
    real(lox_real) :: value
  end type lox_number

  type, extends(lox_object) :: lox_string
    character(len=:), allocatable :: value
  end type lox_string

  interface lox_string
    module procedure :: new_string
  end interface lox_string

  type, extends(lox_visitor) :: lox_interpreter
    type(lox_diagnostic), allocatable :: diag(:)
    class(lox_object), allocatable :: local
    type(lox_environment) :: env
  contains
    procedure :: evaluate
    procedure :: visit_ast
    procedure :: visit_assign
    procedure :: visit_binary
    procedure :: visit_grouping
    procedure :: visit_literal
    procedure :: visit_unary
    procedure :: visit_block
    procedure :: visit_expr_stmt
    procedure :: visit_print
    procedure :: visit_var
  end type lox_interpreter

  interface to_string
    module procedure :: logical_to_string
    module procedure :: real_to_string
  end interface to_string

  abstract interface
    pure function compare_interface(left, right) result(res)
      import :: lox_number, lox_boolean
      type(lox_number), intent(in) :: left, right
      type(lox_boolean) :: res
    end function compare_interface
  end interface

contains

  recursive subroutine visit_ast(self, ast)
    class(lox_interpreter), intent(inout) :: self
    class(lox_ast), intent(in) :: ast

    integer :: istmt

    ! Initialize the global scope
    if (self%env%current == 0) call self%env%push

    do istmt = 1, ast%nstmt
      associate(entry => ast%stmts(istmt)%stmt)
        call entry%accept(self)
      end associate
    end do
  end subroutine visit_ast

  recursive subroutine visit_assign(self, expr)
    class(lox_interpreter), intent(inout) :: self
    class(lox_assign), intent(in) :: expr

    call self%evaluate(expr%value)
    call handle_error(self, expr)

    call self%env%assign(expr%name, self%local)
    call handle_error(self, expr)
  end subroutine visit_assign

  recursive subroutine visit_binary(self, expr)
    class(lox_interpreter), intent(inout) :: self
    class(lox_binary), intent(in) :: expr

    class(lox_object), allocatable :: left, right

    call self%evaluate(expr%left)
    call move_alloc(self%local, left)

    call self%evaluate(expr%right)
    call move_alloc(self%local, right)

    select case(expr%operator%ttype)
    case(MINUS)
      self%local = op_subtract(left, right)
    case(SLASH)
      self%local = op_divide(left, right)
    case(STAR)
      self%local = op_multiply(left, right)
    case(PLUS)
      self%local = op_add(left, right)
    case(GREATER)
      self%local = op_gt(left, right)
    case(GREATER_EQUAL)
      self%local = op_ge(left, right)
    case(LESS)
      self%local = op_lt(left, right)
    case(LESS_EQUAL)
      self%local = op_le(left, right)
    case(EQUAL_EQUAL)
      self%local = op_equal(left, right)
    case(BANG_EQUAL)
      self%local = op_not(op_equal(left, right))
    end select

    call handle_error(self, expr)
  end subroutine visit_binary

  recursive subroutine visit_grouping(self, expr)
    class(lox_interpreter), intent(inout) :: self
    class(lox_grouping), intent(in) :: expr

    call self%evaluate(expr%expression)

    call handle_error(self, expr)
  end subroutine visit_grouping

  recursive subroutine visit_literal(self, expr)
    class(lox_interpreter), intent(inout) :: self
    class(lox_literal), intent(in) :: expr

    if (allocated(self%local)) deallocate(self%local)

    if (allocated(expr%object)) then
      select case(expr%object%ttype)
      case(IDENTIFIER)
        select case(expr%object%val)
        case("nil")
        case("true")
          self%local = lox_boolean(.true.)
        case("false")
          self%local = lox_boolean(.false.)
        case default
          call self%env%get(expr%object, self%local)
        end select

      case(NUMBER)
        block
          real(lox_real) :: val
          integer :: stat
          read(expr%object%val, *, iostat=stat) val
          self%local = lox_number(val)
        end block

      case(STRING)
        block
          integer :: length
          length = len(expr%object%val) - 1
          self%local = lox_string(expr%object%val(2:length))
        end block

      case default
        self%local = lox_error("Not implemented")

      end select
    end if

    call handle_error(self, expr)
  end subroutine visit_literal

  recursive subroutine visit_unary(self, expr)
    class(lox_interpreter), intent(inout) :: self
    class(lox_unary), intent(in) :: expr

    class(lox_object), allocatable :: local

    call self%evaluate(expr%right)
    call move_alloc(self%local, local)

    select case(expr%operator%ttype)
    case(MINUS)
      self%local = op_negate(local)

    case(BANG)
      self%local = op_not(local)

    end select

    call handle_error(self, expr)
  end subroutine visit_unary

  recursive subroutine visit_block(self, stmt)
    class(lox_interpreter), intent(inout) :: self
    class(lox_block), intent(in) :: stmt

    integer :: istmt

    call self%env%push
    do istmt = 1, stmt%nstmt
      associate(entry => stmt%stmts(istmt)%stmt)
        call entry%accept(self)
      end associate
    end do
    call self%env%pop
  end subroutine visit_block

  recursive subroutine visit_expr_stmt(self, stmt)
    class(lox_interpreter), intent(inout) :: self
    class(lox_expr_stmt), intent(in) :: stmt

    call self%evaluate(stmt%expression)
  end subroutine visit_expr_stmt

  recursive subroutine visit_print(self, stmt)
    class(lox_interpreter), intent(inout) :: self
    class(lox_print), intent(in) :: stmt

    class(lox_object), allocatable :: object

    call self%evaluate(stmt%expression)
    call move_alloc(self%local, object)
    write(output_unit, '(a)') stringify(object)
  end subroutine visit_print

  recursive subroutine visit_var(self, stmt)
    class(lox_interpreter), intent(inout) :: self
    class(lox_var), intent(in) :: stmt

    class(lox_object), allocatable :: object

    if (allocated(stmt%expression)) then
      call self%evaluate(stmt%expression)
      call move_alloc(self%local, object)
    end if
    call self%env%define(stmt%name, object)
  end subroutine visit_var


  subroutine handle_error(self, expr)
    class(lox_interpreter), intent(inout) :: self
    class(lox_expr), intent(in) :: expr

    type(lox_token), allocatable :: token
    integer :: line, first, last

    if (.not.allocated(self%local)) return

    select type(error => self%local)
    type is(lox_error)
      if (.not.allocated(self%diag)) allocate(self%diag(0))
      token = expr%backtrace()

      if (allocated(token)) then
        call line_descriptor(token, line, first, last)
        self%diag = [self%diag, &
          & lox_diagnostic(level_runtime_error, error%message, &
          & label=[label_type(level_runtime_error, "here", line, first, last, .true.)])]
      else
        self%diag = [self%diag, lox_diagnostic(level_runtime_error, error%message)]
      end if
      deallocate(self%local)
    end select
  end subroutine handle_error

  pure function cast_boolean(object) result(res)
    class(lox_object), intent(in), optional :: object
    class(lox_object), allocatable :: res

    if (present(object)) then
      select type(object)
      type is(lox_boolean)
        res = lox_boolean(object%value)
      class default
        res = lox_boolean(.true.)
      end select
    else
      res = lox_boolean(.false.)
    end if
  end function cast_boolean

  pure function op_negate(object) result(res)
    class(lox_object), intent(in), optional :: object
    class(lox_object), allocatable :: res

    if (present(object)) then
      select type(object)
      type is(lox_number)
        res = lox_number(-object%value)
      class default
        res = lox_error("Cannot negate '"//repr(object)//"'")
      end select
    else
      res = lox_error("Cannot negate nil")
    end if
  end function op_negate

  pure function op_not(object) result(res)
    class(lox_object), intent(in), optional :: object
    class(lox_object), allocatable :: res

    if (present(object)) then
      select type(object)
      type is(lox_boolean)
        res = lox_boolean(.not.object%value)
      class default
        res = lox_boolean(.false.)
      end select
    else
      res = lox_boolean(.true.)
    end if
  end function op_not

  pure function op_subtract(left, right) result(res)
    class(lox_object), intent(in), optional :: left, right
    class(lox_object), allocatable :: res

    if (.not.(present(left) .or. present(right))) then
      res = lox_error("Cannot subtract '"//repr(left)//"' and '"//repr(right)//"'")
      return
    end if

    select type(left)
    type is(lox_number)
      select type(right)
      type is(lox_number)
        res = lox_number(left%value - right%value)
      class default
        res = lox_error("Cannot subtract '"//repr(left)//"' and '"//repr(right)//"'")
      end select
    class default
      res = lox_error("Cannot subtract '"//repr(left)//"' and '"//repr(right)//"'")
    end select
  end function op_subtract

  pure function op_multiply(left, right) result(res)
    class(lox_object), intent(in), optional :: left, right
    class(lox_object), allocatable :: res

    if (.not.(present(left) .or. present(right))) then
      res = lox_error("Cannot multiply '"//repr(left)//"' and '"//repr(right)//"'")
      return
    end if

    select type(left)
    type is(lox_number)
      select type(right)
      type is(lox_number)
        res = lox_number(left%value * right%value)
      class default
        res = lox_error("Cannot multiply '"//repr(left)//"' and '"//repr(right)//"'")
      end select
    class default
      res = lox_error("Cannot multiply '"//repr(left)//"' and '"//repr(right)//"'")
    end select
  end function op_multiply

  pure function op_divide(left, right) result(res)
    class(lox_object), intent(in), optional :: left, right
    class(lox_object), allocatable :: res

    if (.not.(present(left) .or. present(right))) then
      res = lox_error("Cannot divide '"//repr(left)//"' and '"//repr(right)//"'")
      return
    end if

    select type(left)
    type is(lox_number)
      select type(right)
      type is(lox_number)
        res = lox_number(left%value / right%value)
      class default
        res = lox_error("Cannot divide '"//repr(left)//"' and '"//repr(right)//"'")
      end select
    class default
      res = lox_error("Cannot divide '"//repr(left)//"' and '"//repr(right)//"'")
    end select
  end function op_divide

  pure function op_add(left, right) result(res)
    class(lox_object), intent(in), optional :: left, right
    class(lox_object), allocatable :: res

    if (.not.(present(left) .or. present(right))) then
      res = lox_error("Cannot add '"//repr(left)//"' and '"//repr(right)//"'")
      return
    end if

    select type(left)
    type is(lox_number)
      select type(right)
      type is(lox_number)
        res = lox_number(left%value + right%value)
      class default
        res = lox_error("Cannot add '"//repr(left)//"' and '"//repr(right)//"'")
      end select

    type is(lox_string)
      select type(right)
      type is(lox_string)
        res = lox_string(left%value//right%value)
      class default
        res = lox_error("Cannot add '"//repr(left)//"' and '"//repr(right)//"'")
      end select

    class default
      res = lox_error("Cannot add '"//repr(left)//"' and '"//repr(right)//"'")
    end select
  end function op_add

  pure function op_lt(left, right) result(res)
    class(lox_object), intent(in), optional :: left, right
    class(lox_object), allocatable :: res

    res = op_compare(left, right, lt)
  contains
    pure function lt(left, right) result(res)
      type(lox_number), intent(in) :: left, right
      type(lox_boolean) :: res

      res = lox_boolean(left%value < right%value)
    end function lt
  end function op_lt

  pure function op_le(left, right) result(res)
    class(lox_object), intent(in), optional :: left, right
    class(lox_object), allocatable :: res

    res = op_compare(left, right, le)
  contains
    pure function le(left, right) result(res)
      type(lox_number), intent(in) :: left, right
      type(lox_boolean) :: res

      res = lox_boolean(left%value < right%value)
    end function le
  end function op_le

  pure function op_gt(left, right) result(res)
    class(lox_object), intent(in), optional :: left, right
    class(lox_object), allocatable :: res

    res = op_compare(left, right, gt)
  contains
    pure function gt(left, right) result(res)
      type(lox_number), intent(in) :: left, right
      type(lox_boolean) :: res

      res = lox_boolean(left%value > right%value)
    end function gt
  end function op_gt

  pure function op_ge(left, right) result(res)
    class(lox_object), intent(in), optional :: left, right
    class(lox_object), allocatable :: res

    res = op_compare(left, right, ge)
  contains
    pure function ge(left, right) result(res)
      type(lox_number), intent(in) :: left, right
      type(lox_boolean) :: res

      res = lox_boolean(left%value >= right%value)
    end function ge
  end function op_ge

  pure function op_compare(left, right, compare) result(res)
    class(lox_object), intent(in), optional :: left, right
    class(lox_object), allocatable :: res
    procedure(compare_interface) :: compare

    if (.not.(present(left) .or. present(right))) then
      res = lox_error("Cannot compare '"//repr(left)//"' and '"//repr(right)//"'")
      return
    end if

    select type(left)
    type is(lox_number)
      select type(right)
      type is(lox_number)
        res = compare(left, right)
      class default
        res = lox_error("Cannot compare '"//repr(left)//"' and '"//repr(right)//"'")
      end select

    class default
      res = lox_error("Cannot compare '"//repr(left)//"' and '"//repr(right)//"'")
    end select
  end function op_compare

  pure function op_equal(left, right) result(res)
    class(lox_object), intent(in), optional :: left, right
    class(lox_object), allocatable :: res

    if (.not.(present(left) .and. present(right))) then
      res = lox_boolean(.true.)
      return
    end if

    if (.not.(present(left) .or. present(right))) then
      res = lox_boolean(.false.)
      return
    end if

    select type(left)
    type is(lox_number)
      select type(right)
      type is(lox_number)
        res = lox_boolean(left%value == right%value)
      class default
        res = lox_boolean(.false.)
      end select

    type is(lox_boolean)
      select type(right)
      type is(lox_boolean)
        res = lox_boolean(left%value .eqv. right%value)
      class default
        res = lox_boolean(.false.)
      end select

    type is(lox_string)
      select type(right)
      type is(lox_string)
        if (len(left%value) == len(right%value)) then
          res = lox_boolean(left%value == right%value)
        else
          res = lox_boolean(.false.)
        end if
      class default
        res = lox_boolean(.false.)
      end select

    class default
      res = lox_error("Cannot compare '"//repr(left)//"' and '"//repr(right)//"'")
    end select
  end function op_equal

  pure function stringify(object) result(string)
    class(lox_object), intent(in), optional :: object
    character(len=:), allocatable :: string

    if (present(object)) then
      select type(object)
      type is(lox_boolean)
        string = to_string(object%value)
      type is(lox_number)
        string = to_string(object%value)
      type is(lox_string)
        string = object%value
      type is(lox_error)
        string = "error("//object%message//")"
      class default
        string = "unknown"
      end select
    else
      string = "nil"
    end if
  end function stringify

  pure function repr(object) result(string)
    class(lox_object), intent(in), optional :: object
    character(len=:), allocatable :: string

    if (present(object)) then
      select type(object)
      type is(lox_boolean)
        string = "boolean("//to_string(object%value)//")"
      type is(lox_number)
        string = "number("//to_string(object%value)//")"
      type is(lox_string)
        string = "string("""//object%value//""")"
      type is(lox_error)
        string = "error("//object%message//")"
      class default
        string = "unknown"
      end select
    else
      string = "nil"
    end if
  end function repr

  recursive subroutine evaluate(self, expr)
    class(lox_interpreter), intent(inout) :: self
    class(lox_expr), intent(in) :: expr

    call expr%accept(self)
  end subroutine evaluate

  pure function new_string(value) result(new)
    character(len=*), intent(in) :: value
    type(lox_string) :: new
    new%value = value
  end function new_string

  pure function real_to_string(value) result(string)
    real(lox_real), intent(in) :: value
    character(len=:), allocatable :: string

    character(len=512) :: buffer
    integer :: stat

    write(buffer, '(g0)', iostat=stat) value
    if (stat == 0) then
      string = trim(buffer)
    else
      string = "NaN"
    end if
  end function real_to_string

  pure function logical_to_string(value) result(string)
    logical, intent(in) :: value
    character(len=merge(4, 5, value)) :: string

    if (value) then
      string = "true"
    else
      string = "false"
    end if
  end function logical_to_string

end module flox_interpreter
