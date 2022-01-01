! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_interpreter
  use, intrinsic :: iso_fortran_env, only : lox_real => real64, output_unit
  use flox_ast, only : lox_visitor, lox_expr, lox_ast, &
    & lox_block, lox_expr_stmt, lox_print, lox_var, lox_if, lox_while, lox_fun, lox_return, &
    & lox_assign, lox_logical, lox_binary, lox_grouping, lox_literal, lox_unary, lox_call
  use flox_diagnostic, only : lox_diagnostic, label_type, level_runtime_error
  use flox_environment, only : lox_environment, lox_scope, lox_object, lox_error
  use flox_map, only : map_type => cuckoo_hash, new_map => new_cuckoo_hash, container_type
  use flox_scanner, only : lox_token, line_descriptor, MINUS, BANG, STAR, SLASH, PLUS, &
    & GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, EQUAL_EQUAL, BANG_EQUAL, &
    & IDENTIFIER, STRING, NUMBER
  use stdlib_strings, only : to_string
  implicit none
  private

  public :: lox_interpreter, new_interpreter
  public :: lox_object, repr

  type, extends(lox_object) :: lox_boolean
    logical :: value
  end type lox_boolean

  type, extends(lox_object) :: lox_number
    real(lox_real) :: value
  end type lox_number

  type, extends(lox_object) :: lox_string
    character(len=:), allocatable :: value
  end type lox_string

  type, extends(lox_object), abstract :: lox_callable
  contains
    procedure(arity), deferred :: arity
    procedure(call), deferred :: call
  end type lox_callable

  interface lox_string
    module procedure :: new_string
  end interface lox_string

  type, extends(lox_visitor) :: lox_interpreter
    type(lox_diagnostic), allocatable :: diag(:)
    class(lox_object), allocatable :: local
    type(lox_environment) :: env
    logical :: return = .false.
    type(map_type) :: locals
  contains
    procedure :: evaluate
    procedure :: resolve
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
  end type lox_interpreter

  interface stringify
    module procedure :: stringify
    module procedure :: logical_to_string
    module procedure :: real_to_string
  end interface stringify

  abstract interface
    pure function compare_interface(left, right) result(res)
      import :: lox_number, lox_boolean
      type(lox_number), intent(in) :: left, right
      type(lox_boolean) :: res
    end function compare_interface
  end interface

  abstract interface
    function arity(self)
      import :: lox_callable
      class(lox_callable), intent(in) :: self
      integer :: arity
    end function arity

    subroutine call(self, interpreter, args)
      import :: lox_callable, lox_interpreter, lox_scope
      class(lox_callable), intent(in) :: self
      class(lox_interpreter), intent(inout) :: interpreter
      class(lox_scope), intent(inout) :: args
    end subroutine call
  end interface


  type, extends(lox_callable) :: lox_builtin
    integer :: narg = 0
    procedure(builtin), pointer, nopass :: impl => null()
  contains
    procedure :: arity => arity_builtin
    procedure :: call => call_builtin
  end type lox_builtin

  interface lox_builtin
    module procedure :: new_builtin
  end interface lox_builtin

  abstract interface
    subroutine builtin(self, args)
      import :: lox_interpreter, lox_scope
      class(lox_interpreter), intent(inout) :: self
      class(lox_scope), intent(inout) :: args
    end subroutine builtin
  end interface

  type, extends(lox_callable) :: lox_function
    class(lox_fun), allocatable :: decl
    integer :: closure
  contains
    procedure :: arity => arity_function
    procedure :: call => call_function
  end type lox_function


  interface int
    module procedure :: cont_to_int
  end interface int


contains

  subroutine new_interpreter(self)
    type(lox_interpreter), intent(out) :: self

    call new_map(self%locals)
  end subroutine new_interpreter

  recursive subroutine visit_ast(self, ast)
    class(lox_interpreter), intent(inout) :: self
    class(lox_ast), intent(in) :: ast

    integer :: istmt

    ! Initialize the global scope
    if (self%env%current == 0) then
      call self%env%push
      call add_builtin(self)
    end if

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

    block
      type(container_type), pointer :: ptr
      integer :: iscope
      call self%locals%get(to_string(expr%id), ptr)
      if (associated(ptr)) then
        iscope = get_env(self%env, int(ptr))
        call self%env%scopes(iscope)%assign(expr%name, self%local)
      else
        call self%env%scopes(1)%assign(expr%name, self%local)
      end if
      call handle_error(self, expr)
    end block
  end subroutine visit_assign

  recursive subroutine visit_logical(self, expr)
    class(lox_interpreter), intent(inout) :: self
    class(lox_logical), intent(in) :: expr

    type(lox_boolean) :: left

    call self%evaluate(expr%left)
    call handle_error(self, expr)

    left = cast_boolean(self%local)
    if (expr%operator%val == "or" .and. left%value) return
    if (expr%operator%val == "and" .and. .not.left%value) return

    call self%evaluate(expr%right)
    call handle_error(self, expr)
  end subroutine visit_logical

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
        block
          type(container_type), pointer :: ptr
          integer :: iscope
          call self%locals%get(to_string(expr%id), ptr)
          if (associated(ptr)) then
            iscope = get_env(self%env, int(ptr))
            call self%env%scopes(iscope)%get(expr%object, self%local)
          else
            call self%env%scopes(1)%get(expr%object, self%local)
          end if
        end block

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

  recursive subroutine visit_call(self, expr)
    class(lox_interpreter), intent(inout) :: self
    class(lox_call), intent(in) :: expr

    class(lox_object), allocatable :: callee
    class(lox_callable), allocatable :: func
    type(lox_scope) :: scope
    integer :: iarg

    call self%evaluate(expr%callee)
    call handle_error(self, expr)
    call move_alloc(self%local, callee)

    do iarg = 1, expr%narg
      associate(arg => expr%args(iarg)%expr)
        call self%evaluate(arg)
        call handle_error(self, arg)
        call scope%define(new_identifier("$"//to_string(iarg)), self%local)
        call handle_error(self, arg)
      end associate
    end do

    func = cast_callable(callee)
    if (.not.allocated(func)) then
      self%local = lox_error("Can only call functions and classes.")
      call handle_error(self, expr)
      return
    end if

    if (expr%narg /= func%arity()) then
      self%local = lox_error("Expected "//to_string(func%arity())//" arguments but got "//&
        & to_string(expr%narg))
      call handle_error(self, expr)
      return
    end if

    call func%call(self, scope)

  end subroutine visit_call

  recursive subroutine visit_block(self, stmt)
    class(lox_interpreter), intent(inout) :: self
    class(lox_block), intent(in) :: stmt

    integer :: istmt

    call self%env%push
    blck_stmt: do istmt = 1, stmt%nstmt
      associate(entry => stmt%stmts(istmt)%stmt)
        call entry%accept(self)
        if (self%return) exit blck_stmt
      end associate
    end do blck_stmt
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
    class(lox_expr), allocatable :: expr

    expr = lox_literal(-1, stmt%name)
    if (allocated(self%local)) deallocate(self%local)
    if (allocated(stmt%expression)) then
      call self%evaluate(stmt%expression)
    end if
    associate(scope => self%env%scopes(self%env%current))
      call scope%define(stmt%name, self%local)
    end associate
    call handle_error(self, expr)
    if (allocated(self%local)) deallocate(self%local)
  end subroutine visit_var

  recursive subroutine visit_if(self, stmt)
    class(lox_interpreter), intent(inout) :: self
    class(lox_if), intent(in) :: stmt

    class(lox_boolean), allocatable :: condition

    call self%evaluate(stmt%condition)
    condition = cast_boolean(self%local)
    if (allocated(self%local)) deallocate(self%local)

    if (condition%value) then
      call stmt%then_branch%accept(self)
    else if (allocated(stmt%else_branch)) then
      call stmt%else_branch%accept(self)
    end if
  end subroutine visit_if

  recursive subroutine visit_while(self, stmt)
    class(lox_interpreter), intent(inout) :: self
    class(lox_while), intent(in) :: stmt

    class(lox_boolean), allocatable :: condition

    while_loop: do
      call self%evaluate(stmt%condition)
      condition = cast_boolean(self%local)
      if (allocated(self%local)) deallocate(self%local)
      if (.not.condition%value) exit while_loop

      call stmt%body%accept(self)
      if (self%return) exit while_loop
    end do while_loop
  end subroutine visit_while

  recursive subroutine visit_fun(self, stmt)
    class(lox_interpreter), intent(inout) :: self
    class(lox_fun), intent(in) :: stmt

    class(lox_object), allocatable :: object
    integer :: closure

    closure = self%env%current
    object = lox_function(stmt, closure)
    associate(scope => self%env%scopes(closure))
      scope%ref = scope%ref + 1
      call scope%define(stmt%name, object)
    end associate
  end subroutine visit_fun

  recursive subroutine visit_return(self, stmt)
    class(lox_interpreter), intent(inout) :: self
    class(lox_return), intent(in) :: stmt

    if (allocated(self%local)) deallocate(self%local)
    if (allocated(stmt%expression)) then
      call self%evaluate(stmt%expression)
    end if
    self%return = .true.
  end subroutine visit_return


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
    type(lox_boolean), allocatable :: res

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

  pure function cast_callable(object) result(res)
    class(lox_object), intent(in), optional :: object
    class(lox_callable), allocatable :: res

    select type(object)
    class is(lox_callable)
      res = object
    end select
  end function cast_callable

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

      res = lox_boolean(left%value <= right%value)
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
        string = stringify(object%value)
      type is(lox_number)
        string = stringify(object%value)
      type is(lox_string)
        string = object%value
      type is(lox_function)
        string = "<fun "//object%decl%name%val//">"
      type is(lox_error)
        string = "error("//object%message//")"
      type is(lox_builtin)
        string = "<builtin>"
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
        string = "boolean("//stringify(object%value)//")"
      type is(lox_number)
        string = "number("//stringify(object%value)//")"
      type is(lox_string)
        string = "string("""//object%value//""")"
      type is(lox_error)
        string = "error("//object%message//")"
      type is(lox_function)
        string = "function(<"//object%decl%name%val//">)"
      type is(lox_builtin)
        string = "function(<builtin>)"
      class default
        string = "unknown"
      end select
    else
      string = "nil"
    end if
  end function repr

  subroutine resolve(self, expr, distance)
    class(lox_interpreter), intent(inout) :: self
    class(lox_expr), intent(in) :: expr
    integer, intent(in) :: distance

    type(container_type), pointer :: ptr

    call self%locals%insert(to_string(expr%id), ptr)
    ptr%val = distance
  end subroutine resolve

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


  module subroutine add_builtin(self)
    class(lox_interpreter), intent(inout) :: self

    class(lox_object), allocatable :: builtin
    type(lox_token) :: id

    associate(scope => self%env%scopes(self%env%current))
      id = new_identifier("nil")
      call scope%define(id, builtin)
      call scope%freeze(id, builtin)

      id = new_identifier("true")
      builtin = lox_boolean(.true.)
      call scope%define(id, builtin)
      call scope%freeze(id, builtin)

      id = new_identifier("false")
      builtin = lox_boolean(.false.)
      call scope%define(id, builtin)
      call scope%freeze(id, builtin)

      id = new_identifier("clock")
      builtin = lox_builtin(0, builtin_clock)
      call scope%define(id, builtin)
      call scope%freeze(id, builtin)

      id = new_identifier("print")
      builtin = lox_builtin(1, builtin_print)
      call scope%define(id, builtin)
      call scope%freeze(id, builtin)
    end associate
  end subroutine add_builtin

  subroutine builtin_clock(self, args)
    class(lox_interpreter), intent(inout) :: self
    class(lox_scope), intent(inout) :: args

    integer, parameter :: i8 = selected_int_kind(18)
    integer(i8) :: time_count, time_rate, time_max

    call system_clock(time_count, time_rate, time_max)
    self%local = lox_number(real(time_count, lox_real)/real(time_rate, lox_real))
  end subroutine builtin_clock

  subroutine builtin_print(self, args)
    class(lox_interpreter), intent(inout) :: self
    class(lox_scope), intent(inout) :: args

    call args%get(new_identifier("$1"), self%local)
    write(output_unit, '(a)') stringify(self%local)
    if (allocated(self%local)) deallocate(self%local)
  end subroutine builtin_print

  function new_identifier(name) result(token)
    character(len=*), intent(in) :: name
    type(lox_token) :: token

    token = lox_token(1, 0, IDENTIFIER, name)
  end function new_identifier

  function new_builtin(narg, impl) result(object)
    integer, intent(in) :: narg
    procedure(builtin) :: impl
    type(lox_builtin) :: object

    object%narg = narg
    object%impl => impl
  end function new_builtin

  function arity_builtin(self) result(narg)
    class(lox_builtin), intent(in) :: self
    integer :: narg

    narg = self%narg
  end function arity_builtin

  subroutine call_builtin(self, interpreter, args)
    class(lox_builtin), intent(in) :: self
    class(lox_interpreter), intent(inout) :: interpreter
    class(lox_scope), intent(inout) :: args

    call self%impl(interpreter, args)
  end subroutine call_builtin

  function arity_function(self) result(narg)
    class(lox_function), intent(in) :: self
    integer :: narg

    narg = size(self%decl%params)
  end function arity_function

  subroutine call_function(self, interpreter, args)
    class(lox_function), intent(in) :: self
    class(lox_interpreter), intent(inout) :: interpreter
    class(lox_scope), intent(inout) :: args

    call interpreter%env%push(self%closure)
    fun_body: block
      class(lox_object), allocatable :: object
      type(lox_token) :: id
      integer :: iarg

      associate(scope => interpreter%env%scopes(interpreter%env%current))
        do iarg = 1, size(self%decl%params)
          call args%get(new_identifier("$"//to_string(iarg)), object)
          call scope%define(self%decl%params(iarg), object)
        end do
      end associate

      call self%decl%body%accept(interpreter)
      if (interpreter%return) interpreter%return = .false.
    end block fun_body
    call interpreter%env%pop
  end subroutine call_function

  pure function cont_to_int(ptr) result(res)
    type(container_type), intent(in) :: ptr
    integer :: res

    res = 0
    select type(val => ptr%val)
    type is(integer)
      res = val
    end select
  end function cont_to_int

  pure function get_env(env, distance) result(res)
    type(lox_environment), intent(in) :: env
    integer, intent(in) :: distance
    integer :: res

    integer :: iscope

    res = env%current
    do iscope = 1, distance
      res = env%scopes(res)%parent
    end do
  end function get_env

end module flox_interpreter
