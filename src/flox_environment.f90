! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_environment
  use flox_scanner, only : lox_token
  implicit none
  private

  public :: lox_object, lox_error, lox_environment, lox_scope

  type, abstract :: lox_object
  end type lox_object

  type, extends(lox_object) :: lox_error
    character(len=:), allocatable :: message
  end type lox_error

  type :: lox_entity
    type(lox_token), allocatable :: var
    class(lox_object), allocatable :: obj
    logical :: frozen = .false.
  end type lox_entity

  type :: lox_environment
    integer :: current = 0
    integer :: nscope = 0
    type(lox_scope), allocatable :: scopes(:)
  contains
    procedure :: push => push_env
    procedure :: pop => pop_env
    procedure :: define => define_env
    procedure :: assign => assign_env
    procedure :: get => get_env
    procedure :: freeze => freeze_env
  end type lox_environment

  type :: lox_scope
    integer :: parent = 0
    integer :: nobj = 0
    type(lox_entity), allocatable :: objects(:)
  contains
    procedure :: define => define_scope
    procedure :: assign => assign_scope
    procedure :: get => get_scope
    procedure :: freeze => freeze_scope
  end type lox_scope

  interface resize
    module procedure resize_scope
    module procedure resize_entity
  end interface resize

  interface move
    module procedure move_scope
    module procedure move_entity
  end interface move

contains

  !> Reallocate list of entities
  subroutine resize_entity(list, n)
    !> Instance of the array to be resized
    type(lox_entity), allocatable, intent(inout) :: list(:)
    !> Dimension of the final array size
    integer, intent(in), optional :: n

    type(lox_entity), allocatable :: tmp(:)
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
  end subroutine resize_entity

  !> Reallocate list of entities
  subroutine resize_scope(list, n)
    !> Instance of the array to be resized
    type(lox_scope), allocatable, intent(inout) :: list(:)
    !> Dimension of the final array size
    integer, intent(in), optional :: n

    type(lox_scope), allocatable :: tmp(:)
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
  end subroutine resize_scope

  !> Move allocation of an entity
  elemental subroutine move_entity(from, to)
    !> Source entity to move allocation from
    type(lox_entity), intent(inout) :: from
    !> Target entity to move allocation to
    type(lox_entity), intent(out) :: to

    call move_alloc(from%var, to%var)
    call move_alloc(from%obj, to%obj)
    to%frozen = from%frozen
  end subroutine move_entity

  !> Move allocation of a scope
  elemental subroutine move_scope(from, to)
    !> Source scope to move allocation from
    type(lox_scope), intent(inout) :: from
    !> Target scope to move allocation to
    type(lox_scope), intent(out) :: to

    to%parent = from%parent
    to%nobj = from%nobj
    call move(from%objects, to%objects)
    from%parent = 0
    from%nobj = 0
  end subroutine move_scope

  subroutine push_env(self)
    !> Instance of environment
    class(lox_environment), target, intent(inout) :: self

    integer :: parent

    parent = self%current
    if (.not.allocated(self%scopes)) call resize(self%scopes)
    if (self%nscope >= size(self%scopes)) call resize(self%scopes)

    self%nscope = self%nscope + 1
    self%current = self%nscope
    associate(scope => self%scopes(self%current))
      scope = lox_scope(parent=parent)
      call resize(scope%objects)
    end associate
  end subroutine push_env

  subroutine pop_env(self)
    !> Instance of environment
    class(lox_environment), target, intent(inout) :: self

    if (self%current == 0) return

    associate(scope => self%scopes(self%current))
      self%current = scope%parent
      self%nscope = self%nscope - 1
      scope = lox_scope()
    end associate
  end subroutine pop_env

  subroutine define_env(self, var, object)
    !> Instance of environment
    class(lox_environment), target, intent(inout) :: self
    !> Name of the variable
    type(lox_token), intent(in) :: var
    !> Object identified by the variable
    class(lox_object), allocatable, intent(inout) :: object

    integer :: iscope
    class(lox_object), allocatable :: copy

    iscope = self%current
    do
      if (allocated(object)) copy = object
      associate(scope => self%scopes(iscope))
        call scope%define(var, copy)
        iscope = scope%parent
      end associate
      if (.not.is_error(copy)) exit
      if (iscope == 0) exit
    end do
    call move_alloc(copy, object)
  end subroutine define_env

  subroutine define_scope(self, var, object)
    !> Instance of environment
    class(lox_scope), target, intent(inout) :: self
    !> Name of the variable
    type(lox_token), intent(in) :: var
    !> Object identified by the variable
    class(lox_object), allocatable, intent(inout) :: object

    type(lox_entity), pointer :: ptr

    if (.not.allocated(self%objects)) call resize(self%objects)
    call find(self, var, ptr)

    if (.not.associated(ptr)) then
      if (self%nobj <= size(self%objects)) call resize(self%objects)

      self%nobj = self%nobj + 1
      ptr => self%objects(self%nobj)
    end if
    if (ptr%frozen) then
      object = lox_error("Cannot redefine frozen symbol '"//var%val//"'")
      return
    end if

    ptr%var = var
    if (allocated(object)) ptr%obj = object
  end subroutine define_scope

  subroutine assign_env(self, var, object)
    !> Instance of environment
    class(lox_environment), target, intent(inout) :: self
    !> Name of the variable
    type(lox_token), intent(in) :: var
    !> Object identified by the variable
    class(lox_object), allocatable, intent(inout) :: object

    integer :: iscope
    class(lox_object), allocatable :: copy

    if (.not.allocated(self%scopes)) call resize(self%scopes)

    iscope = self%current
    do
      if (allocated(object)) copy = object
      associate(scope => self%scopes(iscope))
        call scope%assign(var, copy)
        iscope = scope%parent
      end associate
      if (.not.is_error(copy)) exit
      if (iscope == 0) exit
    end do
    call move_alloc(copy, object)
  end subroutine assign_env

  subroutine assign_scope(self, var, object)
    !> Instance of environment
    class(lox_scope), target, intent(inout) :: self
    !> Name of the variable
    type(lox_token), intent(in) :: var
    !> Object identified by the variable
    class(lox_object), allocatable, intent(inout) :: object

    type(lox_entity), pointer :: ptr

    if (.not.allocated(self%objects)) call resize(self%objects)
    call find(self, var, ptr)

    if (associated(ptr)) then
      if (ptr%frozen) then
        object = lox_error("Cannot assign to frozen symbol '"//var%val//"'")
        return
      end if
      ptr%var = var
      if (allocated(object)) ptr%obj = object
    else
      object = lox_error("Cannot assign to undefined variable '"//var%val//"'")
    end if
  end subroutine assign_scope

  subroutine get_env(self, var, object)
    !> Instance of environment
    class(lox_environment), target, intent(inout) :: self
    !> Name of the variable
    type(lox_token), intent(in) :: var
    !> Object identified by the variable
    class(lox_object), allocatable, intent(out) :: object

    integer :: iscope

    if (.not.allocated(self%scopes)) call resize(self%scopes)

    iscope = self%current
    do
      associate(scope => self%scopes(iscope))
        call scope%get(var, object)
        iscope = scope%parent
      end associate
      if (.not.is_error(object)) exit
      if (iscope == 0) exit
    end do
  end subroutine get_env

  subroutine get_scope(self, var, object)
    !> Instance of environment
    class(lox_scope), target, intent(inout) :: self
    !> Name of the variable
    type(lox_token), intent(in) :: var
    !> Object identified by the variable
    class(lox_object), allocatable, intent(out) :: object

    type(lox_entity), pointer :: ptr

    if (.not.allocated(self%objects)) call resize(self%objects)
    call find(self, var, ptr)

    if (associated(ptr)) then
      if (allocated(ptr%obj)) object = ptr%obj
    else
      object = lox_error("Undefined variable '"//var%val//"' referenced")
    end if
  end subroutine get_scope

  subroutine freeze_env(self, var, object)
    !> Instance of environment
    class(lox_environment), target, intent(inout) :: self
    !> Name of the variable
    type(lox_token), intent(in) :: var
    !> Object identified by the variable
    class(lox_object), allocatable, intent(out) :: object

    integer :: iscope

    if (.not.allocated(self%scopes)) call resize(self%scopes)

    iscope = self%current
    do
      associate(scope => self%scopes(iscope))
        call scope%freeze(var, object)
        iscope = scope%parent
      end associate
      if (.not.is_error(object)) exit
      if (iscope == 0) exit
    end do
  end subroutine freeze_env

  subroutine freeze_scope(self, var, object)
    !> Instance of environment
    class(lox_scope), target, intent(inout) :: self
    !> Name of the variable
    type(lox_token), intent(in) :: var
    !> Object identified by the variable
    class(lox_object), allocatable, intent(out) :: object

    type(lox_entity), pointer :: ptr

    if (.not.allocated(self%objects)) call resize(self%objects)
    call find(self, var, ptr)

    if (associated(ptr)) then
      ptr%frozen = .true.
    else
      object = lox_error("Undefined variable '"//var%val//"' referenced")
    end if
  end subroutine freeze_scope

  subroutine find(self, var, ptr)
    !> Instance of environment
    class(lox_scope), target, intent(in) :: self
    !> Name of the variable
    type(lox_token), intent(in) :: var
    !> Pointer to the variable
    type(lox_entity), pointer, intent(out) :: ptr

    integer :: iv

    ptr => null()
    do iv = 1, self%nobj
      if (self%objects(iv)%var%val == var%val) then
        ptr => self%objects(iv)
        exit
      end if
    end do
  end subroutine find

  !> Check whether an object is an error
  pure function is_error(object)
    !> Object to check
    class(lox_object), intent(in), optional :: object
    !> Result of the check
    logical :: is_error

    if (present(object)) then
      select type(object)
      type is(lox_error)
        is_error = .true.
      class default
        is_error = .false.
      end select
    else
      is_error = .false.
    end if
  end function is_error

end module flox_environment
