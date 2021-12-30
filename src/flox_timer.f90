! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_timer
  use stdlib_strings, only : to_string
  implicit none
  private

  public :: lox_timer, format_time, tp


  !> Double precision real numbers
  integer, parameter :: tp = selected_real_kind(15)

  !> Long length for integers
  integer, parameter :: i8 = selected_int_kind(18)

  type :: time_record
    character(len=:), allocatable :: label
    logical :: running = .false.
    real(tp) :: time = 0.0_tp
  end type time_record

  type :: lox_timer
    integer :: n = 0
    character(len=:), allocatable :: last
    type(time_record), allocatable :: record(:)
  contains
    procedure :: push
    procedure :: pop
    procedure :: get
  end type lox_timer

contains


  subroutine push(self, label)
    class(lox_timer), intent(inout) :: self
    character(len=*), intent(in) :: label

    integer :: it

    if (.not.allocated(self%record)) call resize(self%record)
    it = find(self%record(:self%n), label)

    if (it == 0) then
      if (self%n >= size(self%record)) then
        call resize(self%record)
      end if

      self%n = self%n + 1
      it = self%n
      self%record(it) = time_record(label)
    end if

    associate(record => self%record(it))
      self%last = record%label
      record%time = record%time + timing() * merge(+1, -1, record%running)
      record%running = .not.record%running
    end associate
  end subroutine push


  subroutine pop(self)
    class(lox_timer), intent(inout) :: self

    integer :: it

    if (.not.allocated(self%record)) return
    it = find(self%record(:self%n), self%last)
    if (it == 0) return

    associate(record => self%record(it))
      record%time = record%time + timing() * merge(+1, -1, record%running)
      record%running = .not.record%running
    end associate
    if (allocated(self%last)) deallocate(self%last)
  end subroutine pop


  function get(self, label) result(time)
    class(lox_timer), intent(in) :: self
    character(len=*), intent(in) :: label
    real(tp) :: time

    integer :: it

    time = 0.0_tp
    if (self%n <= 0) return
    it = find(self%record(:self%n), label)
    if (it == 0) return

    associate(record => self%record(it))
      time = record%time
      if (record%running) then
        time = time + timing()
      end if
    end associate
  end function get


  pure function find(record, label) result(pos)
    type(time_record), intent(in) :: record(:)
    character(len=*), intent(in), optional :: label
    integer :: pos

    integer :: i

    pos = 0
    if (present(label)) then
      do i = size(record), 1, -1
        if (allocated(record(i)%label)) then
          if (label == record(i)%label) then
            pos = i
            exit
          end if
        end if
      end do
    else
      do i = size(record), 1, -1
        if (record(i)%running) then
          pos = i
          exit
        end if
      end do
    end if
  end function find


  function format_time(time) result(string)
    real(tp), intent(in) :: time
    character(len=:), allocatable :: string

    real(tp) :: secs
    integer :: mins, hours, days

    secs = time
    days = int(secs/86400.0_tp)
    secs = secs - days*86400.0_tp
    hours = int(secs/3600.0_tp)
    secs = secs - hours*3600.0_tp
    mins = int(secs/60.0_tp)
    secs = time - mins*60.0_tp

    if (days > 0) then
      string = to_string(days, '(i0, " d,")')
    else
      string = repeat(" ", 4)
    end if
    if (hours > 0) then
      string = string // to_string(hours, '(1x, i2, " h,")')
    else
      string = string // repeat(" ", 6)
    end if
    if (mins > 0) then
      string = string // to_string(mins, '(1x, i2, " min,")')
    else
      string = string // repeat(" ", 8)
    end if
    string = string // to_string(secs, '(f6.3)')//" sec"
  end function format_time


  function timing() result(time)
    real(tp) :: time

    integer(i8) :: time_count, time_rate, time_max
    call system_clock(time_count, time_rate, time_max)
    time = real(time_count, tp)/real(time_rate, tp)
  end function timing


  !> Reallocate list of timing records
  pure subroutine resize(var, n)
    !> Instance of the array to be resized
    type(time_record), allocatable, intent(inout) :: var(:)
    !> Dimension of the final array size
    integer, intent(in), optional :: n

    type(time_record), allocatable :: tmp(:)
    integer :: this_size, new_size
    integer, parameter :: initial_size = 20

    if (allocated(var)) then
      this_size = size(var, 1)
      call move_alloc(var, tmp)
    else
      this_size = initial_size
    end if

    if (present(n)) then
      new_size = n
    else
      new_size = this_size + this_size/2 + 1
    end if

    allocate(var(new_size))

    if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(var, 1))
      var(:this_size) = tmp(:this_size)
      deallocate(tmp)
    end if

  end subroutine resize

end module flox_timer
