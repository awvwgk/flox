! This file is part of flox.
! SPDX-Identifier: Apache-2.0

!> Definition of the command line interface
module flox_app_cli
  use flox_app_report, only : app_report, fatal_error, printout
  use flox_app_help, only : help_text
  implicit none
  private

  public :: app_config, get_arguments

  !> Configuration for the command line interface
  type :: app_config
    !> Input file for the interpreter
    character(len=:), allocatable :: input_file
  end type app_config

contains

  !> Evaluate the command line arguments
  subroutine get_arguments(config, report)
    !> Instance of the command line configuration
    type(app_config), intent(out) :: config
    !> Report for error handling
    type(app_report), allocatable, intent(out) :: report

    integer :: iarg, narg
    character(len=:), allocatable :: arg

    iarg = 0
    narg = command_argument_count()
    do while(iarg < narg)
      iarg = iarg + 1
      call get_argument(iarg, arg)
      select case(arg)
      case("--help")
        call printout(report, help_text)
        exit
      case default
        if (.not. allocated(config%input_file)) then
          call move_alloc(arg, config%input_file)
          cycle
        end if
        call fatal_error(report, "Too many positional arguments provided: '"//arg//"'")
        exit
      end select
    end do
    if (allocated(report)) return

  end subroutine get_arguments

  !> Obtain the command line argument at a given index
  subroutine get_argument(idx, arg)
    !> Index of command line argument, range [0:command_argument_count()]
    integer, intent(in) :: idx
    !> Command line argument
    character(len=:), allocatable, intent(out) :: arg

    integer :: length, stat

    call get_command_argument(idx, length=length, status=stat)
    if (stat /= 0) then
      return
    endif

    allocate(character(len=length) :: arg, stat=stat)
    if (stat /= 0) then
      return
    endif

    if (length > 0) then
      call get_command_argument(idx, arg, status=stat)
      if (stat /= 0) then
        deallocate(arg)
        return
      end if
    end if
  end subroutine get_argument

end module flox_app_cli
