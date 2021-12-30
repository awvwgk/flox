! This file is part of flox.
! SPDX-Identifier: Apache-2.0

!> Reporting system with exception-like behavior
module flox_app_report
  use, intrinsic :: iso_fortran_env, only : error_unit
  implicit none
  private

  public :: app_report, is_error
  public :: fatal_error, printout, clear_report

  enum, bind(c)
    enumerator :: &
      success, info, failure
  end enum

  !> Status report for exception-like error handling
  type :: app_report
    !> Report status
    integer :: stat = success
    !> Message to report
    character(len=:), allocatable :: message
  contains
    final :: escalate_report
  end type app_report

contains

  !> Create a fatal error
  subroutine fatal_error(report, message)
    !> Instance of the report
    type(app_report), allocatable, intent(out) :: report
    !> Message associated with the error
    character(len=*), intent(in) :: message

    allocate(report)
    report%stat = failure
    report%message = message
  end subroutine fatal_error

  !> Create a fatal error
  subroutine printout(report, message)
    !> Instance of the report
    type(app_report), allocatable, intent(out) :: report
    !> Message associated with the error
    character(len=*), intent(in) :: message

    allocate(report)
    report%stat = info
    report%message = message
  end subroutine printout

  !> Clear report
  subroutine clear_report(report)
    !> Instance of the report
    type(app_report), allocatable, intent(inout) :: report

    if (allocated(report)) then
      report%stat = success
      if (allocated(report%message)) then
        deallocate(report%message)
      end if
      deallocate(report)
    end if
  end subroutine clear_report

  !> Escalate an uncaught exception
  subroutine escalate_report(report)
    !> Instance of the report
    type(app_report), intent(inout) :: report

    if (report%stat /= success) then
      write(error_unit, '(a)') "Uncaught exception encountered"
      if (allocated(report%message)) then
        write(error_unit, '(a)') report%message
      end if
      error stop
    end if
  end subroutine escalate_report

  !> Check whether a report is an error
  pure function is_error(report)
    !> Instance of the report
    type(app_report), intent(in), optional :: report
    !> Return true if the report is an error
    logical :: is_error

    is_error = .false.
    if (present(report)) then
      is_error = .not.any(report%stat == [success, info])
    end if
  end function is_error

end module flox_app_report
