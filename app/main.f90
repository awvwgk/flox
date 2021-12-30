! This file is part of flox.
! SPDX-Identifier: Apache-2.0

program main
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  use flox_app_cli, only : app_config, get_arguments
  use flox_app_driver, only : driver
  use flox_app_report, only : app_report, clear_report, is_error
  implicit none

  type(app_config) :: config
  type(app_report), allocatable :: report

  call get_arguments(config, report)
  call handle_report(report)

  call driver(config, report)
  call handle_report(report)

contains

  subroutine handle_report(report)
    type(app_report), allocatable, intent(inout) :: report

    interface
      subroutine sys_exit(stat) bind(c, name="exit")
        use, intrinsic :: iso_c_binding, only : c_int
        integer(c_int), value :: stat
      end subroutine sys_exit
    end interface
    integer :: stat

    if (allocated(report)) then
      stat = merge(1, 0, is_error(report))
      write(merge(error_unit, output_unit, is_error(report)), '(a)') &
        & report%message
      call clear_report(report)

      ! Use exit(3) to get consistent behaviour across compilers
      call sys_exit(stat)
    end if
  end subroutine handle_report

end program main
