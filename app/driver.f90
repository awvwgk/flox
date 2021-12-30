! This file is part of flox.
! SPDX-Identifier: Apache-2.0

!> Definition of the command line driver
module flox_app_driver
  use flox, only : run_file, run_prompt
  use flox_app_cli, only : app_config
  use flox_app_report, only : app_report
  use flox_terminal, only : lox_terminal
  implicit none
  private

  public :: driver

contains

  !> Main entry point for the application driver
  subroutine driver(config, report)
    !> Command-line configuration
    type(app_config), intent(in) :: config
    !> Report for error handling
    type(app_report), allocatable, intent(out) :: report

    if (allocated(config%input_file)) then
      call run_file(config%input_file, lox_terminal(.true.))
    else
      call run_prompt(lox_terminal(.true.))
    end if
  end subroutine driver

end module flox_app_driver
