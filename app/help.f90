! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_app_help
  implicit none
  private

  public :: progname, help_text


  !> Name of the program
  character(len=*), parameter :: progname = "flox"

  !> Newline character
  character(len=*), parameter :: nl = new_line('a')

  !> Usage message
  character(len=*), parameter :: help_text = &
    "Usage: "//progname//" [options] [file]" // nl // &
    "" // nl // &
    "A script file can be provided as argument. If no script is provided the" // nl // &
    "interpreter is started in interactive mode." // nl // &
    "" // nl // &
    "Options:" // nl // &
    "" // nl // &
    "  --help                 Print this help message and exit" // nl // &
    "" // nl // &
    "Implementation of lox interpreter after https://craftinginterpreters.com" // nl // &
    ""

end module flox_app_help
