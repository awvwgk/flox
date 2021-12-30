! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_terminal
  implicit none
  private

  public :: lox_terminal

  character(len=*), parameter :: &
    esc = achar(27), &
    code = esc // "[0;", &
    reset = esc // "[0m"

  character(len=*), parameter :: &
    bold = code // "1m", &
    dim = code // "2m", &
    italic = code // "3m", &
    underline = code // "4m", &
    blink = code // "5m", &
    reverse = code // "7m", &
    hidden = code // "8m"

  character(len=*), parameter :: &
    black = code // "30m", &
    red = code // "31m", &
    green = code // "32m", &
    yellow = code // "33m", &
    blue = code // "34m", &
    magenta = code // "35m", &
    cyan = code // "36m", &
    white = code // "37m"

  character(len=*), parameter :: &
    bg_black = code // "40m", &
    bg_red = code // "41m", &
    bg_green = code // "42m", &
    bg_yellow = code // "43m", &
    bg_blue = code // "44m", &
    bg_magenta = code // "45m", &
    bg_cyan = code // "46m", &
    bg_white = code // "47m"

  character(len=*), parameter :: &
    bold_black = code // "1;30m", &
    bold_red = code // "1;31m", &
    bold_green = code // "1;32m", &
    bold_yellow = code // "1;33m", &
    bold_blue = code // "1;34m", &
    bold_magenta = code // "1;35m", &
    bold_cyan = code // "1;36m", &
    bold_white = code // "1;37m"

  !> Colorizer class for handling colorful output in the terminal
  type :: lox_terminal

    character(len=:), allocatable :: &
      reset, &
      bold, &
      dim, &
      italic, &
      underline, &
      blink, &
      reverse, &
      hidden

    character(len=:), allocatable :: &
      black, &
      red, &
      green, &
      yellow, &
      blue, &
      magenta, &
      cyan, &
      white

    character(len=:), allocatable :: &
      bg_black, &
      bg_red, &
      bg_green, &
      bg_yellow, &
      bg_blue, &
      bg_magenta, &
      bg_cyan, &
      bg_white

    character(len=:), allocatable :: &
      bold_black, &
      bold_red, &
      bold_green, &
      bold_yellow, &
      bold_blue, &
      bold_magenta, &
      bold_cyan, &
      bold_white

  end type lox_terminal

  !> Constructor for the colorizer
  interface lox_terminal
    module procedure :: new_terminal
  end interface lox_terminal

contains

  !> Create a new colorizer object
  function new_terminal(use_color) result(new)
    !> Enable color output
    logical, intent(in) :: use_color
    !> New instance of the colorizer
    type(lox_terminal) :: new

    if (use_color) then
      new%reset = reset
      new%bold = bold
      new%dim = dim
      new%italic = italic
      new%underline = underline
      new%blink = blink
      new%reverse = reverse
      new%hidden = hidden
      new%black = black
      new%red = red
      new%green = green
      new%yellow = yellow
      new%blue = blue
      new%magenta = magenta
      new%cyan = cyan
      new%white = white
      new%bg_black = bg_black
      new%bg_red = bg_red
      new%bg_green = bg_green
      new%bg_yellow = bg_yellow
      new%bg_blue = bg_blue
      new%bg_magenta = bg_magenta
      new%bg_cyan = bg_cyan
      new%bg_white = bg_white
      new%bold_black = bold_black
      new%bold_red = bold_red
      new%bold_green = bold_green
      new%bold_yellow = bold_yellow
      new%bold_blue = bold_blue
      new%bold_magenta = bold_magenta
      new%bold_cyan = bold_cyan
      new%bold_white = bold_white
    else
      new%reset = ""
      new%bold = ""
      new%dim = ""
      new%italic = ""
      new%underline = ""
      new%blink = ""
      new%reverse = ""
      new%hidden = ""
      new%black = ""
      new%red = ""
      new%green = ""
      new%yellow = ""
      new%blue = ""
      new%magenta = ""
      new%cyan = ""
      new%white = ""
      new%bg_black = ""
      new%bg_red = ""
      new%bg_green = ""
      new%bg_yellow = ""
      new%bg_blue = ""
      new%bg_magenta = ""
      new%bg_cyan = ""
      new%bg_white = ""
      new%bold_black = ""
      new%bold_red = ""
      new%bold_green = ""
      new%bold_yellow = ""
      new%bold_blue = ""
      new%bold_magenta = ""
      new%bold_cyan = ""
      new%bold_white = ""
    end if
  end function new_terminal

end module flox_terminal
