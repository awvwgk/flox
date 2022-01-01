! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_diagnostic
  use flox_terminal, only : lox_terminal
  implicit none
  private

  public :: render, operator(==)
  public :: lox_diagnostic, label_type


  interface render
    module procedure render_diagnostic
    module procedure render_text
    module procedure render_text_with_label
    module procedure render_text_with_labels
  end interface render

  interface operator(==)
    module procedure op_equal
  end interface operator(==)


  !> Enumerator for diagnostic levels
  type :: level_enum
    !> Raw identifier for enumerator
    integer :: id
  end type level_enum

  !> Actual enumerator values
  type(level_enum), parameter, public :: &
    level_error = level_enum(0), &
    level_syntax_error = level_enum(1), &
    level_eof_error = level_enum(2), &
    level_semantic_error = level_enum(3), &
    level_runtime_error = level_enum(4), &
    level_warning = level_enum(10), &
    level_help = level_enum(20), &
    level_note = level_enum(30), &
    level_info = level_enum(40)


  type label_type
    !> Level of message
    type(level_enum) :: level
    !> Primary message
    logical :: primary
    !> Line number of message
    integer :: line
    !> First and last character of message
    integer :: first, last
    !> Message text
    character(len=:), allocatable :: text
    !> Identifier of context
    character(len=:), allocatable :: source
  end type label_type

  interface label_type
    module procedure new_label
  end interface label_type


  !> Definition of diagnostic message
  type :: lox_diagnostic
    !> Level of message
    type(level_enum) :: level
    !> Primary message
    character(len=:), allocatable :: message
    !> Context of the diagnostic source
    character(len=:), allocatable :: source
    !> Messages associated with this diagnostic
    type(label_type), allocatable :: label(:)
    !> Additional diagnostic information
    type(lox_diagnostic), allocatable :: sub(:)
  end type lox_diagnostic

  interface lox_diagnostic
    module procedure new_diagnostic
  end interface lox_diagnostic


  type :: line_token
    integer :: first, last
  end type line_token

  character(len=*), parameter :: nl = new_line('a')


contains


  function new_label(level, text, line, first, last, primary) result(new)
    type(level_enum), intent(in) :: level
    character(len=*), intent(in), optional :: text
    integer, intent(in) :: line, first, last
    logical, intent(in), optional :: primary
    type(label_type) :: new

    if (present(text)) new%text = text
    new%level = level
    new%line = line
    new%first = first
    new%last = last
    if (present(primary)) then
      new%primary = primary
    else
      new%primary = .false.
    end if
  end function new_label


  !> Create new diagnostic message
  function new_diagnostic(level, message, source, label, diagnostic) result(new)
    !> Level of message
    type(level_enum), intent(in) :: level
    !> Primary message
    character(len=*), intent(in), optional :: message
    !> Context of the diagnostic source
    character(len=*), intent(in), optional :: source
    !> Messages associated with this diagnostic
    type(label_type), intent(in), optional :: label(:)
    !> Additional diagnostic information
    type(lox_diagnostic), intent(in), optional :: diagnostic(:)
    type(lox_diagnostic) :: new

    new%level = level
    if (present(message)) new%message = message
    if (present(source)) new%source = source
    if (present(label)) new%label = label
    if (present(diagnostic)) new%sub = diagnostic
  end function new_diagnostic


  pure function line_tokens(input) result(token)
    character(len=*), intent(in) :: input
    type(line_token), allocatable :: token(:)

    integer :: first, last

    first = 1
    last = 0
    allocate(token(0))
    do while (first <= len(input))
      last = index(input(first+1:), nl) + first - 1
      if (last < first) then
        last = len(input)
      end if

      token = [token, line_token(first, last)]

      first = last + (1 + len(nl))
    end do
  end function line_tokens

  pure recursive function render_diagnostic(diag, input, color) result(string)
    character(len=*), intent(in) :: input
    type(lox_diagnostic), intent(in) :: diag
    type(lox_terminal), intent(in) :: color
    character(len=:), allocatable :: string

    integer :: is

    string = &
      render_message(diag%level, diag%message, color)

    if (allocated(diag%label)) then
      string = string // nl // &
        render_text_with_labels(input, diag%label, color, source=diag%source)
    end if

    if (allocated(diag%sub)) then
      do is = 1, size(diag%sub)
        string = string // nl // &
          render_diagnostic(diag%sub(is), input, color)
      end do
    end if

  end function render_diagnostic

  pure function render_message(level, message, color) result(string)
    type(level_enum), intent(in) :: level
    character(len=*), intent(in), optional :: message
    type(lox_terminal), intent(in) :: color
    character(len=:), allocatable :: string

    if (present(message)) then
      string = &
        level_name(level, color) // color%bold // ": " // message // color%reset
    else
      string = &
        level_name(level, color)
    end if
  end function render_message

  pure function level_name(level, color) result(string)
    type(level_enum), intent(in) :: level
    type(lox_terminal), intent(in) :: color
    character(len=:), allocatable :: string

    select case(level%id)
    case(level_error%id)
      string = color%bold_red // "error" // color%reset
    case(level_syntax_error%id, level_eof_error%id)
      string = color%bold_red // "syntax error" // color%reset
    case(level_semantic_error%id)
      string = color%bold_red // "semantic error" // color%reset
    case(level_runtime_error%id)
      string = color%bold_red // "runtime error" // color%reset
    case(level_warning%id)
      string = color%bold_yellow // "warning" // color%reset
    case(level_help%id)
      string = color%bold_cyan // "help" // color%reset
    case(level_note%id)
      string = color%bold_blue // "note" // color%reset
    case(level_info%id)
      string = color%bold_magenta // "info" // color%reset
    case default
      string = color%bold_blue // "unknown" // color%reset
    end select
  end function level_name

  pure function render_source(source, offset, color) result(string)
    character(len=*), intent(in) :: source
    integer, intent(in) :: offset
    type(lox_terminal), intent(in) :: color
    character(len=:), allocatable :: string

    string = &
      & repeat(" ", offset) // color%bold_blue // "-->" // color%reset // " " // source
  end function render_source

  pure function render_text(input, color, source) result(string)
    character(len=*), intent(in) :: input
    type(lox_terminal), intent(in) :: color
    character(len=*), intent(in), optional :: source
    character(len=:), allocatable :: string

    integer :: it, offset
    type(line_token), allocatable :: token(:)

    token = line_tokens(input)
    offset = integer_width(size(token))

    if (present(source)) then
      string = render_source(source, offset, color) // nl // &
        & repeat(" ", offset + 1) // color%bold_blue // "|" // color%reset
    else
      string = &
        & repeat(" ", offset + 1) // color%bold_blue // "|" // color%reset
    end if

    do it = 1, size(token)
      string = string // nl //&
        & render_line(input(token(it)%first:token(it)%last), to_string(it, offset), color)
    end do
    string = string // nl // &
      repeat(" ", offset + 1) // color%bold_blue // "|" // color%reset

  end function render_text

  pure function render_text_with_label(input, label, color, source) result(string)
    character(len=*), intent(in) :: input
    type(label_type), intent(in) :: label
    type(lox_terminal), intent(in) :: color
    character(len=*), intent(in), optional :: source
    character(len=:), allocatable :: string

    integer :: it, offset, first, last
    type(line_token), allocatable :: token(:)

    token = line_tokens(input)
    first = max(1, label%line - 1)
    last = min(size(token), label%line + 1)
    offset = integer_width(last)

    if (present(source)) then
      string = render_source(source, offset, color) // ":" // &
        & to_string(label%line) // ":" // &
        & to_string(label%first) // "-" // to_string(label%last) // nl // &
        & repeat(" ", offset + 1) // color%bold_blue // "|" // color%reset
    else
      string = &
        & repeat(" ", offset + 1) // color%bold_blue // "|" // color%reset
    end if

    do it = first, last
      string = string // nl //&
        & render_line(input(token(it)%first:token(it)%last), &
        &             to_string(it, offset), color)
      if (it == label%line) then
        string = string // nl //&
          & repeat(" ", offset + 1) // color%bold_blue // "|" // color%reset // &
          & render_label(label, color)
      end if
    end do
    string = string // nl // &
      repeat(" ", offset + 1) // color%bold_blue // "|" // color%reset

  end function render_text_with_label

  pure function render_text_with_labels(input, label, color, source) result(string)
    character(len=*), intent(in) :: input
    type(label_type), intent(in) :: label(:)
    type(lox_terminal), intent(in) :: color
    character(len=*), intent(in), optional :: source
    character(len=:), allocatable :: string

    integer :: it, il, offset, first, last
    type(line_token), allocatable :: token(:)
    logical, allocatable :: display(:)

    token = line_tokens(input)
    first = max(1, minval(label%line) - 1)
    last = min(size(token), maxval(label%line) + 1)
    offset = integer_width(last)

    it = 1  ! Without a primary we use the first label
    do il = 1, size(label)
      if (label(il)%primary) then
        it = il
        exit
      end if
    end do

    if (present(source)) then
      string = render_source(source, offset, color) // ":" // &
        & to_string(label(it)%line) // ":" // &
        & to_string(label(it)%first) // "-" // to_string(label(it)%last) // nl // &
        & repeat(" ", offset + 1) // color%bold_blue // "|" // color%reset
    else
      string = &
        & repeat(" ", offset + 1) // color%bold_blue // "|" // color%reset
    end if

    allocate(display(first:last), source=.false.)
    do il = 1, size(label)
      display(max(first, label(il)%line - 1):min(last, label(il)%line + 1)) = .true.
    end do

    do it = first, last
      if (.not.display(it)) then
        if (display(it-1)) then
          string = string // nl //&
            & repeat(" ", offset + 1) // color%bold_blue // ":" // color%reset
        end if
        cycle
      end if

      string = string // nl //&
        & render_line(input(token(it)%first:token(it)%last), &
        &             to_string(it, offset), color)
      if (any(it == label%line)) then
        do il = 1, size(label)
          if (label(il)%line /= it) cycle
          string = string // nl //&
            & repeat(" ", offset + 1) // color%bold_blue // "|" // color%reset // &
            & render_label(label(il), color)
        end do
      end if
    end do
    string = string // nl // &
      repeat(" ", offset + 1) // color%bold_blue // "|" // color%reset

  end function render_text_with_labels

  pure function render_label(label, color) result(string)
    type(label_type), intent(in) :: label
    type(lox_terminal), intent(in) :: color
    character(len=:), allocatable :: string

    integer :: width
    character :: marker
    character(len=:), allocatable :: this_color

    marker = merge("^", "-", label%primary)
    width = label%last - label%first + 1
    this_color = level_color(label%level, color)

    string = &
      & repeat(" ", label%first) // this_color // repeat(marker, width) // color%reset
    if (allocated(label%text)) then
      string = string // &
        & " " // this_color // label%text // color%reset
    end if

  end function render_label

  pure function level_color(level, color) result(this_color)
    type(level_enum), intent(in) :: level
    type(lox_terminal), intent(in) :: color
    character(len=:), allocatable :: this_color

    select case(level%id)
    case(level_error%id, level_syntax_error%id, level_eof_error%id, &
        & level_semantic_error%id, level_runtime_error%id)
      this_color = color%bold_red
    case(level_warning%id)
      this_color = color%bold_yellow
    case(level_help%id)
      this_color = color%bold_cyan
    case(level_info%id)
      this_color = color%bold_magenta
    case default
      this_color = color%bold_blue
    end select
  end function level_color

  pure function render_line(input, line, color) result(string)
    character(len=*), intent(in) :: input
    character(len=*), intent(in) :: line
    type(lox_terminal), intent(in) :: color
    character(len=:), allocatable :: string

    string = &
      & line // " " // color%bold_blue // "|" // color%reset // " " // input
  end function render_line

  pure function integer_width(input) result(width)
    integer, value :: input
    integer :: width

    width = 0
    do while (input /= 0)
      input = input / 10
      width = width + 1
    end do

  end function integer_width

  !> Represent an integer as character sequence.
  pure function to_string(val, width) result(string)
    integer, intent(in) :: val
    integer, intent(in), optional :: width
    character(len=:), allocatable :: string
    integer, parameter :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer
    integer :: pos
    integer :: n
    character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

    if (val == 0) then
      string = numbers(0)
      return
    end if

    n = abs(val)
    buffer = ""

    pos = buffer_len + 1
    do while (n > 0)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10))
      n = n/10
    end do
    if (val < 0) then
      pos = pos - 1
      buffer(pos:pos) = '-'
    end if

    if (present(width)) then
      string = repeat(" ", max(width-(buffer_len+1-pos), 0)) // buffer(pos:)
    else
      string = buffer(pos:)
    end if
  end function to_string


  elemental function op_equal(left, right) result(equal)
    type(level_enum), intent(in) :: left
    type(level_enum), intent(in) :: right
    logical :: equal

    equal = left%id == right%id
  end function op_equal


end module flox_diagnostic
