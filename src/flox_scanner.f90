! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_scanner
  use flox_diagnostic, only : lox_diagnostic, label_type, level_error
  implicit none
  private

  public :: lox_scanner, lox_token
  public :: check_token, line_descriptor

  public :: &
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, &
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR, &
    BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, &
    IDENTIFIER, STRING, NUMBER, &
    COMMENT, SPACE, NEWLINE, EOF, INVALID

  type :: lox_scanner
  contains
    procedure :: scan_tokens
  end type lox_scanner

  interface lox_scanner
    module procedure new_scanner
  end interface lox_scanner

  type :: lox_token
    integer :: first
    integer :: length
    integer :: ttype
    character(len=:), allocatable :: val
  end type lox_token

  enum, bind(c)
    enumerator :: &
      ! Single-character tokens.
      LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, &
      COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR, &

      ! One or two character tokens.
      BANG, BANG_EQUAL, &
      EQUAL, EQUAL_EQUAL, &
      GREATER, GREATER_EQUAL, &
      LESS, LESS_EQUAL, &

      ! Literals.
      IDENTIFIER, STRING, NUMBER, &

      COMMENT, SPACE, NEWLINE, EOF, INVALID
  end enum

  !> Newline character
  character(len=1), parameter :: nl = achar(10)
  !> Formfeed character is allowed in strings
  character(len=1), parameter :: formfeed = achar(12)
  !> Carriage return is allowed as part of the newline and in strings
  character(len=1), parameter :: carriage_return = achar(13)
  !> Backspace is allowed in strings
  character(len=1), parameter :: bspace = achar(8)
  !> Tabulators are allowed as whitespace and in strings
  character(len=1), parameter :: tabulator = achar(9)

  character(len=*), parameter :: &
    lowercase = "abcdefghijklmnopqrstuvwxyz", &
    uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ", &
    numbers = "0123456789"

  integer, parameter :: initial_size = 128

contains

  function new_scanner() result(new)
    type(lox_scanner) :: new
  end function new_scanner

  subroutine scan_tokens(self, source, tokens)
    class(lox_scanner), intent(inout) :: self
    character(len=*), intent(in) :: source
    type(lox_token), allocatable, intent(out) :: tokens(:)

    integer :: ntoken
    integer :: pos
    type(lox_token) :: token

    pos = 0
    tokens = [lox_token::]

    do while(pos <= len(source))
      call get_token(source, pos, token)
      tokens = [tokens, token]
    end do
  end subroutine scan_tokens

  subroutine get_token(source, pos, token)
    character(len=*), intent(in) :: source
    integer, intent(inout) :: pos
    type(lox_token), intent(out) :: token

    integer :: start
    character :: ch

    ch = peek(source, pos)
    pos = pos + 1
    start = pos
    select case(ch)
    case("(")
      token = lox_token(start, 0, LEFT_PAREN, ch)
    case(")")
      token = lox_token(start, 0, RIGHT_PAREN, ch)
    case("{")
      token = lox_token(start, 0, LEFT_BRACE, ch)
    case("}")
      token = lox_token(start, 0, RIGHT_BRACE, ch)
    case(",")
      token = lox_token(start, 0, COMMA, ch)
    case(".")
      token = lox_token(start, 0, DOT, ch)
    case("-")
      token = lox_token(start, 0, MINUS, ch)
    case("+")
      token = lox_token(start, 0, PLUS, ch)
    case(";")
      token = lox_token(start, 0, SEMICOLON, ch)
    case("*")
      token = lox_token(start, 0, STAR, ch)

    case("!")
      if (peek(source, pos) == "=") then
        pos = pos + 1
        token = lox_token(start, 1, BANG_EQUAL, source(start:pos))
      else
        token = lox_token(start, 0, BANG, ch)
      end if

    case("=")
      if (peek(source, pos) == "=") then
        pos = pos + 1
        token = lox_token(start, 1, EQUAL_EQUAL, source(start:pos))
      else
        token = lox_token(start, 0, EQUAL, ch)
      end if

    case("<")
      if (peek(source, pos) == "=") then
        pos = pos + 1
        token = lox_token(start, 1, LESS_EQUAL, source(start:pos))
      else
        token = lox_token(start, 0, LESS, ch)
      end if

    case(">")
      if (peek(source, pos) == "=") then
        pos = pos + 1
        token = lox_token(start, 1, GREATER_EQUAL, source(start:pos))
      else
        token = lox_token(start, 0, GREATER, ch)
      end if

    case("/")
      if (peek(source, pos) == "/") then
        start = pos
        do while (peek(source, pos) /= nl)
          pos = pos + 1
        end do
        token = lox_token(start, pos - start, COMMENT)
      else
        token = lox_token(start, 0, SLASH, ch)
      end if

    case(" ", formfeed, carriage_return, bspace, tabulator)
      start = pos
      do while(any(peek(source, pos) == [" ", formfeed, carriage_return, bspace, tabulator]))
        pos = pos + 1
      end do
      token = lox_token(start, pos - start, SPACE)

    case(nl)
      token = lox_token(start, -1, NEWLINE)

    case("""")
      do while(peek(source, pos) /= """")
        pos = pos + 1
      end do
      pos = pos + 1
      token = lox_token(start, pos - start, STRING, source(start:pos))

    case("0":"9")
      start = pos
      do while(is_number(peek(source, pos)))
        pos = pos + 1
      end do
      if (peek(source, pos) == "." .and. is_number(peek(source, pos+1))) then
        pos = pos + 1
        do while(is_number(peek(source, pos)))
          pos = pos + 1
        end do
      end if
      token = lox_token(start, pos - start, NUMBER, source(start:pos))

    case("a":"z", "A":"Z")
      start = pos
      do while(is_alphanumeric(peek(source, pos)))
        pos = pos + 1
      end do
      token = lox_token(start, pos - start, IDENTIFIER, source(start:pos))

    case default
      token = lox_token(start, -1, INVALID)

    end select
  end subroutine get_token

  pure function peek(source, pos)
    character(len=*), intent(in) :: source
    integer, intent(in) :: pos
    character :: peek

    if (pos + 1 <= len(source)) then
      peek = source(pos+1:pos+1)
    else
      peek = nl
    end if
  end function peek

  elemental function is_number(ch)
    character(len=*), intent(in) :: ch
    logical :: is_number

    is_number = verify(ch, numbers) == 0
  end function

  elemental function is_alphanumeric(ch)
    character(len=*), intent(in) :: ch
    logical :: is_alphanumeric

    is_alphanumeric = verify(ch, lowercase//uppercase//numbers//"_") == 0
  end function

  !> Reallocate list of tokens
  subroutine resize(list, n)
    !> Instance of the array to be resized
    type(lox_token), allocatable, intent(inout) :: list(:)
    !> Dimension of the final array size
    integer, intent(in), optional :: n

    type(lox_token), allocatable :: tmp(:)
    integer :: this_size, new_size

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
      list(:this_size) = tmp(:this_size)
      deallocate(tmp)
    end if
  end subroutine resize

  subroutine check_token(token, diagnostic)
    !> Token to be checked
    type(lox_token), intent(in) :: token
    !> Diagnostic for this token
    type(lox_diagnostic), allocatable, intent(out) :: diagnostic

    integer :: line, first, last
    character(len=*), parameter :: message = "Unexpected character found in input", &
      label = "invalid in this context"

    if (token%ttype == INVALID) then
      call line_descriptor(token, line, first, last)
      diagnostic = lox_diagnostic(level_error, message, &
        & label=[label_type(level_error, label, line, first, last, .true.)])
    end if
  end subroutine check_token

  subroutine line_descriptor(token, line, first, last)
    !> Token to be checked
    type(lox_token), intent(in) :: token

    integer, intent(out) :: line, first, last

    line = 1
    first = token%first
    last = token%first + token%length + 1
  end subroutine line_descriptor

end module flox_scanner
