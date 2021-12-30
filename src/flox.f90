! This file is part of flox.
! SPDX-Identifier: Apache-2.0

!> Main module of the Fortran lox interpreter
module flox
  use, intrinsic :: iso_fortran_env, only : output_unit
  use stdlib_io, only : getline
  use stdlib_strings, only : to_string
  use flox_diagnostic, only : lox_diagnostic, render, level_eof_error, operator(==)
  use flox_ast, only : lox_ast
  use flox_ast_printer, only : lox_ast_printer
  use flox_interpreter, only : lox_interpreter, lox_object, repr
  use flox_parser, only : lox_parser
  use flox_scanner, only : lox_scanner, lox_token, check_token
  use flox_terminal, only : lox_terminal
  use flox_timer, only : lox_timer, format_time, tp
  implicit none
  private

  public :: run_file, run_prompt

  type :: lox
    type(lox_interpreter) :: interpreter
    type(lox_timer) :: timer
    integer :: verbosity = 2
  contains
    procedure :: run
  end type lox

contains

  !> Entry point for running a lox script from a file
  subroutine run_file(filename, terminal)
    !> Name of the file containing the lox script
    character(len=*), intent(in) :: filename
    !> Instance of the terminal output
    type(lox_terminal), intent(in) :: terminal

    character(len=:), allocatable :: input
    type(lox_diagnostic), allocatable :: diagnostic(:)
    class(lox_object), allocatable :: object
    type(lox) :: instance

    call read_file(filename, input)
    call instance%run(input, object, diagnostic)
  end subroutine run_file

  !> Entry point for running the lox interpreter interactively
  subroutine run_prompt(terminal)
    !> Instance of the terminal output
    type(lox_terminal), intent(in) :: terminal

    character(len=:), allocatable :: input, source, label
    type(lox_diagnostic), allocatable :: diagnostic(:)
    class(lox_object), allocatable :: object
    type(lox) :: instance
    integer :: stat, line, i
    logical :: more

    call instance%timer%push("total")

    more = .false.
    source = ""
    line = 0
    interactive: do
      if (more) then
        write(output_unit, '(*(a))', advance='no') &
          terminal%bold_blue // repeat(".", len(label) + 1) // &
          terminal%reset // " "
      else
        line = line + 1
        label = to_string(line)
        write(output_unit, '(*(a))', advance='no') &
          terminal%bold_blue // to_string(line) // &
          terminal%bold // ">" // &
          terminal%reset // " "
      end if
      call getline(input, stat)
      if (stat /= 0) exit interactive
      source = source // " " // input

      call instance%timer%push("run")
      call instance%run(source, object, diagnostic)
      call instance%timer%pop
      if (allocated(diagnostic)) then
        more = all(diagnostic%level == level_eof_error)
        if (more) cycle

        do i = 1, size(diagnostic)
          write(output_unit, '(a)') render(diagnostic(i), source, terminal)
        end do
      else if (allocated(object)) then
        write(output_unit, '(a)') &
          terminal%bold_green // "=>" // &
          terminal%reset // " " // &
          terminal%bold // repr(object) // &
          terminal%reset
      end if
      more = .false.
      source = ""
    end do interactive
    write(output_unit, '(a)')

    call instance%timer%pop
    time: block
      integer :: it
      real(tp) :: ttime, rtime, stime
      character(len=*), parameter :: label(*) = [character(len=20):: &
        & "scan", "parse", "interpret"]
      if (instance%verbosity > 0) then
        ttime = instance%timer%get("total")
        rtime = instance%timer%get("run")
        if (rtime <= 1.0e-3_tp) exit time
        write(output_unit, '(a)') "", &
          & " total:"//repeat(" ", 16)//format_time(ttime), &
          & "  user:"//repeat(" ", 16)//format_time(ttime-rtime), &
          & "system:"//repeat(" ", 16)//format_time(rtime)
      end if
      if (instance%verbosity > 1) then
        do it = 1, size(label)
          stime = instance%timer%get(label(it))
          if (stime <= epsilon(0.0_tp)) cycle
          write(output_unit, '(a)') &
            & " - "//label(it)//format_time(stime) &
            & //" ("//to_string(int(stime/rtime*100), '(i3)')//"%)"
        end do
      end if
    end block time
  end subroutine run_prompt

  !> Entry point of the lox interpreter
  subroutine run(self, source, object, diagnostic)
    !> Instance of the lox interpreter
    class(lox), intent(inout) :: self
    !> Source code to interpret
    character(len=*), intent(in) :: source
    !> Returned object
    class(lox_object), allocatable, intent(out) :: object
    !> Generated diagnostic messages
    type(lox_diagnostic), allocatable, intent(out) :: diagnostic(:)

    type(lox_scanner) :: scanner
    type(lox_parser) :: parser
    type(lox_ast) :: ast
    type(lox_token), allocatable :: tokens(:)
    integer :: i

    call self%timer%push("scan")
    scanner = lox_scanner()
    call scanner%scan_tokens(source, tokens)
    call self%timer%pop

    call self%timer%push("parse")
    call parser%parse(tokens, ast)
    call self%timer%pop
    if (allocated(parser%diag)) then
      call move_alloc(parser%diag, diagnostic)
      return
    end if

    if (self%verbosity > 1) then
      block
        type(lox_ast_printer) :: printer
        call ast%accept(printer)
        write(output_unit, '(*(a))') "AST: ", printer%string
      end block
    end if

    call self%timer%push("interpret")
    call ast%accept(self%interpreter)
    call self%timer%pop
    if (allocated(self%interpreter%diag)) then
      call move_alloc(self%interpreter%diag, diagnostic)
      return
    end if

    if (allocated(self%interpreter%local)) then
      object = self%interpreter%local
    end if
  end subroutine run

  !> Reads a whole file into a character string
  subroutine read_file(filename, string)
    !> Name of the file to read
    character(len=*), intent(in) :: filename
    !> Character string to store the file content
    character(len=:), allocatable, intent(out) :: string

    character(len=:), allocatable :: line
    character(len=*), parameter :: nl = new_line('a')
    integer :: io, stat

    string = ""
    open(file=filename, newunit=io, status='old', iostat=stat)
    do while (stat == 0)
      call getline(io, line, stat)
      if (stat == 0) string = string // line // nl
    end do
    close(io)
    if (is_iostat_end(stat)) stat = 0
  end subroutine read_file

end module flox
