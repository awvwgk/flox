! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module flox_constants
  implicit none
  private

  public :: i8

  integer, parameter :: i8 = selected_int_kind(18)
end module flox_constants
