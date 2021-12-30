! This file is part of flox.
! SPDX-Identifier: Apache-2.0

module test_scanner
  use testdrive, only : unittest_type, new_unittest, error_type, check
  implicit none
  private

  public :: collect_scanner

contains

!> Collect all exported unit tests
subroutine collect_scanner(testsuite)
   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
     new_unittest("...", test_) &
     ]
end subroutine collect_scanner

subroutine test_(error)
  !> Error handling
  type(error_type), allocatable, intent(out) :: error

end subroutine test_

end module test_scanner
