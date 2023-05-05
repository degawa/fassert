program array_false
    use, intrinsic :: iso_fortran_env
    use :: fassert
    implicit none

    logical :: stat
    logical :: a(2), b(2)

    a = .false.
    b = .true.

    !------------------------------------------------------------------!
    print *, "v--"
    call expect_false(a, "all false", stat)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat)
    ! ! PASSED: all false
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.true.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.true.)
    ! ! PASSED: all false
    ! !     All elements are false.
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.false.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.false.)
    ! ! PASSED: all false
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.true., quiet=.false.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.true., quiet=.false.)
    ! ! PASSED: all false
    ! !     All elements are false.
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.true., quiet=.true.)
    ! !     All elements are false.
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.false., quiet=.false.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.false., quiet=.false.)
    ! ! PASSED: all false
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^--"

    !------------------------------------------------------------------!
    print *, "v--"
    call expect_false(a, "all false", stat, expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, expected_failure=.true.)
    ! ! FAILED: all false [unexpected pass]
    ! !     All elements are false.
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.true., expected_failure=.true.)
    ! ! FAILED: all false [unexpected pass]
    ! !     All elements are false.
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.false., expected_failure=.true.)
    ! ! FAILED: all false [unexpected pass]
    ! !     All elements are false.
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.true., quiet=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.true., quiet=.false., expected_failure=.true.)
    ! ! FAILED: all false [unexpected pass]
    ! !     All elements are false.
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.true., quiet=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.true., quiet=.true., expected_failure=.true.)
    ! !     All elements are false.
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.false., quiet=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.false., quiet=.false., expected_failure=.true.)
    ! ! FAILED: all false [unexpected pass]
    ! !     All elements are false.
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(a, "all false", stat, verbose=.false., quiet=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(a, "all false", stat, verbose=.false., quiet=.true., expected_failure=.true.)
    !```
    print *, "^--"

    !------------------------------------------------------------------!
    print *, "v--"
    call expect_false(b, "all false", stat)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat)
    ! ! FAILED: all false
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.true.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.true.)
    ! ! FAILED: all false
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.false.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.false.)
    ! ! FAILED: all false
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.true., quiet=.false.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.true., quiet=.false.)
    ! ! FAILED: all false
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.true., quiet=.true.)
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.false., quiet=.false.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.false., quiet=.false.)
    ! ! FAILED: all false
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^--"

    !------------------------------------------------------------------!
    print *, "v--"
    call expect_false(b, "all false", stat, expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, expected_failure=.true.)
    ! ! PASSED: all false [expected failure]
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.true., expected_failure=.true.)
    ! ! PASSED: all false [expected failure]
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.false., expected_failure=.true.)
    ! ! PASSED: all false [expected failure]
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.true., quiet=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.true., quiet=.false., expected_failure=.true.)
    ! ! PASSED: all false [expected failure]
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.true., quiet=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.true., quiet=.true., expected_failure=.true.)
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.false., quiet=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.false., quiet=.false., expected_failure=.true.)
    ! ! PASSED: all false [expected failure]
    !```
    print *, "^--"

    print *, "v--"
    call expect_false(b, "all false", stat, verbose=.false., quiet=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_false(b, "all false", stat, verbose=.false., quiet=.true., expected_failure=.true.)
    !```
    print *, "^--"
end program array_false
