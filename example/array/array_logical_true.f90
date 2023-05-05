program array_true
    use, intrinsic :: iso_fortran_env
    use :: fassert
    implicit none

    logical :: stat
    logical :: a(2), b(2)

    a = .true.
    b = .false.

    !------------------------------------------------------------------!
    print *, "v--"
    call expect_true(a, "all true", stat)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat)
    ! ! PASSED: all true
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.true.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.true.)
    ! ! PASSED: all true
    ! !     All elements are true.
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.false.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.false.)
    ! ! PASSED: all true
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.true., quiet=.false.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.true., quiet=.false.)
    ! ! PASSED: all true
    ! !     All elements are true.
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.true., quiet=.true.)
    ! !     All elements are true.
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.false., quiet=.false.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.false., quiet=.false.)
    ! ! PASSED: all true
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^--"

    !------------------------------------------------------------------!
    print *, "v--"
    call expect_true(a, "all true", stat, expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, expected_failure=.true.)
    ! ! FAILED: all true [unexpected pass]
    ! !     All elements are true.
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.true., expected_failure=.true.)
    ! ! FAILED: all true [unexpected pass]
    ! !     All elements are true.
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.false., expected_failure=.true.)
    ! ! FAILED: all true [unexpected pass]
    ! !     All elements are true.
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.true., quiet=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.true., quiet=.false., expected_failure=.true.)
    ! ! FAILED: all true [unexpected pass]
    ! !     All elements are true.
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.true., quiet=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.true., quiet=.true., expected_failure=.true.)
    ! !     All elements are true.
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.false., quiet=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.false., quiet=.false., expected_failure=.true.)
    ! ! FAILED: all true [unexpected pass]
    ! !     All elements are true.
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(a, "all true", stat, verbose=.false., quiet=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(a, "all true", stat, verbose=.false., quiet=.true., expected_failure=.true.)
    !```
    print *, "^--"

    !------------------------------------------------------------------!
    print *, "v--"
    call expect_true(b, "all true", stat)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat)
    ! ! FAILED: all true
    ! !     Expected: T
    ! !     Actual  : F
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.true.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.true.)
    ! ! FAILED: all true
    ! !     Expected: T
    ! !     Actual  : F
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.false.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.false.)
    ! ! FAILED: all true
    ! !     Expected: T
    ! !     Actual  : F
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.true., quiet=.false.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.true., quiet=.false.)
    ! ! PFAILED: all true
    ! !      Expected: T
    ! !      Actual  : F
    ! !      Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.true., quiet=.true.)
    ! !     Expected: T
    ! !     Actual  : F
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.false., quiet=.false.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.false., quiet=.false.)
    ! ! FAILED: all true
    ! !     Expected: T
    ! !     Actual  : F
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^--"

    !------------------------------------------------------------------!
    print *, "v--"
    call expect_true(b, "all true", stat, expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, expected_failure=.true.)
    ! ! PASSED: all true [expected failure]
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.true., expected_failure=.true.)
    ! ! PASSED: all true [expected failure]
    ! !     Expected: T
    ! !     Actual  : F
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.false., expected_failure=.true.)
    ! ! PASSED: all true [expected failure]
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.true., quiet=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.true., quiet=.false., expected_failure=.true.)
    ! ! PASSED: all true [expected failure]
    ! !     Expected: T
    ! !     Actual  : F
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.true., quiet=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.true., quiet=.true., expected_failure=.true.)
    ! !     Expected: T
    ! !     Actual  : F
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.false., quiet=.false., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.false., quiet=.false., expected_failure=.true.)
    ! ! PASSED: all true [expected failure]
    !```
    print *, "^--"

    print *, "v--"
    call expect_true(b, "all true", stat, verbose=.false., quiet=.true., expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_true(b, "all true", stat, verbose=.false., quiet=.true., expected_failure=.true.)
    !```
    print *, "^--"
end program array_true
