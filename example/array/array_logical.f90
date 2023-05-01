program array_logical
    use, intrinsic :: iso_fortran_env
    use :: fassert
    implicit none

    logical :: stat
    logical :: a(2), b(2), c(3), d(2)

    a = .true.

    !------------------------------------------------------------------!
    b = .true.
    print *, "v--"
    call expect_equal(a, b, "[T T] == [T T]", stat)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[T T] == [T T]", stat)
    ! ! PASSED: [T T] == [T T]
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.true.)
    ! ! PASSED: [T T] == [T T]
    ! !     All elements are equivalent.
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.true.)
    ! ! PASSED: [T T] == [T T]
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.true., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.true., quiet=.false.
    ! ! PASSED: [T T] == [T T]
    ! !     All elements are equivalent.
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.true., quiet=.true.)
    ! !     All elements are equivalent.
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.false., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.false., quiet=.false.)
    ! ! PASSED: [T T] == [T T]
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[T T] == [T T]", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^--"

    !------------------------------------------------------------------!
    c = .true.
    print *, "v--"
    call expect_equal(a, c, "[T T] == [T T T]", stat)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[T T] == [T T T]", stat)
    ! ! FAILED: [T T] == [T T T] [shape check]
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.true.)
    ! ! FAILED: [T T] == [T T T] [shape check]
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.false.)
    ! ! FAILED: [T T] == [T T T] [shape check]
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.true., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.true., quiet=.false.)
    ! ! FAILED: [T T] == [T T T] [shape check]
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.true., quiet=.true.)
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.false., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.false., quiet=.false.)
    ! ! FAILED: [T T] == [T T T] [shape check]
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[T T] == [T T T]", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^--"

    !------------------------------------------------------------------!
    d = .false.
    print *, "v--"
    call expect_equal(a, d, "[T T] == [F F]", stat)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[T T] == [F F]", stat)
    ! ! FAILED: [T T] == [F F]
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.true.)
    ! ! FAILED: [T T] == [F F]
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.false.)
    ! ! FAILED: [T T] == [F F]
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.true., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.true., quiet=.false.)
    ! ! FAILED: [T T] == [F F]
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.true., quiet=.true.)
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.false., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.false., quiet=.false.)
    ! ! FAILED: [T T] == [F F]
    ! !     Expected: F
    ! !     Actual  : T
    ! !     Position: at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[T T] == [F F]", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^--"

end program array_logical
