program array_int
    use, intrinsic :: iso_fortran_env
    use :: fassert
    implicit none

    logical :: stat
    integer(int64) :: a(2), b(2), c(3), d(2)

    a = 1

    !------------------------------------------------------------------!
    b = 1
    print *, "v--"
    call expect_equal(a, b, "[1 1] == [1 1]", stat)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[1 1] == [1 1]", stat)
    ! ! PASSED: [1 1] == [1 1]
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.true.)
    ! ! PASSED: [1 1] == [1 1]
    ! !     Maximum Absolute Difference: 0 at (1)
    ! !     Minimum Absolute Difference: 0 at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.true.)
    ! ! PASSED: [1 1] == [1 1]
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.true., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.true., quiet=.false.
    ! ! PASSED: [1 1] == [1 1]
    ! !     Maximum Absolute Difference: 0 at (1)
    ! !     Minimum Absolute Difference: 0 at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.true., quiet=.true.)
    ! !     Maximum Absolute Difference: 0 at (1)
    ! !     Minimum Absolute Difference: 0 at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.false., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.false., quiet=.false.)
    ! ! PASSED: [1 1] == [1 1]
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, b, "[1 1] == [1 1]", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^--"

    !------------------------------------------------------------------!
    c = 1
    print *, "v--"
    call expect_equal(a, c, "[1 1] == [1 1 1]", stat)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[1 1] == [1 1 1]", stat)
    ! ! FAILED: [1 1] == [1 1 1] [shape check]
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.true.)
    ! ! FAILED: [1 1] == [1 1 1] [shape check]
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.false.)
    ! ! FAILED: [1 1] == [1 1 1] [shape check]
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.true., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.true., quiet=.false.)
    ! ! FAILED: [1 1] == [1 1 1] [shape check]
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.true., quiet=.true.)
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.false., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.false., quiet=.false.)
    ! ! FAILED: [1 1] == [1 1 1] [shape check]
    ! !     Expected Shape: (3)
    ! !     Actual Shape  : (2)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, c, "[1 1] == [1 1 1]", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^--"

    !------------------------------------------------------------------!
    d = 2
    print *, "v--"
    call expect_equal(a, d, "[1 1] == [2 2]", stat)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[1 1] == [2 2]", stat)
    ! ! FAILED: [1 1] == [2 2]
    ! !     Maximum Absolute Difference: 1 at (1)
    ! !     Minimum Absolute Difference: 1 at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.true.)
    ! ! FAILED: [1 1] == [2 2]
    ! !     Maximum Absolute Difference: 1 at (1)
    ! !     Minimum Absolute Difference: 1 at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.false.)
    ! ! FAILED: [1 1] == [2 2]
    ! !     Maximum Absolute Difference: 1 at (1)
    ! !     Minimum Absolute Difference: 1 at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.true., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.true., quiet=.false.)
    ! ! FAILED: [1 1] == [2 2]
    ! !     Maximum Absolute Difference: 1 at (1)
    ! !     Minimum Absolute Difference: 1 at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.true., quiet=.true.)
    ! !     Maximum Absolute Difference: 1 at (1)
    ! !     Minimum Absolute Difference: 1 at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.false., quiet=.false.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.false., quiet=.false.)
    ! ! FAILED: [1 1] == [2 2]
    ! !     Maximum Absolute Difference: 1 at (1)
    ! !     Minimum Absolute Difference: 1 at (1)
    !```
    print *, "^--"

    print *, "v--"
    call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(a, d, "[1 1] == [2 2]", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^--"

end program array_int
