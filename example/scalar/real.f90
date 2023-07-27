program real
    use :: fassert
    implicit none

    logical :: stat

    call expect_equal(0.+1., 1., "result should be 1 when input 0+1", stat)
    call expect_equal(0.-1., 1., "result should be 1 when input 0+1", stat)
    !|
    !```Fortran
    ! call expect_equal(0.+1., 1., "result should be 1 when input 0+1", stat)
    ! ! PASSED: result should be 1 when input 0+1
    !
    ! call expect_equal(0.-1., 1., "result should be 1 when input 0+1", stat)
    ! ! FAILED: result shoud be 1 when input 0+1
    ! !     Expected:  0.100000E+01
    ! !     Actual  : -0.100000E+01
    ! !     Difference:  0.200000E+01
    !```

    call expect_equal(1.+2., 3., "result should be 3 when input 1+2", stat, &
                      verbose=.true.)
    call expect_equal(1.-2., 3., "result should be 3 when input 1+2", stat, &
                      verbose=.true.)
    !|
    !```Fortran
    ! call expect_equal(1.+2., 3., "result should be 3 when input 1+2", stat, verbose=.true.)
    ! ! PASSED: result should be 3 when input 1+2
    ! !     Expected:  0.300000E+01
    ! !     Actual  :  0.300000E+01
    ! !     Difference:  0.000000E+00
    !
    ! call expect_equal(1.-2., 3., "result should be 3 when input 1+2", stat, verbose=.true.)
    ! ! FAILED: result should be 3 when input 1+2
    ! !     Expected:  0.300000E+01
    ! !     Actual  : -0.100000E+01
    ! !     Difference:  0.400000E+01
    !```

    call expect_equal(2.-5., 7., "result should not be 7 when input 2-5", stat, &
                      expected_failure=.true.)
    call expect_equal(2.+5., 7., "result should not be 7 when input 2-5", stat, &
                      expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_equal(2.-5., 7., "result should not be 7 when input 2-5", stat, expected_failure=.true.)
    ! ! PASSED: result should not be 7 when input 2-5 [expected failure]
    !
    ! call expect_equal(2.+5., 7., "result should not be 7 when input 2-5", stat, expected_failure=.true.)
    ! ! FAILED: result should not be 7 when input 2-5 [unexpected pass]
    ! !     Expected:  0.700000E+01
    ! !     Actual  :  0.700000E+01
    ! !     Difference:  0.000000E+00
    !```

    call expect_equal(7.+8., 15., "it should return 15 when input 7+8", stat, &
                      quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(7.+8., 15., "result should be 15 when input 7+8", stat, quiet=.true.)
    !```

    print *, "v-- quiet=.true. suppress assertion message output, but output expected and actual value"
    call expect_equal(7.-8., 15., "it should return 15 when input 7+8", stat, &
                      quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(7.-8., 15., "result should be 15 when input 7+8", stat, quiet=.true.)
    ! !     Expected:  0.150000E+02
    ! !     Actual  : -0.100000E+01
    ! !     Difference:  0.160000E+02
    !```
    print *, "^-- quiet=.true. suppress assertion message output, but output expected and actual value"

    print *, "v-- verbose=.false. and quiet=.true. suppress all message output"
    call expect_equal(7.-8., 15., "it should return 15 when input 7+8", stat, &
                      verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_equal(7.-8., 15., "result should be 15 when input 7+8", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^-- verbose=.false. and quiet=.true. suppress all message output"
    print *

end program real
