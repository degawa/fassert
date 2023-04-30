program assert_equal_complex
    use :: fassert
    implicit none

    logical :: stat

    call expect_equal(cmplx(0., 1.), cmplx(0., 1.), "result should be (0, 1)", stat)
    call expect_equal(cmplx(0., 1.), cmplx(0., 0.), "result should be (0, 0)", stat)
    !|
    !```Fortran
    ! call expect_equal(cmplx(0., 1.), cmplx(0., 1.), "result should be (0, 1)", stat)
    ! ! PASSED: result should be (0, 1)
    !
    ! call expect_equal(cmplx(0., 1.), cmplx(0., 0.), "result should be (0, 0)", stat)
    ! ! FAILED: result should be (0, 0)
    ! !     Expected: ( 0.000000E+00, 0.000000E+00)
    ! !     Actual  : ( 0.000000E+00, 0.100000E+01)
    ! !     Difference: ( 0.000000E+00,-0.100000E+01)
    !```

    call expect_equal(cmplx(2., 1.) + cmplx(1., 2.), cmplx(3., 3.), "result should be (3, 3) when input (2, 1)+(1, 2)", stat, &
                      verbose=.true.)
    call expect_equal(cmplx(2., 1.) - cmplx(1., 2.), cmplx(3., 3.), "result should be (3, 3) when input (2, 1)+(1, 2)", stat, &
                      verbose=.true.)
    !|
    !```Fortran
    ! call expect_equal(cmplx(2., 1.) + cmplx(1., 2.), cmplx(3., 3.), "result should be (3, 3) when input (2, 1)+(1, 2)", stat, verbose=.true.)
    ! ! PASSED: result should be (3, 3) when input (2, 1)+(1, 2)
    ! !     Expected: ( 0.300000E+01, 0.300000E+01)
    ! !     Actual  : ( 0.300000E+01, 0.300000E+01)
    ! !     Difference: ( 0.000000E+00, 0.000000E+00)
    !
    ! call expect_equal(cmplx(2., 1.) - cmplx(1., 2.), cmplx(3., 3.), "result should be (3, 3) when input (2, 1)+(1, 2)", stat, verbose=.true.)
    ! ! FAILED: result should be (3, 3) when input (2, 1)+(1, 2)
    ! !     Expected: ( 0.300000E+01, 0.300000E+01)
    ! !     Actual  : ( 0.100000E+01,-0.100000E+01)
    ! !     Difference: ( 0.200000E+01, 0.400000E+01)
    !```

    call expect_equal(cmplx(2., 5.) - cmplx(5., 2.), cmplx(7., 7.), "result should be (7, 7) when input (2, 5)+(5, 2)", stat, &
                      expected_failure=.true.)
    call expect_equal(cmplx(2., 5.) + cmplx(5., 2.), cmplx(7., 7.), "result should be (7, 7) when input (2, 5)+(5, 2)", stat, &
                      expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_equal(cmplx(2., 5.) - cmplx(5., 2.), cmplx(7., 7.), "result should be (y, 7) when input (2, 5)+(5, 2)", stat, expected_failure=.true.)
    ! ! PASSED: result should be (7, 7) when input (2, 5)+(5, 2) [expected failure]
    !
    ! call expect_equal(cmplx(2., 5.) + cmplx(5., 2.), cmplx(7., 7.), "result should be (y, 7) when input (2, 5)+(5, 2)", stat, expected_failure=.true.)
    ! ! FAILED: result should be (7, 7) when input (2, 5)+(5, 2) [unexpected pass]
    ! !     Expected: ( 0.700000E+01, 0.700000E+01)
    ! !     Actual  : ( 0.700000E+01, 0.700000E+01)
    ! !     Difference: ( 0.000000E+00, 0.000000E+00)
    !```

    call expect_equal(cmplx(7., 8.) + cmplx(8., 7.), cmplx(15., 15.), "it should return (15, 15) when input (7, 8)+(8, 7)", stat, &
                      quiet=.true.)
    !|
    !```Fortran
    ! expect_equal(cmplx(7., 8.) + cmplx(8., 7.), cmplx(15., 15.), "it should return (15, 15) when input (7, 8)+(8, 7)", stat, quiet=.true.)
    !```

    print *, "v-- quiet=.true. suppress assertion message output, but output expected and actual value"
    call expect_equal(cmplx(7., 8.) - cmplx(8., 7.), cmplx(15., 15.), "it should return (15, 15) when input (7, 8)+(8, 7)", stat, &
                      quiet=.true.)
    !|
    !```Fortran
    ! expect_equal(cmplx(7., 8.) - cmplx(8., 7.), cmplx(15., 15.), "it should return (15, 15) when input (7, 8)+(8, 7)", stat, quiet=.true.)
    ! !     Expected: ( 0.150000E+02, 0.150000E+02)
    ! !     Actual  : (-0.100000E+01, 0.100000E+01)
    ! !     Difference: ( 0.160000E+02, 0.140000E+02)
    !```
    print *, "^-- quiet=.true. suppress assertion message output, but output expected and actual value"

    print *, "v-- verbose=.false. and quiet=.true. suppress all message output"
    call expect_equal(cmplx(7., 8.) - cmplx(8., 7.), cmplx(15., 15.), "it should return (15, 15) when input (7, 8)+(8, 7)", stat, &
                      verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! expect_equal(cmplx(7., 8.) - cmplx(8., 7.), cmplx(15., 15.), "it should return (15, 15) when input (7, 8)+(8, 7)", stat, verbose=.false., quiet=.true.)
    !```
    print *, "^-- verbose=.false. and quiet=.true. suppress all message output"
    print *

end program assert_equal_complex
