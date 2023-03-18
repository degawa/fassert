program logical
    use :: fassert
    implicit none

    logical :: stat

    call expect_equal(.true., .true., "result should be true", stat)
    call expect_equal(.false., .true., "result should be true", stat)
    !*
    !```Fortran
    ! call expect_equal(.true., .true., "result should be true", stat)
    ! ! PASSED: result should be true
    !
    ! call expect_equal(.false., .true., "result should be true", stat)
    ! ! FAILED: rresult should be true
    ! !     Expected: T
    ! !     Actual  : F
    !```

    call expect_equal(.false., .false., "result should be false", stat)
    call expect_equal(.true., .false., "result should be false", stat)
    !*
    !```Fortran
    ! call expect_equal(.false., .false., "result should be false", stat)
    ! ! PASSED: result should be false
    !
    ! call expect_equal(.true., .false., "result should be false", stat)
    ! ! FAILED: result should be false
    ! !     Expected: F
    ! !     Actual  : T
    !```

    call expect_true(.true., "result should be true", stat)
    call expect_true(.false., "result should be true", stat)
    !*
    !```Fortran
    ! call expect_true(.true., "result should be true", stat)
    ! ! PASSED: result should be true
    !
    ! call expect_true(.false., "result should be true", stat)
    ! ! FAILED: rresult should be true
    ! !     Expected: T
    ! !     Actual  : F
    !```

    call expect_true(.false., "result should not be true", stat, expected_failure=.true.)
    call expect_true(.true., "result should not be true", stat, expected_failure=.true.)
    !*
    !```Fortran
    ! call expect_true(.false., "result should not be true", stat, expected_failure=.true.)
    ! ! PASSED: result should not be true [expected failure]
    !
    ! call expect_true(.true., "result should not be true", stat, expected_failure=.true.)
    ! ! FAILED: result should not be true [unexpected pass]
    ! !     Expected: F
    ! !     Actual  : T
    !```

    call expect_false(.false., "result should be false", stat)
    call expect_false(.true., "result should be false", stat)
    !*
    !```Fortran
    ! call expect_false(.false., "result should be false", stat)
    ! ! PASSED: result should be false
    !
    ! call expect_false(.true., "result should be false", stat)
    ! ! FAILED: result should be false
    ! !     Expected: F
    ! !     Actual  : T
    !```

    call expect_false(.true., "result should not be false", stat, expected_failure=.true.)
    call expect_false(.false., "result should not be false", stat, expected_failure=.true.)
    !*
    !```Fortran
    ! call expect_false(.true., "result should not be false", stat, expected_failure=.true.)
    ! ! PASSED: result should not be false [expected failure]
    !
    ! call expect_false(.false., "result should not be false", stat, expected_failure=.true.)
    ! ! FAILED: result should not be false [unexpected pass]
    ! !     Expected: T
    ! !     Actual  : F
    !```

end program logical
