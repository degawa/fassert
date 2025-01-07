program same_type
    use, intrinsic :: iso_fortran_env
    use :: fassert
    implicit none

    type :: a_parent_type
    end type a_parent_type

    type, extends(a_parent_type) :: an_extended_type
    end type an_extended_type

    type, extends(a_parent_type) :: another_extended_type
    end type another_extended_type

    logical :: stat

    call expect_same_type(1_int32, 2_int32, "1_int32 is the same type as 2_int32", stat)

    call expect_same_type(1.0_real32, 2.0_real32, "1_real32 is the same type as 2_real32", stat)

    call expect_same_type(1_int8, 1_int16, "1_int8 is the same type as 1_int16", stat)

    call expect_same_type(1_int8, 1_int16, "1_int8 is not the same type as 1_int16", stat, expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_same_type(1_int32, 2_int32, "1_int32 is the same type as 2_int32", stat)
    ! ! PASSED: 1_int32 is the same type as 2_int32
    !
    ! call expect_same_type(1.0_real32, 2.0_real32, "1_real32 is the same type as 2_real32", stat)
    ! ! PASSED: 1_real32 is the same type as 2_real32
    !
    ! call expect_same_type(1_int8, 1_int16, "1_int8 is the same type as 1_int16", stat)
    ! ! FAILED: 1_int8 is the same type as 1_int16
    !
    ! call expect_same_type(1_int8, 1_int16, "1_int8 is not the same type as 1_int16", stat, expected_failure=.true.)
    ! ! PASSED: 1_int8 is not the same type as 1_int16 [expected failure]
    !```

    block
        type(a_parent_type) :: a, b
        type(an_extended_type) :: c, d
        type(another_extended_type) :: e, f

        call expect_same_type(a, b, "a is the same type as b", stat)

        call expect_same_type(c, d, "c is the same type as d", stat)

        call expect_same_type(e, f, "e is the same type as f", stat)
        !|
        !```Fortran
        ! call expect_same_type(a, b, "a is the same type as b", stat)
        ! ! PASSED: a is the same type as b
        !
        ! call expect_same_type(c, d, "c is the same type as d", stat)
        ! ! PASSED: c is the same type as d
        !
        ! call expect_same_type(e, f, "e is the same type as f", stat)
        ! ! PASSED: e is the same type as f
        !```

        call expect_same_type(c, a, "c is not the same type as a", stat, expected_failure=.true.)

        call expect_same_type(e, a, "e is not the same type as a", stat, expected_failure=.true.)

        call expect_same_type(d, f, "d is not the same type as f", stat, expected_failure=.true.)
        !|
        !```Fortran
        ! call expect_same_type(c, a, "c is not the same type as a", stat, expected_failure=.true.)
        ! ! PASSED: c is not the same type as a [expected failure]
        !
        ! call expect_same_type(e, a, "e is not the same type as a", stat, expected_failure=.true.)
        ! ! PASSED: e is not the same type as a [expected failure]
        !
        ! call expect_same_type(d, f, "d is not the same type as f", stat, expected_failure=.true.)
        ! ! PASSED: d is not the same type as f [expected failure]
        !```
    end block
end program same_type
