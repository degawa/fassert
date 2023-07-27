module test_common_floatingPointNumber_unitTests_real32
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_floatingPointNumber_real32
    use :: fassert_common_userSpecified, only:ULP
    use :: strings_enclose
    use :: strith, to_str => to_string
    implicit none
    private
    public :: is_distance_less_than_n_ulp_real32_returns_true
    public :: is_distance_less_than_n_ulp_real32_returns_false
    public :: sign_real32_returns_0_when_input_positive_number
    public :: sign_real32_returns_1_when_input_negative_number

contains
    subroutine is_distance_less_than_n_ulp_real32_returns_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: f, g
        integer(int32) :: a, b

        f = 67329.234
        g = f + spacing(f)
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 1199800478  1199800479
        ! print *, f, g ! 67329.2344       67329.2422
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = 67329.234
        g = f - spacing(f)
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 1199800478  1199800477
        ! print *, f, g ! 67329.2344       67329.2266
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = huge(f)
        g = f - spacing(f)
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 2139095039  2139095038
        ! print *, f, g ! 3.40282347E+38   3.40282326E+38
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = 1.17549435e-38
        g = 1.17549449e-38
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 8388608     8388609
        ! print *, f, g ! 1.17549435E-38   1.17549449E-38
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return
    end subroutine is_distance_less_than_n_ulp_real32_returns_true

    subroutine is_distance_less_than_n_ulp_real32_returns_false(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: f, g
        integer(int32) :: a, b

        f = tiny(f)
        g = f*2. ! result of f + spacing(f) is compiler dependent
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 8388608    16777216
        ! print *, f, g ! 1.17549435E-38   2.35098870E-38
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .false., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are not within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = 1.
        g = -1.
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 1065353216 -1082130432
        ! print *, f, g ! 1.00000000      -1.00000000
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .false., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are not within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = huge(f)
        g = -huge(g)
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 2139095039    -8388609
        ! print *, f, g ! 3.40282347E+38  -3.40282347E+38
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .false., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are not within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return
    end subroutine is_distance_less_than_n_ulp_real32_returns_false

    subroutine sign_real32_returns_0_when_input_positive_number(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: f
        f = tiny(f)
        call check(error, sign(f) == 0, "sign bit of "//to_string(f)//" is 0")
        if (occurred(error)) return

        f = 1e0
        call check(error, sign(f) == 0, "sign bit of "//to_string(f)//" is 0")
        if (occurred(error)) return

        f = huge(f)
        call check(error, sign(f) == 0, "sign bit of "//to_string(f)//" is 0")
        if (occurred(error)) return

        f = 0e0
        call check(error, sign(f) == 0, "sign bit of "//to_string(f)//" is 0")
        if (occurred(error)) return
    end subroutine sign_real32_returns_0_when_input_positive_number

    subroutine sign_real32_returns_1_when_input_negative_number(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: f
        f = -tiny(f)
        call check(error, sign(f) == 1, "sign bit of "//to_string(f)//" is 1")
        if (occurred(error)) return

        f = -1e0
        call check(error, sign(f) == 1, "sign bit of "//to_string(f)//" is 1")
        if (occurred(error)) return

        f = -huge(f)
        call check(error, sign(f) == 1, "sign bit of "//to_string(f)//" is 1")
        if (occurred(error)) return

        f = -0e0
        call check(error, sign(f) == 1, "sign bit of "//to_string(f)//" is 1")
        if (occurred(error)) return
    end subroutine sign_real32_returns_1_when_input_negative_number
end module test_common_floatingPointNumber_unitTests_real32
