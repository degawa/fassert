module test_common_floatingPointNumber_unitTests_real64
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_floatingPointNumber_real64
    use :: fassert_common_userSpecified, only:ULP
    use :: strings_enclose
    use :: strith, to_str => to_string
    implicit none
    private
    public :: is_distance_less_than_n_ulp_real64_returns_true
    public :: is_distance_less_than_n_ulp_real64_returns_false

contains
    subroutine is_distance_less_than_n_ulp_real64_returns_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real64) :: f, g
        integer(int64) :: a, b

        f = 67329.234
        g = f + spacing(f)
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 4679363242965860352  4679363242965860353
        ! print *, f, g ! 67329.234375000000        67329.234375000015
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = 67329.234
        g = f - spacing(f)
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 4679363242965860352  4679363242965860351
        ! print *, f, g ! 67329.234375000000        67329.234374999985
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = huge(f)
        g = f - spacing(f)
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 9218868437227405311  9218868437227405310
        ! print *, f, g ! 1.7976931348623157E+308   1.7976931348623155E+308
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = 2.2250738585072014d-308
        g = 2.2250738585072019d-308
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 4503599627370496     4503599627370497
        ! print *, f, g ! 2.2250738585072014E-308   2.2250738585072019E-308
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return
    end subroutine is_distance_less_than_n_ulp_real64_returns_true

    subroutine is_distance_less_than_n_ulp_real64_returns_false(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real64) :: f, g
        integer(int64) :: a, b

        f = tiny(f)
        g = f*2d0 ! result of f + spacing(f) is compiler dependent
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 4503599627370496     9007199254740992
        ! print *, f, g ! 2.2250738585072014E-308   4.4501477170144028E-308
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .false., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are not within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = 1d0
        g = -1d0
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 4607182418800017408 -4616189618054758400
        ! print *, f, g ! 1.0000000000000000       -1.0000000000000000
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .false., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are not within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = huge(f)
        g = -huge(g)
        a = transfer(f, mold=a)
        b = transfer(g, mold=b)
        ! print *, a, b ! 9218868437227405311    -4503599627370497
        ! print *, f, g ! 1.7976931348623157E+308  -1.7976931348623157E+308
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .false., &
                   to_string(f)//enclose(to_str(a, as_unsigned=.true.)//" ULP", "(")//" and "// &
                   to_string(g)//enclose(to_str(b, as_unsigned=.true.)//" ULP", "(")//" are not within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return
    end subroutine is_distance_less_than_n_ulp_real64_returns_false
end module test_common_floatingPointNumber_unitTests_real64
