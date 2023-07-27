module test_common_floatingPointNumber_unitTests_real128
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_floatingPointNumber_real128
    use :: fassert_common_userSpecified, only:ULP
    use :: fassert_common_floatingPointNumber_int128, to_str => to_string
    use :: strings_enclose
    implicit none
    private
#if !defined(NAGFOR)
    public :: is_distance_less_than_n_ulp_real128_returns_true
    public :: is_distance_less_than_n_ulp_real128_returns_false
#endif
    public :: are_close_real128_returns_T_when_within_relative_epsilon
    public :: are_close_real128_returns_F_when_not_within_relative_epsilon
    public :: sign_real128_returns_0_when_input_positive_number
    public :: sign_real128_returns_1_when_input_negative_number

contains

#if !defined(NAGFOR)
    subroutine is_distance_less_than_n_ulp_real128_returns_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: f, g
        character(:), allocatable :: a, b

        f = 67329.234
        g = f + spacing(f)
        a = to_str(as_int128(f), remove_0_padding=.true.)
        b = to_str(as_int128(g), remove_0_padding=.true.)
        ! print '(2(A:," "))', a, b ! 85148618257777126945509618650670170112 85148618257777126945509618650670170113
        ! print *, f, g ! 67329.2343750000000000000000000000000         67329.2343750000000000000000000000126
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//enclose(a//" ULP", "(")//" and "// &
                   to_string(g)//enclose(b//" ULP", "(")//" are not within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = 67329.234
        g = f - spacing(f)
        a = to_str(as_int128(f), remove_0_padding=.true.)
        b = to_str(as_int128(g), remove_0_padding=.true.)
        ! print '(2(A:," "))', a, b ! 85148618257777126945509618650670170112 85148618257777126945509618650670170111
        ! print *, f, g ! 67329.2343750000000000000000000000000         67329.2343749999999999999999999999874
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//enclose(a//" ULP", "(")//" and "// &
                   to_string(g)//enclose(b//" ULP", "(")//" are not within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = huge(f)
        g = f - spacing(f)
        a = to_str(as_int128(f), remove_0_padding=.true.)
        b = to_str(as_int128(g), remove_0_padding=.true.)
        ! print '(2(A:," "))', a, b ! 170135991163610696904058773219554885631 170135991163610696904058773219554885630
        ! print *, f, g ! 1.18973149535723176508575932662800702E+4932   1.18973149535723176508575932662800690E+4932
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//enclose(a//" ULP", "(")//" and "// &
                   to_string(g)//enclose(b//" ULP", "(")//" are not within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return
    end subroutine is_distance_less_than_n_ulp_real128_returns_true

    subroutine is_distance_less_than_n_ulp_real128_returns_false(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: f, g
        character(:), allocatable :: a, b

        f = tiny(f)
        g = f*2
        a = to_str(as_int128(f), remove_0_padding=.true.)
        b = to_str(as_int128(g), remove_0_padding=.true.)
        ! print '(2(A:," "))', a, b ! 5192296858534827628530496329220096 10384593717069655257060992658440192
        ! print *, f, g ! 3.36210314311209350626267781732175260E-4932   6.72420628622418701252535563464350521E-4932
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .false., &
                   to_string(f)//enclose(a//" ULP", "(")//" and "// &
                   to_string(g)//enclose(b//" ULP", "(")//" are within "// &
                   to_string(ULP)//" ULP")
        if (occurred(error)) return
    end subroutine is_distance_less_than_n_ulp_real128_returns_false
#endif

    subroutine are_close_real128_returns_T_when_within_relative_epsilon(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: f, g
        real(real128) :: tol

        f = 67329.234
        g = f + spacing(f)
        tol = compute_tolerance(f, g)
        ! print *, f, g ! 67329.2343750000000000000000000000000         67329.2343750000000000000000000000126
        ! print *, tol ! 2.52435489670723777731753140890491593E-0029
        call check(error, are_close(f, g, factor=ULP) .eqv. .true., &
                   to_string(f)//" and "//to_string(g)//" are within "//to_string(tol))
        if (occurred(error)) return

        f = 67329.234
        g = f - spacing(f)
        tol = compute_tolerance(f, g)
        ! print *, f, g ! 67329.2343750000000000000000000000000         67329.2343749999999999999999999999874
        ! print *, tol ! 2.52435489670723777731753140890491593E-0029
        call check(error, are_close(f, g, factor=ULP) .eqv. .true., &
                   to_string(f)//" and "//to_string(g)//" are within "//to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = huge(f)
        g = f - spacing(f)
        tol = compute_tolerance(f, g)
        ! print *, f, g ! 1.18973149535723176508575932662800702E+4932   1.18973149535723176508575932662800690E+4932
        ! print *, tol ! 2.29133951268909632383885951874042196E+4898
        call check(error, are_close(f, g, factor=ULP) .eqv. .true., &
                   to_string(f)//" and "//to_string(g)//" are within "//to_string(ULP)//" ULP")
        if (occurred(error)) return
    end subroutine are_close_real128_returns_T_when_within_relative_epsilon

    subroutine are_close_real128_returns_F_when_not_within_relative_epsilon(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: f, g
        real(real128) :: tol

        f = tiny(f)
        g = f*2
        tol = compute_tolerance(f, g)
        ! print *, f, g ! 3.36210314311209350626267781732175260E-4932   6.72420628622418701252535563464350521E-4932
        ! print *, tol ! 2.59007004777521004436977558329105862E-4965
        call check(error, are_close(f, g, factor=ULP) .eqv. .false., &
                   to_string(f)//" and "//to_string(g)//" are within "//to_string(ULP)//" ULP")
        if (occurred(error)) return
    end subroutine are_close_real128_returns_F_when_not_within_relative_epsilon

    function compute_tolerance(lhs, rhs) result(tol)
        implicit none
        real(real128), intent(in) :: lhs
        real(real128), intent(in) :: rhs
        real(real128) :: tol
        real(real128) :: relative_epsilon, space

        relative_epsilon = epsilon(lhs)*max(abs(lhs), abs(rhs))
        space = max(spacing(lhs), spacing(rhs))
        tol = real(ULP, kind=real128)*min(space, relative_epsilon)
    end function compute_tolerance

    subroutine sign_real128_returns_0_when_input_positive_number(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: f
        f = tiny(f)
        call check(error, sign(f) == 0, "sign bit of "//to_string(f)//" is 0, but got "//to_string(sign(f)))
        if (occurred(error)) return

        f = 1d0
        call check(error, sign(f) == 0, "sign bit of "//to_string(f)//" is 0, but got "//to_string(sign(f)))
        if (occurred(error)) return

        f = huge(f)
        call check(error, sign(f) == 0, "sign bit of "//to_string(f)//" is 0, but got "//to_string(sign(f)))
        if (occurred(error)) return

        f = 0d0
        call check(error, sign(f) == 0, "sign bit of "//to_string(f)//" is 0, but got "//to_string(sign(f)))
        if (occurred(error)) return
    end subroutine sign_real128_returns_0_when_input_positive_number

    subroutine sign_real128_returns_1_when_input_negative_number(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: f
        f = -tiny(f)
        call check(error, sign(f) == 1, "sign bit of "//to_string(f)//" is 1, but got "//to_string(sign(f)))
        if (occurred(error)) return

        f = -1d0
        call check(error, sign(f) == 1, "sign bit of "//to_string(f)//" is 1, but got "//to_string(sign(f)))
        if (occurred(error)) return

        f = -huge(f)
        call check(error, sign(f) == 1, "sign bit of "//to_string(f)//" is 1, but got "//to_string(sign(f)))
        if (occurred(error)) return

        f = -0d0
        call check(error, sign(f) == 1, "sign bit of "//to_string(f)//" is 1, but got "//to_string(sign(f)))
        if (occurred(error)) return
    end subroutine sign_real128_returns_1_when_input_negative_number
end module test_common_floatingPointNumber_unitTests_real128
