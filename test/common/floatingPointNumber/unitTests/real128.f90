module test_common_floatingPointNumber_unitTests_real128
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_floatingPointNumber_real128
    use :: fassert_common_floatingPointNumber_int128, only:as_int128
    use :: fassert_common_userSpecified, only:ULP
    use :: strings_enclose
    use :: test_common_floatingPointNumber_unitTests_int128, to_str => to_string
    implicit none
    private
    public :: is_distance_less_than_n_ulp_real128_returns_true
    public :: is_distance_less_than_n_ulp_real128_returns_false

contains

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
end module test_common_floatingPointNumber_unitTests_real128
