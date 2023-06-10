module test_common_compare_approx_real_unitTests_isApproxEqual
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: par_funnel
    use :: fassert_common_compare_approx_real
    implicit none
    private
    public :: is_approx_equal_real32
    public :: is_approx_equal_real64
contains
    subroutine is_approx_equal_real32(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: val1, val2, tol

        val1 = 1e0; val2 = 2e0; tol = 1e0
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = 1e0; val2 = 0e0; tol = 1e0
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = 1e0; val2 = 1.09e0; tol = 1e-1
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = 1e0; val2 = 0.91e0; tol = 1e-1
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = 0.1e0; val2 = 0.2e0; tol = 1e-2
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .false., &
                   to_string(val1)//" and "//to_string(val2)//" are not equal within tolerance "//to_string(tol))
        if (occurred(error)) return
    end subroutine is_approx_equal_real32

    subroutine is_approx_equal_real64(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real64) :: val1, val2, tol

        val1 = 1d0; val2 = 2d0; tol = 1d0
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = 1d0; val2 = 0d0; tol = 1d0
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = 1d0; val2 = 1.09d0; tol = 1d-1
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = 1d0; val2 = 0.91d0; tol = 1d-1
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = 0.1d0; val2 = 0.2d0; tol = 1d-2
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .false., &
                   to_string(val1)//" and "//to_string(val2)//" are not equal within tolerance "//to_string(tol))
        if (occurred(error)) return
    end subroutine is_approx_equal_real64
end module test_common_compare_approx_real_unitTests_isApproxEqual
