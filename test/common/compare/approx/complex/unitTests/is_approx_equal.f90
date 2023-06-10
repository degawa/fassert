module test_common_compare_approx_complex_unitTests_isApproxEqual
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: par_funnel
    use :: fassert_common_compare_approx_complex
    implicit none
    private
    public :: is_approx_equal_complex32
    public :: is_approx_equal_complex64
contains
    subroutine is_approx_equal_complex32(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        complex(real32) :: val1, val2
        real(real32) :: tol

        val1 = cmplx(1e0, 0e0); val2 = cmplx(0e0, 0e0); tol = 1e0
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = cmplx(0e0, 1e0); val2 = cmplx(0e0, 0e0); tol = 1e0
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = cmplx(0e0, 1e0); val2 = cmplx(1e0, 0e0); tol = 1e0
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .false., &
                   to_string(val1)//" and "//to_string(val2)//" are not equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = cmplx(0e0, 1e0); val2 = cmplx(1e0, 0e0); tol = sqrt(2e0)
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return
    end subroutine is_approx_equal_complex32

    subroutine is_approx_equal_complex64(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        complex(real64) :: val1, val2
        real(real64) :: tol

        val1 = cmplx(1d0, 0d0, kind=real64); val2 = cmplx(0d0, 0d0, kind=real64); tol = 1d0
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = cmplx(0d0, 1d0, kind=real64); val2 = cmplx(0d0, 0d0, kind=real64); tol = 1d0
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = cmplx(0d0, 1d0, kind=real64); val2 = cmplx(1d0, 0d0, kind=real64); tol = 1d0
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .false., &
                   to_string(val1)//" and "//to_string(val2)//" are not equal within tolerance "//to_string(tol))
        if (occurred(error)) return

        val1 = cmplx(0d0, 1d0, kind=real64); val2 = cmplx(1d0, 0d0, kind=real64); tol = sqrt(2d0)
        call check(error, is_approx_equal(val1, val2, tol) .eqv. .true., &
                   to_string(val1)//" and "//to_string(val2)//" are equal within tolerance "//to_string(tol))
        if (occurred(error)) return
    end subroutine is_approx_equal_complex64
end module test_common_compare_approx_complex_unitTests_isApproxEqual
