module test_common_floatingPointNumber_unitTests_real32
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_floatingPointNumber_real32
    use :: fassert_common_userSpecified, only:ULP
    implicit none
    private
    public :: is_distance_less_than_n_ulp_real32_returns_true
    public :: sign_real32_returns_0_when_input_positive_number
    public :: sign_real32_returns_1_when_input_negative_number

contains
    subroutine is_distance_less_than_n_ulp_real32_returns_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: f, g

        f = 67329.234
        g = f + spacing(f)
        ! block
        !     integer(int32) :: a, b
        !     a = transfer(f, mold=a)
        !     b = transfer(g, mold=b)
        !     print *, a, b ! 1199800478  1199800479
        !     print *, f, g ! 67329.2344       67329.2422
        ! end block
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//" and "//to_string(g)//" are within "//to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = 67329.234
        g = f - spacing(f)
        ! block
        !     integer(int32) :: a, b
        !     a = transfer(f, mold=a)
        !     b = transfer(g, mold=b)
        !     print *, a, b ! 1199800478  1199800477
        !     print *, f, g ! 67329.2344       67329.2266
        ! end block
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//" and "//to_string(g)//" are within "//to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = huge(f)
        g = f - spacing(f)
        ! block
        !     integer(int32) :: a, b
        !     a = transfer(f, mold=a)
        !     b = transfer(g, mold=b)
        !     print *, a, b ! 2139095039  2139095038
        !     print *, f, g ! 3.40282347E+38   3.40282326E+38
        ! end block
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//" and "//to_string(g)//" are within "//to_string(ULP)//" ULP")
        if (occurred(error)) return

        ! block
        !     integer(int32) :: a, b
        !     f = tiny(f)
        !     a = transfer(f, mold=a)
        !     b = a + 1
        !     g = transfer(b, g)
        !     print *, a, b ! 8388608     8388609
        !     print *, f, g ! 1.17549435E-38   1.17549449E-38
        ! end block
        f = 1.17549435e-38
        g = 1.17549449e-38
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .true., &
                   to_string(f)//" and "//to_string(g)//" are within "//to_string(ULP)//" ULP")
        if (occurred(error)) return

        f = tiny(f)
        g = f + spacing(f)
        ! block
        !     integer(int32) :: a, b
        !     a = transfer(f, mold=a)
        !     b = transfer(g, mold=b)
        !     print *, a, b ! 8388608    16777216
        !     print *, f, g ! 1.17549435E-38   2.35098870E-38
        ! end block
        call check(error, is_distance_less_than_n_ulp(f, g, ULP) .eqv. .false., &
                   to_string(f)//" and "//to_string(g)//" are within "//to_string(ULP)//" ULP")
        if (occurred(error)) return
    end subroutine is_distance_less_than_n_ulp_real32_returns_true

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
