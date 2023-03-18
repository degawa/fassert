module test_common_optval_unitTests_r64
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_optval
    implicit none
    private
    public :: optvalReal64_should_return_x_when_intpu_x_and_default
    public :: optvalReal64_should_return_default_when_does_not_input_x

contains
    subroutine optvalReal64_should_return_x_when_intpu_x_and_default(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real64) :: x, default
        real(real64) :: result

        call setup(x, default)

        result = optval(x, default)
        call check(error, result == x, &
                   "expected "//to_string(x)//", but got "//to_string(result))
        if (occurred(error)) return
    contains
        subroutine setup(x, default)
            implicit none
            real(real64), intent(inout) :: x
            real(real64), intent(inout) :: default

            call random_number(x)
            call random_number(default)
        end subroutine setup
    end subroutine optvalReal64_should_return_x_when_intpu_x_and_default

    subroutine optvalReal64_should_return_default_when_does_not_input_x(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real64) :: x, default
        real(real64) :: result

        call setup(x, default)

        result = optval(default=default)
        call check(error, result == default, &
                   "expected "//to_string(default)//", but got "//to_string(result))
        if (occurred(error)) return
    contains
        subroutine setup(x, default)
            implicit none
            real(real64), intent(inout) :: x
            real(real64), intent(inout) :: default

            call random_number(x)
            call random_number(default)
        end subroutine setup
    end subroutine optvalReal64_should_return_default_when_does_not_input_x
end module test_common_optval_unitTests_r64
