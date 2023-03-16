module test_common_optval_unitTests_l
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: assert_common_optval
    implicit none
    private
    public :: optval_logical_should_return_x_when_intpu_x_and_default
    public :: optval_logical_should_return_default_when_does_not_input_x

contains
    subroutine optval_logical_should_return_x_when_intpu_x_and_default(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: x, default
        logical :: result

        call setup(x, default)

        result = optval(x, default)
        call check(error, result .eqv. x, &
                   "expected "//to_string(x)//", but got "//to_string(result))
        if (occurred(error)) return
    contains
        subroutine setup(x, default)
            implicit none
            logical, intent(inout) :: x
            logical, intent(inout) :: default
            real(real32) :: rand

            call random_number(rand)
            if (rand > 0.5) then
                x = .true.
            else
                x = .false.
            end if
            default = .not. x
        end subroutine setup
    end subroutine optval_logical_should_return_x_when_intpu_x_and_default

    subroutine optval_logical_should_return_default_when_does_not_input_x(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: x, default
        logical :: result

        call setup(x, default)

        result = optval(default=default)
        call check(error, result .eqv. default, &
                   "expected "//to_string(default)//", but got "//to_string(result))
        if (occurred(error)) return
    contains
        subroutine setup(x, default)
            implicit none
            logical, intent(inout) :: x
            logical, intent(inout) :: default
            real(real32) :: rand

            call random_number(rand)
            if (rand > 0.5) then
                x = .true.
            else
                x = .false.
            end if
            default = .not. x
        end subroutine setup
    end subroutine optval_logical_should_return_default_when_does_not_input_x
end module test_common_optval_unitTests_l
