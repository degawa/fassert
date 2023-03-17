module test_common_status_unitTests
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: fassette_common_status
    implicit none
    private
    public :: is_test_passed_should_return_true_when_input_passed
    public :: is_test_passed_should_return_false_when_input_failed
    public :: is_test_failed_should_return_true_when_input_failed
    public :: is_test_failed_should_return_false_when_input_passed

contains
    subroutine is_test_passed_should_return_true_when_input_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        call check(error, is_test_passed(passed) .eqv. .true., &
                   "expected "//to_string(.true.)//", but got "//to_string(is_test_passed(passed)))
        if (occurred(error)) return
    end subroutine is_test_passed_should_return_true_when_input_passed

    subroutine is_test_passed_should_return_false_when_input_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        call check(error, is_test_passed(failed) .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(is_test_passed(failed)))
        if (occurred(error)) return
    end subroutine is_test_passed_should_return_false_when_input_failed

    subroutine is_test_failed_should_return_true_when_input_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        call check(error, is_test_failed(failed) .eqv. .true., &
                   "expected "//to_string(.true.)//", but got "//to_string(is_test_passed(failed)))
        if (occurred(error)) return
    end subroutine is_test_failed_should_return_true_when_input_failed

    subroutine is_test_failed_should_return_false_when_input_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        call check(error, is_test_failed(passed) .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(is_test_failed(passed)))
        if (occurred(error)) return
    end subroutine is_test_failed_should_return_false_when_input_passed
end module test_common_status_unitTests
