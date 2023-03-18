module test_common_status_unitTests
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: fassette_common_status
    implicit none
    private
    public :: isTestPassed_should_return_true_when_input_passed
    public :: isTestPassed_should_return_false_when_input_failed
    public :: isTestFailed_should_return_true_when_input_failed
    public :: isTestFailed_should_return_false_when_input_passed
    public :: isTestExpecFail_should_return_true_when_input_true
    public :: isTestExpecFail_should_return_false_when_input_false
    public :: isTestExpecFail_should_return_false_when_no_input

contains
    subroutine isTestPassed_should_return_true_when_input_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        call check(error, is_test_passed(passed) .eqv. .true., &
                   "expected "//to_string(.true.)//", but got "//to_string(is_test_passed(passed)))
        if (occurred(error)) return
    end subroutine isTestPassed_should_return_true_when_input_passed

    subroutine isTestPassed_should_return_false_when_input_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        call check(error, is_test_passed(failed) .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(is_test_passed(failed)))
        if (occurred(error)) return
    end subroutine isTestPassed_should_return_false_when_input_failed

    subroutine isTestFailed_should_return_true_when_input_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        call check(error, is_test_failed(failed) .eqv. .true., &
                   "expected "//to_string(.true.)//", but got "//to_string(is_test_passed(failed)))
        if (occurred(error)) return
    end subroutine isTestFailed_should_return_true_when_input_failed

    subroutine isTestFailed_should_return_false_when_input_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        call check(error, is_test_failed(passed) .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(is_test_failed(passed)))
        if (occurred(error)) return
    end subroutine isTestFailed_should_return_false_when_input_passed

    subroutine isTestExpecFail_should_return_true_when_input_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, is_test_of_expected_failure(.true.) .eqv. .true., &
                   "expected "//to_string(.true.)//", but got "//to_string(is_test_of_expected_failure(.true.)))
        if (occurred(error)) return
    end subroutine isTestExpecFail_should_return_true_when_input_true

    subroutine isTestExpecFail_should_return_false_when_input_false(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, is_test_of_expected_failure(.false.) .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(is_test_of_expected_failure(.false.)))
        if (occurred(error)) return
    end subroutine isTestExpecFail_should_return_false_when_input_false

    subroutine isTestExpecFail_should_return_false_when_no_input(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, is_test_of_expected_failure() .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(is_test_of_expected_failure()))
        if (occurred(error)) return
    end subroutine isTestExpecFail_should_return_false_when_no_input
end module test_common_status_unitTests
