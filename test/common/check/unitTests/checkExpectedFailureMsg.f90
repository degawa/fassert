module test_common_check_unitTests_expectedFailureMsg
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_actual_value
    use :: strings_enclose
    use :: fassert_common_unit
    use :: fassert_common_message, &
        only:prefix_passed, prefix_failed, note_expected_failure, note_unexpected_pass
    use :: fassert_common_status
    use :: fassert_common_check
    implicit none
    private
    public :: checkExpecFailMsg_should_store_msg_with_prefix_when_test_failed
    public :: checkExpecFailMsg_should_store_msg_with_prefix_when_test_passed
    public :: checkExpecFailMsg_stat_should_be_passed_status_when_test_failed
    public :: checkExpecFailMsg_stat_should_be_failed_status_when_test_passed
    public :: checkExpecFailMsg_should_not_alloc_msg_when_failed_quiet_true

contains
    subroutine checkExpecFailMsg_should_store_msg_with_prefix_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: test_name, buffer, expected

        call setup(test_name, expected)

        call check_expected_failure(failed, test_name, output_message=buffer)

        call check(error, len_trim(buffer) == len(expected), &
                   "expected message length "//to_string(len(expected)) &
                   //", but got "//to_string(len_trim(buffer)))
        if (occurred(error)) return

        call check(error, trim(buffer) == expected, &
                   "expected "//enclose(expected, '"')// &
                   ", but got "//enclose(trim(buffer), '"'))

        call teardown()
    contains
        subroutine setup(test_name, expected)
            character(:), allocatable :: test_name, expected

            test_name = "check_expected_failure should store message with prefix "//enclose(prefix_passed, "'")// &
                        " and note "//enclose(note_expected_failure, "'")//" when test failed"

            expected = prefix_passed//test_name//note_expected_failure
        end subroutine setup

        subroutine teardown()
        end subroutine teardown
    end subroutine checkExpecFailMsg_should_store_msg_with_prefix_when_test_failed

    subroutine checkExpecFailMsg_should_store_msg_with_prefix_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: test_name, buffer, expected

        call setup(test_name, expected)

        call check_expected_failure(.true., test_name, output_message=buffer)

        call check(error, len_trim(buffer) == len(expected), &
                   "expected message length "//to_string(len(expected)) &
                   //", but got "//to_string(len_trim(buffer)))
        if (occurred(error)) return

        call check(error, trim(buffer) == expected, &
                   "expected "//enclose(expected, '"')// &
                   ", but got "//enclose(trim(buffer), '"'))

        call teardown()
    contains
        subroutine setup(test_name, expected)
            character(:), allocatable, intent(inout) :: test_name, expected

            test_name = "check_expected_failure should store message with prefix "//enclose(prefix_failed, "'")// &
                        " and note "//enclose(note_unexpected_pass, "'")//"when test failed"

            expected = prefix_failed//test_name//note_unexpected_pass
        end subroutine setup

        subroutine teardown()
        end subroutine teardown
    end subroutine checkExpecFailMsg_should_store_msg_with_prefix_when_test_passed

    subroutine checkExpecFailMsg_stat_should_be_passed_status_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        logical :: stat
        character(:), allocatable :: buffer

        call check_expected_failure(.false., "", stat, output_message=buffer)

        call check(error, stat .eqv. passed, &
                   "expected "//to_string(passed)//", but got "//to_string(stat))
    end subroutine checkExpecFailMsg_stat_should_be_passed_status_when_test_failed

    subroutine checkExpecFailMsg_stat_should_be_failed_status_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        logical :: stat
        character(:), allocatable :: buffer

        call check_expected_failure(.true., "", stat, output_message=buffer)

        call check(error, stat .eqv. failed, &
                   "expected "//to_string(failed)//", but got "//to_string(stat))
    end subroutine checkExpecFailMsg_stat_should_be_failed_status_when_test_passed

    subroutine checkExpecFailMsg_should_not_alloc_msg_when_failed_quiet_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: test_name, buffer

        call setup(test_name)

        call check_expected_failure(.true., test_name, quiet=.true., output_message=buffer)

        call check(error,.not. allocated(buffer), &
                   "expected allocate status "//to_string(.false.)// &
                   ", but got "//to_string(.not. allocated(buffer)))
        if (occurred(error)) return

        call teardown()
    contains
        subroutine setup(test_name)
            character(:), allocatable :: test_name
            test_name = "check_expected_failure should not allocate message when test passed but quiet=.true."
        end subroutine setup

        subroutine teardown()
        end subroutine teardown
    end subroutine checkExpecFailMsg_should_not_alloc_msg_when_failed_quiet_true
end module test_common_check_unitTests_expectedFailureMsg
