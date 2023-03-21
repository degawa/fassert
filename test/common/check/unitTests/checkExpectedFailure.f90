module test_common_check_unitTests_expectedFailure
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
    public :: checkExpecFail_should_write_msg_with_prefix_when_test_failed
    public :: checkExpecFail_should_write_msg_with_prefix_when_test_passed
    public :: checkExpecFail_stat_should_be_passed_status_when_test_failed
    public :: checkExpecFail_stat_should_be_failed_status_when_test_passed
    public :: checkExpecFail_should_not_write_msg_when_failed_quiet_true

contains
    subroutine checkExpecFail_should_write_msg_with_prefix_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, buffer, expected

        call setup(scratch_unit_number, test_name, expected)

        call check_expected_failure(failed, test_name)

        call get_actual_value(error, scratch_unit_number, buffer)
        call check(error, len_trim(buffer) == len(expected), &
                   "expected message length "//to_string(len(expected)) &
                   //", but got "//to_string(len_trim(buffer)))
        if (occurred(error)) return

        call check(error, trim(buffer) == expected, &
                   "expected "//enclose(expected, '"')// &
                   ", but got "//enclose(trim(buffer), '"'))

        call teardown(scratch_unit_number)
    contains
        subroutine setup(unit_number, test_name, expected)
            use :: newunit
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable :: test_name, expected

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

            test_name = "check_expected_failure should write message with prefix "//enclose(prefix_passed, "'")// &
                        " and note "//enclose(note_expected_failure, "'")//" when test failed"

            expected = prefix_passed//test_name//note_expected_failure
        end subroutine setup
        subroutine teardown(unit_number)
            integer(int32), intent(in) :: unit_number
            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine checkExpecFail_should_write_msg_with_prefix_when_test_failed

    subroutine checkExpecFail_should_write_msg_with_prefix_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, buffer, expected

        call setup(scratch_unit_number, test_name, expected)

        call check_expected_failure(.true., test_name)

        call get_actual_value(error, scratch_unit_number, buffer)
        call check(error, len_trim(buffer) == len(expected), &
                   "expected message length "//to_string(len(expected)) &
                   //", but got "//to_string(len_trim(buffer)))
        if (occurred(error)) return

        call check(error, trim(buffer) == expected, &
                   "expected "//enclose(expected, '"')// &
                   ", but got "//enclose(trim(buffer), '"'))

        call teardown(scratch_unit_number)
    contains
        subroutine setup(unit_number, test_name, expected)
            use :: newunit
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable, intent(inout) :: test_name, expected

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

            test_name = "check_expected_failure should write message with prefix "//enclose(prefix_failed, "'")// &
                        " and note "//enclose(note_unexpected_pass, "'")//"when test failed"

            expected = prefix_failed//test_name//note_unexpected_pass
        end subroutine setup
        subroutine teardown(unit_number)
            integer(int32), intent(in) :: unit_number
            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine checkExpecFail_should_write_msg_with_prefix_when_test_passed

    subroutine checkExpecFail_stat_should_be_passed_status_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        integer(int32) :: scratch_unit_number
        logical :: stat

        call setup(scratch_unit_number)

        call check_expected_failure(.false., "", stat)

        call check(error, stat .eqv. passed, &
                   "expected "//to_string(passed)//", but got "//to_string(stat))

        call teardown(scratch_unit_number)
    contains
        subroutine setup(unit_number)
            use :: newunit
            integer(int32), intent(inout) :: unit_number

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")
        end subroutine setup
        subroutine teardown(unit_number)
            integer(int32), intent(in) :: unit_number
            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine checkExpecFail_stat_should_be_passed_status_when_test_failed

    subroutine checkExpecFail_stat_should_be_failed_status_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        integer(int32) :: scratch_unit_number
        logical :: stat

        call setup(scratch_unit_number)

        call check_expected_failure(.true., "", stat)

        call check(error, stat .eqv. failed, &
                   "expected "//to_string(failed)//", but got "//to_string(stat))
        call teardown(scratch_unit_number)
    contains
        subroutine setup(unit_number)
            use :: newunit
            integer(int32), intent(inout) :: unit_number

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")
        end subroutine setup
        subroutine teardown(unit_number)
            integer(int32), intent(in) :: unit_number
            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine checkExpecFail_stat_should_be_failed_status_when_test_passed

    subroutine checkExpecFail_should_not_write_msg_when_failed_quiet_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, buffer

        call setup(scratch_unit_number, test_name)

        call check_expected_failure(.true., test_name, quiet=.true.)

        call get_actual_value(error, scratch_unit_number, buffer)
        call check(error, len_trim(buffer) == 0, &
                   "expected zero message length , but got "//to_string(len_trim(buffer)))
        if (occurred(error)) return

        call check(error, trim(buffer) == "", &
                   'expected "", but got '//enclose(trim(buffer), '"'))

        call teardown(scratch_unit_number)
    contains
        subroutine setup(unit_number, test_name)
            use :: newunit
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable :: test_name

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

            test_name = "check_expected_failure should not write message when test passed but quiet=.true."
        end subroutine setup
        subroutine teardown(unit_number)
            integer(int32), intent(in) :: unit_number
            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine checkExpecFail_should_not_write_msg_when_failed_quiet_true
end module test_common_check_unitTests_expectedFailure
