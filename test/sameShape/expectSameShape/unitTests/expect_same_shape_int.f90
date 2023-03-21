module test_sameShape_expectSameShape_unitTests_expect_int
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, get_all_actual_value
    use :: strings_enclose
    use :: fassert_common_unit
    use :: fassert_common_message
    use :: fassert_common_status
    use :: expectSameShape
    implicit none
    private
    ! D: dimension, I: integer
    ! T: true
    public :: D1I32_should_write_message_with_prefix_when_test_passed
    public :: D2I32_should_write_message_with_prefix_when_test_passed
    public :: D3I32_should_write_message_with_prefix_when_test_passed
    public :: D1I32_should_write_message_with_prefix_when_test_failed
    public :: D2I32_should_write_message_with_prefix_when_test_failed
    public :: D3I32_should_write_message_with_prefix_when_test_failed
    public :: D1I32_should_not_write_message_when_passed_input_quiet_T
    public :: D2I32_should_not_write_message_when_passed_input_quiet_T
    public :: D3I32_should_not_write_message_when_passed_input_quiet_T
    public :: D1I32_should_not_write_message_when_failed_input_quiet_T
    public :: D2I32_should_not_write_message_when_failed_input_quiet_T
    public :: D3I32_should_not_write_message_when_failed_input_quiet_T
    public :: D1I32_stat_should_be_passed_status_when_test_passed
    public :: D2I32_stat_should_be_passed_status_when_test_passed
    public :: D3I32_stat_should_be_passed_status_when_test_passed
    public :: D1I32_stat_should_be_failed_status_when_test_failed
    public :: D2I32_stat_should_be_failed_status_when_test_failed
    public :: D3I32_stat_should_be_failed_status_when_test_failed

contains
    subroutine D1I32_should_write_message_with_prefix_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:), expected(:)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual, msg_expected
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number, msg_expected)

        call expect_same_shape(actual, expected, test_name, stat)

        call get_all_actual_value(error, scratch_unit_number, msg_actual)
        call check(error, len_trim(msg_actual) == len(msg_expected), &
                   "expected message length "//to_string(len(msg_expected)) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == msg_expected, &
                   "expected "//enclose(msg_expected, '"')//", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number, msg)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable, intent(inout) :: test_name
            character(:), allocatable, intent(inout) :: msg

            allocate (a(10))
            allocate (b, mold=a)
            test_name = "expect_same_shape should write a message with prefix "// &
                        enclose(prefix_passed, "'")//" when test passed"
            msg = prefix_passed//test_name

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D1I32_should_write_message_with_prefix_when_test_passed

    subroutine D2I32_should_write_message_with_prefix_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :), expected(:, :)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual, msg_expected
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number, msg_expected)

        call expect_same_shape(actual, expected, test_name, stat)

        call get_all_actual_value(error, scratch_unit_number, msg_actual)
        call check(error, len_trim(msg_actual) == len(msg_expected), &
                   "expected message length "//to_string(len(msg_expected)) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == msg_expected, &
                   "expected "//enclose(msg_expected, '"')//", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number, msg)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable, intent(inout) :: test_name
            character(:), allocatable, intent(inout) :: msg

            allocate (a(5, 2))
            allocate (b, mold=a)
            test_name = "expect_same_shape should write a message with prefix "// &
                        enclose(prefix_passed, "'")//" when test passed"
            msg = prefix_passed//test_name

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D2I32_should_write_message_with_prefix_when_test_passed

    subroutine D3I32_should_write_message_with_prefix_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :, :), expected(:, :, :)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual, msg_expected
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number, msg_expected)

        call expect_same_shape(actual, expected, test_name, stat)

        call get_all_actual_value(error, scratch_unit_number, msg_actual)
        call check(error, len_trim(msg_actual) == len(msg_expected), &
                   "expected message length "//to_string(len(msg_expected)) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == msg_expected, &
                   "expected "//enclose(msg_expected, '"')//", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number, msg)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable, intent(inout) :: test_name
            character(:), allocatable, intent(inout) :: msg

            allocate (a(5, 4, 3))
            allocate (b, mold=a)
            test_name = "expect_same_shape should write a message with prefix "// &
                        enclose(prefix_passed, "'")//" when test passed"
            msg = prefix_passed//test_name

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D3I32_should_write_message_with_prefix_when_test_passed

    subroutine D1I32_should_write_message_with_prefix_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:), expected(:)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual, msg_expected
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number, msg_expected)

        call expect_same_shape(actual, expected, test_name, stat)

        call get_all_actual_value(error, scratch_unit_number, string=msg_actual)
        call check(error, len_trim(msg_actual) == len(msg_expected), &
                   "expected message length "//to_string(len(msg_expected)) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == msg_expected, &
                   "expected "//enclose(msg_expected, '"')//", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number, msg)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable, intent(inout) :: test_name
            character(:), allocatable, intent(inout) :: msg

            allocate (a(10))
            allocate (b(11))
            test_name = "expect_same_shape should write a message with prefix "// &
                        enclose(prefix_failed, "'")//" when test failed"
            msg = prefix_failed//test_name//new_line(" ")// &
                  "    Expected Shape: (11)"//new_line(" ")// &
                  "    Actual Shape  : (10)"

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D1I32_should_write_message_with_prefix_when_test_failed

    subroutine D2I32_should_write_message_with_prefix_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :), expected(:, :)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual, msg_expected
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number, msg_expected)

        call expect_same_shape(actual, expected, test_name, stat)

        call get_all_actual_value(error, scratch_unit_number, msg_actual)
        call check(error, len_trim(msg_actual) == len(msg_expected), &
                   "expected message length "//to_string(len(msg_expected)) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == msg_expected, &
                   "expected "//enclose(msg_expected, '"')//", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number, msg)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable, intent(inout) :: test_name
            character(:), allocatable, intent(inout) :: msg

            allocate (a(5, 2))
            allocate (b(6, 2))
            test_name = "expect_same_shape should write a message with prefix "// &
                        enclose(prefix_failed, "'")//" when test failed"
            msg = prefix_failed//test_name//new_line(" ")// &
                  "    Expected Shape: (6,2)"//new_line(" ")// &
                  "    Actual Shape  : (5,2)"
            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D2I32_should_write_message_with_prefix_when_test_failed

    subroutine D3I32_should_write_message_with_prefix_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :, :), expected(:, :, :)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual, msg_expected
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number, msg_expected)

        call expect_same_shape(actual, expected, test_name, stat)

        call get_all_actual_value(error, scratch_unit_number, msg_actual)
        call check(error, len_trim(msg_actual) == len(msg_expected), &
                   "expected message length "//to_string(len(msg_expected)) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == msg_expected, &
                   "expected "//enclose(msg_expected, '"')//", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number, msg)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable, intent(inout) :: test_name
            character(:), allocatable, intent(inout) :: msg

            allocate (a(4, 3, 2))
            allocate (b(3, 3, 3))
            test_name = "expect_same_shape should write a message with prefix "// &
                        enclose(prefix_failed, "'")//" when test failed"
            msg = prefix_failed//test_name//new_line(" ")// &
                  "    Expected Shape: (3,3,3)"//new_line(" ")// &
                  "    Actual Shape  : (4,3,2)"

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D3I32_should_write_message_with_prefix_when_test_failed

    subroutine D1I32_should_not_write_message_when_passed_input_quiet_T(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:), expected(:)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number)

        call expect_same_shape(actual, expected, test_name, stat, quiet=.true.)

        call get_all_actual_value(error, scratch_unit_number, msg_actual)
        call check(error, len_trim(msg_actual) == 0, &
                   "expected message length "//to_string(0) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == "", &
                   "expected "//enclose("", '"')//", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable :: test_name

            allocate (a(4))
            allocate (b, mold=a)
            test_name = "expect_same_shape should not write any messages when test passed and `quiet`=`.true.`."

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D1I32_should_not_write_message_when_passed_input_quiet_T

    subroutine D2I32_should_not_write_message_when_passed_input_quiet_T(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :), expected(:, :)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number)

        call expect_same_shape(actual, expected, test_name, stat, quiet=.true.)

        call get_all_actual_value(error, scratch_unit_number, msg_actual)
        call check(error, len_trim(msg_actual) == 0, &
                   "expected message length "//to_string(0) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == "", &
                   "expected "//enclose("", '"')//", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable :: test_name

            allocate (a(1, 2))
            allocate (b, mold=a)
            test_name = "expect_same_shape should not write any messages when test passed and `quiet`=`.true.`."

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D2I32_should_not_write_message_when_passed_input_quiet_T

    subroutine D3I32_should_not_write_message_when_passed_input_quiet_T(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :, :), expected(:, :, :)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number)

        call expect_same_shape(actual, expected, test_name, stat, quiet=.true.)

        call get_all_actual_value(error, scratch_unit_number, msg_actual)
        call check(error, len_trim(msg_actual) == 0, &
                   "expected message length "//to_string(0) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == "", &
                   "expected "//enclose("", '"')//", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable :: test_name

            allocate (a(1, 2, 3))
            allocate (b, mold=a)
            test_name = "expect_same_shape should not write any messages when test passed and `quiet`=`.true.`."

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D3I32_should_not_write_message_when_passed_input_quiet_T

    subroutine D1I32_should_not_write_message_when_failed_input_quiet_T(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:), expected(:)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual, msg_expected
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number, msg_expected)

        call expect_same_shape(actual, expected, test_name, stat, quiet=.true.)

        call get_all_actual_value(error, scratch_unit_number, msg_actual)
        call check(error, len_trim(msg_actual) == len_trim(msg_expected), &
                   "expected message length "//to_string(len_trim(msg_expected)) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == trim(msg_expected), &
                   "expected "//enclose(trim(msg_expected), '"')// &
                   ", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number, msg)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable :: test_name, msg

            allocate (a(4))
            allocate (b(6))
            test_name = "expect_same_shape should not write any messages when test failed and `quiet`=`.true.`."
            msg = "    Expected Shape: (6)"//new_line(" ")// &
                  "    Actual Shape  : (4)"

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")
        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D1I32_should_not_write_message_when_failed_input_quiet_T

    subroutine D2I32_should_not_write_message_when_failed_input_quiet_T(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :), expected(:, :)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual, msg_expected
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number, msg_expected)

        call expect_same_shape(actual, expected, test_name, stat, quiet=.true.)

        call get_all_actual_value(error, scratch_unit_number, msg_actual)
        call check(error, len_trim(msg_actual) == len_trim(msg_expected), &
                   "expected message length "//to_string(len_trim(msg_expected)) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == trim(msg_expected), &
                   "expected "//enclose(trim(msg_expected), '"')// &
                   ", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number, msg)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable :: test_name, msg

            allocate (a(1, 2))
            allocate (b(4, 5))
            test_name = "expect_same_shape should not write any messages when test failed and `quiet`=`.true.`."
            msg = "    Expected Shape: (4,5)"//new_line(" ")// &
                  "    Actual Shape  : (1,2)"

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D2I32_should_not_write_message_when_failed_input_quiet_T

    subroutine D3I32_should_not_write_message_when_failed_input_quiet_T(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :, :), expected(:, :, :)
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: test_name, msg_actual, msg_expected
        logical :: stat

        call setup(actual, expected, test_name, scratch_unit_number, msg_expected)

        call expect_same_shape(actual, expected, test_name, stat, quiet=.true.)

        call get_all_actual_value(error, scratch_unit_number, msg_actual)
        call check(error, len_trim(msg_actual) == len_trim(msg_expected), &
                   "expected message length "//to_string(len_trim(msg_expected)) &
                   //", but got "//to_string(len_trim(msg_actual)))
        if (occurred(error)) return

        call check(error, trim(msg_actual) == trim(msg_actual), &
                   "expected "//enclose(trim(msg_expected), '"')// &
                   ", but got "//enclose(trim(msg_actual), '"'))

        call teardown(actual, expected, scratch_unit_number)
    contains
        subroutine setup(a, b, test_name, unit_number, msg)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable :: test_name, msg

            allocate (a(1, 2, 3))
            allocate (b(2, 2, 3))
            test_name = "expect_same_shape should not write any messages when test failed and `quiet`=`.true.`."
            msg = "    Expected Shape: (2,2,3)"//new_line(" ")// &
                  "    Actual Shape  : (1,2,3)"

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

        end subroutine setup
        subroutine teardown(a, b, unit_number)
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)
            integer(int32), intent(in) :: unit_number

            deallocate (a)
            deallocate (b)

            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine D3I32_should_not_write_message_when_failed_input_quiet_T

    subroutine D1I32_stat_should_be_passed_status_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:), expected(:)
        character(:), allocatable :: test_name
        logical :: stat

        call setup(actual, expected, test_name)

        call expect_same_shape(actual, expected, test_name, stat, quiet=.true.)

        call check(error, stat .eqv. passed, &
                   "expected "//to_string(passed)//", but got "//to_string(stat))

        call teardown(actual, expected)
    contains
        subroutine setup(a, b, test_name)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)
            character(:), allocatable, intent(inout) :: test_name

            allocate (a(10))
            allocate (b, mold=a)
            test_name = "stat should be passed status when test passed"

        end subroutine setup

        subroutine teardown(a, b)
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)

            deallocate (a)
            deallocate (b)

        end subroutine teardown
    end subroutine D1I32_stat_should_be_passed_status_when_test_passed

    subroutine D2I32_stat_should_be_passed_status_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :), expected(:, :)
        character(:), allocatable :: test_name
        logical :: stat

        call setup(actual, expected, test_name)

        call expect_same_shape(actual, expected, test_name, stat, quiet=.true.)

        call check(error, stat .eqv. passed, &
                   "expected "//to_string(passed)//", but got "//to_string(stat))

        call teardown(actual, expected)
    contains
        subroutine setup(a, b, test_name)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)
            character(:), allocatable, intent(inout) :: test_name

            allocate (a(4, 5))
            allocate (b, mold=a)
            test_name = "stat should be passed status when test passed"

        end subroutine setup

        subroutine teardown(a, b)
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)

            deallocate (a)
            deallocate (b)

        end subroutine teardown
    end subroutine D2I32_stat_should_be_passed_status_when_test_passed

    subroutine D3I32_stat_should_be_passed_status_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :, :), expected(:, :, :)
        character(:), allocatable :: test_name
        logical :: stat

        call setup(actual, expected, test_name)

        call expect_same_shape(actual, expected, test_name, stat, quiet=.true.)

        call check(error, stat .eqv. passed, &
                   "expected "//to_string(passed)//", but got "//to_string(stat))

        call teardown(actual, expected)
    contains
        subroutine setup(a, b, test_name)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)
            character(:), allocatable, intent(inout) :: test_name

            allocate (a(4, 5, 6))
            allocate (b, mold=a)
            test_name = "stat should be passed status when test passed"

        end subroutine setup

        subroutine teardown(a, b)
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)

            deallocate (a)
            deallocate (b)

        end subroutine teardown
    end subroutine D3I32_stat_should_be_passed_status_when_test_passed

    subroutine D1I32_stat_should_be_failed_status_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:), expected(:)
        character(:), allocatable :: test_name
        logical :: stat

        call setup(actual, expected, test_name)

        call expect_same_shape(actual, expected, test_name, stat, verbose=.false., quiet=.true.)

        call check(error, stat .eqv. failed, &
                   "expected "//to_string(failed)//", but got "//to_string(stat))

        call teardown(actual, expected)
    contains
        subroutine setup(a, b, test_name)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)
            character(:), allocatable, intent(inout) :: test_name

            allocate (a(10))
            allocate (b(4))
            test_name = "stat should be failed-status when test failed"

        end subroutine setup

        subroutine teardown(a, b)
            integer(int32), allocatable, intent(inout) :: a(:)
            integer(int32), allocatable, intent(inout) :: b(:)

            deallocate (a)
            deallocate (b)

        end subroutine teardown
    end subroutine D1I32_stat_should_be_failed_status_when_test_failed

    subroutine D2I32_stat_should_be_failed_status_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :), expected(:, :)
        character(:), allocatable :: test_name
        logical :: stat

        call setup(actual, expected, test_name)

        call expect_same_shape(actual, expected, test_name, stat, verbose=.false., quiet=.true.)

        call check(error, stat .eqv. failed, &
                   "expected "//to_string(failed)//", but got "//to_string(stat))

        call teardown(actual, expected)
    contains
        subroutine setup(a, b, test_name)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)
            character(:), allocatable, intent(inout) :: test_name

            allocate (a(4, 5))
            allocate (b(1, 2))
            test_name = "stat should be failed-status when test failed"

        end subroutine setup

        subroutine teardown(a, b)
            integer(int32), allocatable, intent(inout) :: a(:, :)
            integer(int32), allocatable, intent(inout) :: b(:, :)

            deallocate (a)
            deallocate (b)

        end subroutine teardown
    end subroutine D2I32_stat_should_be_failed_status_when_test_failed

    subroutine D3I32_stat_should_be_failed_status_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32), allocatable :: actual(:, :, :), expected(:, :, :)
        character(:), allocatable :: test_name
        logical :: stat

        call setup(actual, expected, test_name)

        call expect_same_shape(actual, expected, test_name, stat, verbose=.false., quiet=.true.)

        call check(error, stat .eqv. failed, &
                   "expected "//to_string(failed)//", but got "//to_string(stat))

        call teardown(actual, expected)
    contains
        subroutine setup(a, b, test_name)
            use :: newunit
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)
            character(:), allocatable, intent(inout) :: test_name

            allocate (a(4, 5, 6))
            allocate (b(2, 3, 4))
            test_name = "stat should be failed-status when test failed"

        end subroutine setup

        subroutine teardown(a, b)
            integer(int32), allocatable, intent(inout) :: a(:, :, :)
            integer(int32), allocatable, intent(inout) :: b(:, :, :)

            deallocate (a)
            deallocate (b)

        end subroutine teardown
    end subroutine D3I32_stat_should_be_failed_status_when_test_failed
end module test_sameShape_expectSameShape_unitTests_expect_int
