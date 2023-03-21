module test_common_message_unitTests_write
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, get_all_actual_value
    use :: fassert_common_message
    use :: fassert_common_unit
    implicit none
    private
    public :: writeMsgToString_should_write_msg_to_str_when_allocated
    public :: writeMsgToString_should_not_write_msg_to_str_when_not_allocated
    public :: writeMsgToUnit_should_write_msg_to_unit_when_allocated
    public :: writeMsgToUnit_should_not_write_msg_to_unit_when_not_allocated

contains
    subroutine writeMsgToString_should_write_msg_to_str_when_allocated(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        character(:), allocatable :: msg, output_message, expected

        call setup(msg, expected)
        call write_message(msg, output_message)
        call check(error, output_message == expected, &
                   "expected "//expected &
                   //", but got "//output_message)
        if (occurred(error)) return

        call teardown(msg, expected, output_message)
    contains
        subroutine setup(msg, expected)
            character(:), allocatable, intent(inout) :: msg, expected
            msg = "write_message(), it should write message to output_message when message is allocated"
            expected = msg
        end subroutine setup
        subroutine teardown(msg, expected, output_message)
            character(:), allocatable, intent(inout) :: msg, expected, output_message
            if (allocated(msg)) deallocate (msg)
            if (allocated(expected)) deallocate (expected)
            if (allocated(output_message)) deallocate (output_message)
        end subroutine teardown
    end subroutine writeMsgToString_should_write_msg_to_str_when_allocated

    subroutine writeMsgToString_should_not_write_msg_to_str_when_not_allocated(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        character(:), allocatable :: msg, actual

        call setup(msg)
        call write_message(msg, actual)
        call check(error, allocated(actual) .eqv. .false., &
                   "expected "//string_false &
                   //", but got "//to_string(allocated(actual)))
        if (occurred(error)) return

        call teardown(msg, actual)
    contains
        subroutine setup(msg)
            character(:), allocatable, intent(inout) :: msg
        end subroutine setup
        subroutine teardown(msg, output_message)
            character(:), allocatable, intent(inout) :: msg, output_message
            if (allocated(msg)) deallocate (msg)
            if (allocated(output_message)) deallocate (output_message)
        end subroutine teardown
    end subroutine writeMsgToString_should_not_write_msg_to_str_when_not_allocated

    subroutine writeMsgToUnit_should_write_msg_to_unit_when_allocated(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: msg, actual, expected

        call setup(scratch_unit_number, msg, expected)
        call write_message(msg)

        call get_all_actual_value(error, scratch_unit_number, actual)
        call check(error, actual == expected, &
                   "expected "//expected &
                   //", but got "//actual)
        if (occurred(error)) return

        call teardown(scratch_unit_number, msg, expected, actual)
    contains
        subroutine setup(unit_number, msg, expected)
            use :: newunit
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable, intent(inout) :: msg, expected

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

            msg = "write_message(), it should write message to unit when message is allocated"
            expected = msg
        end subroutine setup
        subroutine teardown(unit_number, msg, expected, output_message)
            integer(int32), intent(in) :: unit_number
            character(:), allocatable, intent(inout) :: msg, expected, output_message
            close (unit_number)
            call set_assertion_message_unit(output_unit)

            if (allocated(msg)) deallocate (msg)
            if (allocated(expected)) deallocate (expected)
            if (allocated(output_message)) deallocate (output_message)
        end subroutine teardown
    end subroutine writeMsgToUnit_should_write_msg_to_unit_when_allocated

    subroutine writeMsgToUnit_should_not_write_msg_to_unit_when_not_allocated(error)
        use :: strings_enclose
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        integer(int32) :: scratch_unit_number
        character(:), allocatable :: msg, actual

        call setup(scratch_unit_number, msg)
        call write_message(msg)

        call get_all_actual_value(error, scratch_unit_number, actual)
        call check(error, actual == "", &
                   "expected "//enclose("", '"') &
                   //", but got "//enclose(actual, '"'))
        if (occurred(error)) return
        if (occurred(error)) return

        call teardown(scratch_unit_number, msg, actual)
    contains
        subroutine setup(unit_number, msg)
            use :: newunit
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable, intent(inout) :: msg

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")
        end subroutine setup

        subroutine teardown(unit_number, msg, output_message)
            integer(int32), intent(in) :: unit_number
            character(:), allocatable, intent(inout) :: msg, output_message
            close (unit_number)
            call set_assertion_message_unit(output_unit)

            if (allocated(msg)) deallocate (msg)
            if (allocated(output_message)) deallocate (output_message)
        end subroutine teardown
    end subroutine writeMsgToUnit_should_not_write_msg_to_unit_when_not_allocated
end module test_common_message_unitTests_write
