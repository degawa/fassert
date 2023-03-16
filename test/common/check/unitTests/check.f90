module test_assert_common_check_unitTests_true
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string
    use :: assert_common_unit
    use :: assert_common_message, only:prefix_passed, prefix_failed
    use :: assert_common_status
    use :: assert_common_check
    implicit none
    private
    public :: check_true_should_write_message_with_prefix_when_test_passed
    public :: check_true_should_write_message_with_prefix_when_test_failed
    public :: stat_should_be_passed_status_when_test_passed
    public :: stat_should_be_failed_status_when_test_failed
    public :: check_true_should_not_write_message_when_passed_quiet_true

contains
    subroutine check_true_should_write_message_with_prefix_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: scratch_unit_number, iostat
        character(256) :: buffer
        character(:), allocatable :: test_name

        call setup(scratch_unit_number, test_name)

        call check_true(.true., test_name)

        rewind (unit=scratch_unit_number)
        read (scratch_unit_number, '(A)', iostat=iostat) buffer
        if (is_iostat_end(iostat)) then
            buffer = ""
        else
            call check(error, iostat == 0, &
                       "io error "//to_string(iostat)//" during getting actual value from scratch file.")
            if (occurred(error)) return
        end if

        call check(error, len_trim(buffer) == len(prefix_passed//test_name), &
                   "expected message length "//to_string(len(test_name)) &
                   //", but got "//to_string(len(prefix_passed) + len_trim(buffer)))
        if (occurred(error)) return

        call check(error, trim(buffer) == prefix_passed//test_name, &
                   "expected '"//prefix_passed//test_name//"', but got '"//trim(buffer)//"'")

        call teardown(scratch_unit_number)
    contains
        subroutine setup(unit_number, test_name)
            use :: newunit
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable :: test_name

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

            test_name = "check_true should write message with prefix '"//prefix_passed//"' when test passed"
        end subroutine setup
        subroutine teardown(unit_number)
            integer(int32), intent(in) :: unit_number
            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine check_true_should_write_message_with_prefix_when_test_passed

    subroutine check_true_should_write_message_with_prefix_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: scratch_unit_number, iostat
        character(256) :: buffer
        character(:), allocatable :: test_name

        call setup(scratch_unit_number, test_name)

        call check_true(.false., test_name)

        rewind (unit=scratch_unit_number)
        read (scratch_unit_number, '(A)', iostat=iostat) buffer
        if (is_iostat_end(iostat)) then
            buffer = ""
        else
            call check(error, iostat == 0, &
                       "io error "//to_string(iostat)//" occured during getting actual value from scratch file.")
            if (occurred(error)) return
        end if

        call check(error, len_trim(buffer) == len(prefix_failed//test_name), &
                   "expected message length "//to_string(len(test_name)) &
                   //", but got "//to_string(len(prefix_failed) + len_trim(buffer)))
        if (occurred(error)) return

        call check(error, trim(buffer) == prefix_failed//test_name, &
                   "expected '"//prefix_failed//test_name//"', but got '"//trim(buffer)//"'")

        call teardown(scratch_unit_number)
    contains
        subroutine setup(unit_number, test_name)
            use :: newunit
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable, intent(inout) :: test_name

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

            test_name = "check_true should write message with prefix '"//prefix_failed//"' when test failed"
        end subroutine setup
        subroutine teardown(unit_number)
            integer(int32), intent(in) :: unit_number
            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine check_true_should_write_message_with_prefix_when_test_failed

    subroutine stat_should_be_passed_status_when_test_passed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        integer(int32) :: scratch_unit_number
        logical :: stat

        call setup(scratch_unit_number)

        call check_true(.true., "", stat)

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
    end subroutine stat_should_be_passed_status_when_test_passed

    subroutine stat_should_be_failed_status_when_test_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        integer(int32) :: scratch_unit_number
        logical :: stat

        call setup(scratch_unit_number)

        call check_true(.false., "", stat)

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
    end subroutine stat_should_be_failed_status_when_test_failed

    subroutine check_true_should_not_write_message_when_passed_quiet_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: scratch_unit_number, iostat
        character(256) :: buffer
        character(:), allocatable :: test_name

        call setup(scratch_unit_number, test_name)

        call check_true(.true., test_name, quiet=.true.)

        rewind (unit=scratch_unit_number)
        read (scratch_unit_number, '(A)', iostat=iostat) buffer
        if (is_iostat_end(iostat)) then
            buffer = ""
        else
            call check(error, iostat == 0, &
                       "io error "//to_string(iostat)//" during getting actual value from scratch file.")
            if (occurred(error)) return
        end if

        call check(error, len_trim(buffer) == 0, &
                   "expected zero message length , but got "//to_string(len_trim(buffer)))
        if (occurred(error)) return

        call check(error, trim(buffer) == "", &
                   'expected "", but got "'//trim(buffer)//'"')

        call teardown(scratch_unit_number)
    contains
        subroutine setup(unit_number, test_name)
            use :: newunit
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable :: test_name

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")

            test_name = "check_true should not write message when test passed but quiet=.true."
        end subroutine setup
        subroutine teardown(unit_number)
            integer(int32), intent(in) :: unit_number
            close (unit_number)
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine check_true_should_not_write_message_when_passed_quiet_true
end module test_assert_common_check_unitTests_true
