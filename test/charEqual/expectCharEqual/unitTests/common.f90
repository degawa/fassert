module test_charEqual_expectCharEqual_unitTests_common
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: fassert_common_message, only:NL
    use :: fassert_common_compare_equal_char, only:is_equal
    use :: fassert_common_unit
    implicit none
    private
    public :: check_result
    public :: before_all
    public :: after_all
    public :: setup
    public :: teardown

    logical, private, parameter :: ignore_case = .true.
    logical, private, parameter :: check_len = .true.

contains
    subroutine check_result(error, stat, expected, stat_exp, message_outpt_unit_number)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        logical, intent(in) :: stat, stat_exp
        character(*), intent(in) :: expected
        integer(int32), intent(in) :: message_outpt_unit_number

        character(:), allocatable :: actual
        logical :: cond

        call get_all_actual_value(error, message_outpt_unit_number, actual)
        if (occurred(error)) return

        cond = all([stat .eqv. stat_exp, &
                    is_equal(expected, actual, ignore_case, check_len)])
        call check(error, cond, "expected "//NL//expected//NL// &
                   "but got "//NL//actual)
        if (occurred(error)) return
    end subroutine check_result

    subroutine before_all(message_output_unit_number)
        use :: newunit
        implicit none
        integer(int32), intent(out) :: message_output_unit_number

        message_output_unit_number = get_newunit_number()
        call set_assertion_message_unit(message_output_unit_number)
    end subroutine before_all

    subroutine after_all()
        implicit none
        call set_assertion_message_unit(output_unit)
    end subroutine after_all

    subroutine setup(message_output_unit_number)
        implicit none
        integer(int32), intent(in) :: message_output_unit_number
        open (unit=message_output_unit_number, status="scratch")
    end subroutine setup

    subroutine teardown(message_output_unit_number)
        implicit none
        integer(int32), intent(in) :: message_output_unit_number
        close (message_output_unit_number)
    end subroutine teardown
end module test_charEqual_expectCharEqual_unitTests_common
