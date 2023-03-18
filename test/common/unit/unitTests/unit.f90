module test_common_unit_unitTests
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_unit
    implicit none
    private
    public :: initial_value_should_equal_to_output_unit
    public :: unit_number_should_equal_to_unit_number_passed_to_setter

contains
    subroutine initial_value_should_equal_to_output_unit(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, assertion_message_unit, output_unit, &
                   "expected "//to_string(output_unit)//", but got "//to_string(assertion_message_unit))
    end subroutine initial_value_should_equal_to_output_unit

    subroutine unit_number_should_equal_to_unit_number_passed_to_setter(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        integer(int32) :: unit_number

        call setup(unit_number)

        call check(error, assertion_message_unit, unit_number, &
                   "expected "//to_string(unit_number)//", but got "//to_string(assertion_message_unit))

        call teardown()
    contains
        subroutine setup(unit_number)
            use :: newunit
            implicit none
            integer(int32), intent(inout) :: unit_number
            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
        end subroutine setup
        subroutine teardown()
            implicit none
            call set_assertion_message_unit(output_unit)
        end subroutine teardown
    end subroutine unit_number_should_equal_to_unit_number_passed_to_setter
end module test_common_unit_unitTests
