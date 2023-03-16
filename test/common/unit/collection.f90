module test_assert_common_unit_collection
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_assert_common_unit_unitTests
    implicit none
    private
    public :: collect_unit

contains
    subroutine collect_unit(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("assertion_message_unit, initial value should equal to output_unit.", &
                                  initial_value_should_equal_to_output_unit) &
                     , new_unittest("assertion_message_unit, it should equal to unit number passed to the setter", &
                                    unit_number_should_equal_to_unit_number_passed_to_setter) &
                     ]
    end subroutine collect_unit
end module test_assert_common_unit_collection
