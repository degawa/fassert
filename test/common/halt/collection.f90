module test_assert_common_halt_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_assert_common_halt_unitTests
    implicit none
    private
    public :: collect_halt

contains
    subroutine collect_halt(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("halt_on_failure(), it should not stop execution when input passed status.", &
                                  halt_should_not_stop_when_input_passed_status) &
                     ]
    end subroutine collect_halt
end module test_assert_common_halt_collection
