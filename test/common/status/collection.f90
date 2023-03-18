module test_common_status_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_status_unitTests
    implicit none
    private
    public :: collect_status

contains
    subroutine collect_status(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("is_test_passed(), it should return .true. when input passed status.", &
                                  isTestPassed_should_return_true_when_input_passed) &
                     , new_unittest("is_test_passed(), it should return .false. when input failed status.", &
                                    isTestPassed_should_return_false_when_input_failed) &
                     , new_unittest("is_test_failed(), it should return .true. when input failed status.", &
                                    isTestFailed_should_return_true_when_input_failed) &
                     , new_unittest("is_test_failed(), it should return .false. when input passed status.", &
                                    isTestFailed_should_return_false_when_input_passed) &
                     ]
    end subroutine collect_status
end module test_common_status_collection
