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
                     new_unittest("is_test_passed(), parameterized test.", &
                                  isTestPassed_parameterized_test) &
                     , new_unittest("is_test_failed(), parameterized test.", &
                                    isTestFailed_parameterized_test) &
                     , new_unittest("is_test_of_expected_failure(), parameterized test.", &
                                    isTestExpecFail_parameterized_test) &
                     ]
    end subroutine collect_status
end module test_common_status_collection
