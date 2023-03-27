module test_common_check_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_check_unitTests_true
    use :: test_common_check_unitTests_expectedFailure
    use :: test_common_check_unitTests_trueStr
    use :: test_common_check_unitTests_expectedFailureMsg
    implicit none
    private
    public :: collect_check

contains
    subroutine collect_check(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("check_true_write_to_unit(), parameterized test.", &
                                  checkTrue_parameterized_test) &
                     , new_unittest("check_true_write_to_string(), parameterized test.", &
                                    checkTrueMsg_parameterized_test) &
                     , new_unittest("check_expected_failure_to_unit(), parameterized test.", &
                                    checkExpecFail_parameterized_test) &
                     , new_unittest("check_expected_failure_write_to_string(), parameterized test.", &
                                    checkExpecFailMsg_parameterized_test) &
                     ]
    end subroutine collect_check
end module test_common_check_collection
