module test_common_check_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_check_unitTests_true
    use :: test_common_check_unitTests_expectedFailure
    implicit none
    private
    public :: collect_check

contains
    subroutine collect_check(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("check_true(), it should write message with prefix 'PASSED: ' when test passed.", &
                                  checkTrue_should_write_message_with_prefix_when_test_passed) &
                     , new_unittest("check_true(), it should write message with prefix 'FAILED: ' when test failed.", &
                                    checkTrue_should_write_message_with_prefix_when_test_failed) &
                     , new_unittest("check_true(), stat should be passed-status when test passed.", &
                                    stat_should_be_passed_status_when_test_passed) &
                     , new_unittest("check_true(), stat should be failed-status when test failed.", &
                                    stat_should_be_failed_status_when_test_failed) &
                     , new_unittest("check_true(), it should not write message when test passed but quiet=`.true.`.", &
                                    checkTrue_should_not_write_message_when_passed_quiet_true) &
                     , new_unittest("check_expected_failure(), it should write message with prefix 'PASSED: ' "// &
                                    "and note '[expected failure]' when test failed.", &
                                    checkExpecFail_should_write_msg_with_prefix_when_test_failed) &
                     , new_unittest("check_expected_failure(), it should write message with prefix 'FAILED: ' "// &
                                    "and note '[unexpected pass]' when test passed.", &
                                    checkExpecFail_should_write_msg_with_prefix_when_test_passed) &
                     , new_unittest("check_expected_failure(), stat should be passed-status when test failed.", &
                                    stat_should_be_passed_status_when_test_failed) &
                     , new_unittest("check_expected_failure(), stat should be failed-status when test passed.", &
                                    stat_should_be_failed_status_when_test_passed) &
                     , new_unittest("check_expected_failure(), it should not write message when test failed but quiet=`.true.`.", &
                                    checkExpecFail_should_not_write_msg_when_failed_quiet_true) &
                     ]
    end subroutine collect_check
end module test_common_check_collection
