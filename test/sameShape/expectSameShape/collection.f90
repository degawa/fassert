module test_sameShape_expectSameShape_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_sameShape_expectSameShape_unitTests_expect_int
    use :: test_sameShape_expectSameShape_unitTests_expect_real
    implicit none
    private
    public :: collect_expect_same_shape

contains
    subroutine collect_expect_same_shape(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        type(unittest_type), allocatable :: test_suite_int(:)
        type(unittest_type), allocatable :: test_suite_real(:)

        test_suite_int = [ &
                         new_unittest("expect_same_shape_rank1_int32(), it should write message"// &
                                      " with prefix 'PASSED: ' when test passed.", &
                                      d1_i32_should_write_message_with_prefix_when_test_passed) &
                         , new_unittest("expect_same_shape_rank1_int32(), it should write message"// &
                                        " with prefix 'FAILED: ' when test failed.", &
                                        d1_i32_should_write_message_with_prefix_when_test_failed) &
                         , new_unittest("expect_same_shape_rank1_int32(), it should not write any messages "// &
                                        "when test passed and `quiet`=`.true.`.", &
                                        d1_i32_should_not_write_message_when_passed_input_quiet_true) &
                         , new_unittest("expect_same_shape_rank1_int32(), it should not write any messages "// &
                                        "when test failed and `quiet`=`.true.`.", &
                                        d1_i32_should_not_write_message_when_failed_input_quiet_true) &
                         , new_unittest("expect_same_shape_rank1_int32(), stat should be passed-status when test passed.", &
                                        d1_i32_stat_should_be_passed_status_when_test_passed) &
                         , new_unittest("expect_same_shape_rank1_int32(), stat should be failed-status when test failed.", &
                                        d1_i32_stat_should_be_failed_status_when_test_failed) &
                         , new_unittest("expect_same_shape_rank2_int32(), it should write message"// &
                                        " with prefix 'PASSED: ' when test passed.", &
                                        d2_i32_should_write_message_with_prefix_when_test_passed) &
                         , new_unittest("expect_same_shape_rank2_int32(), it should write message"// &
                                        " with prefix 'FAILED: ' when test failed.", &
                                        d2_i32_should_write_message_with_prefix_when_test_failed) &
                         , new_unittest("expect_same_shape_rank2_int32(), it should not write any messages "// &
                                        "when test passed and `quiet`=`.true.`.", &
                                        d2_i32_should_not_write_message_when_passed_input_quiet_true) &
                         , new_unittest("expect_same_shape_rank2_int32(), it should not write any messages "// &
                                        "when test failed and `quiet`=`.true.`.", &
                                        d2_i32_should_not_write_message_when_failed_input_quiet_true) &
                         , new_unittest("expect_same_shape_rank2_int32(), stat should be passed-status when test passed.", &
                                        d2_i32_stat_should_be_passed_status_when_test_passed) &
                         , new_unittest("expect_same_shape_rank2_int32(), stat should be failed-status when test failed.", &
                                        d2_i32_stat_should_be_failed_status_when_test_failed) &
                         , new_unittest("expect_same_shape_rank3_int32(), it should write message"// &
                                        " with prefix 'PASSED: ' when test passed.", &
                                        d3_i32_should_write_message_with_prefix_when_test_passed) &
                         , new_unittest("expect_same_shape_rank3_int32(), it should write message"// &
                                        " with prefix 'FAILED: ' when test failed.", &
                                        d3_i32_should_write_message_with_prefix_when_test_failed) &
                         , new_unittest("expect_same_shape_rank3_int32(), it should not write any messages "// &
                                        "when test passed and `quiet`=`.true.`.", &
                                        d3_i32_should_not_write_message_when_passed_input_quiet_true) &
                         , new_unittest("expect_same_shape_rank3_int32(), it should not write any messages "// &
                                        "when test failed and `quiet`=`.true.`.", &
                                        d3_i32_should_not_write_message_when_failed_input_quiet_true) &
                         , new_unittest("expect_same_shape_rank3_int32(), stat should be passed-status when test passed.", &
                                        d3_i32_stat_should_be_passed_status_when_test_passed) &
                         , new_unittest("expect_same_shape_rank3_int32(), stat should be failed-status when test failed.", &
                                        d3_i32_stat_should_be_failed_status_when_test_failed) &
                         ]

        test_suite_real = [ &
                          new_unittest("expect_same_shape_rank1_real32(), it should write message"// &
                                       " with prefix 'PASSED: ' when test passed.", &
                                       d1_r32_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank1_real32(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         d1_r32_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank1_real32(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         d1_r32_should_not_write_message_when_passed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank1_real32(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         d1_r32_should_not_write_message_when_failed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank1_real32(), stat should be passed-status when test passed.", &
                                         d1_r32_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank1_real32(), stat should be failed-status when test failed.", &
                                         d1_r32_stat_should_be_failed_status_when_test_failed) &
                          , new_unittest("expect_same_shape_rank2_real32(), it should write message"// &
                                         " with prefix 'PASSED: ' when test passed.", &
                                         d2_r32_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank2_real32(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         d2_r32_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank2_real32(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         d2_r32_should_not_write_message_when_passed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank2_real32(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         d2_r32_should_not_write_message_when_failed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank2_real32(), stat should be passed-status when test passed.", &
                                         d2_r32_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank2_real32(), stat should be failed-status when test failed.", &
                                         d2_r32_stat_should_be_failed_status_when_test_failed) &
                          , new_unittest("expect_same_shape_rank3_real32(), it should write message"// &
                                         " with prefix 'PASSED: ' when test passed.", &
                                         d3_r32_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank3_real32(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         d3_r32_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank3_real32(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         d3_r32_should_not_write_message_when_passed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank3_real32(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         d3_r32_should_not_write_message_when_failed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank3_real32(), stat should be passed-status when test passed.", &
                                         d3_r32_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank3_real32(), stat should be failed-status when test failed.", &
                                         d3_r32_stat_should_be_failed_status_when_test_failed) &
                          , new_unittest("expect_same_shape_rank1_real64(), it should write message"// &
                                         " with prefix 'PASSED: ' when test passed.", &
                                         d1_r64_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank1_real64(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         d1_r64_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank1_real64(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         d1_r64_should_not_write_message_when_passed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank1_real64(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         d1_r64_should_not_write_message_when_failed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank1_real64(), stat should be passed-status when test passed.", &
                                         d1_r64_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank1_real64(), stat should be failed-status when test failed.", &
                                         d1_r64_stat_should_be_failed_status_when_test_failed) &
                          , new_unittest("expect_same_shape_rank2_real64(), it should write message"// &
                                         " with prefix 'PASSED: ' when test passed.", &
                                         d2_r64_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank2_real64(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         d2_r64_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank2_real64(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         d2_r64_should_not_write_message_when_passed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank2_real64(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         d2_r64_should_not_write_message_when_failed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank2_real64(), stat should be passed-status when test passed.", &
                                         d2_r64_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank2_real64(), stat should be failed-status when test failed.", &
                                         d2_r64_stat_should_be_failed_status_when_test_failed) &
                          , new_unittest("expect_same_shape_rank3_real64(), it should write message"// &
                                         " with prefix 'PASSED: ' when test passed.", &
                                         d3_r64_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank3_real64(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         d3_r64_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank3_real64(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         d3_r64_should_not_write_message_when_passed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank3_real64(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         d3_r64_should_not_write_message_when_failed_input_quiet_true) &
                          , new_unittest("expect_same_shape_rank3_real64(), stat should be passed-status when test passed.", &
                                         d3_r64_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank3_real64(), stat should be failed-status when test failed.", &
                                         d3_r64_stat_should_be_failed_status_when_test_failed) &
                          ]

        test_suite = [test_suite_int, test_suite_real]
    end subroutine collect_expect_same_shape
end module test_sameShape_expectSameShape_collection
