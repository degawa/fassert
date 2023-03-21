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
                                      D1I32_should_write_message_with_prefix_when_test_passed) &
                         , new_unittest("expect_same_shape_rank1_int32(), it should write message"// &
                                        " with prefix 'FAILED: ' when test failed.", &
                                        D1I32_should_write_message_with_prefix_when_test_failed) &
                         , new_unittest("expect_same_shape_rank1_int32(), it should not write any messages "// &
                                        "when test passed and `quiet`=`.true.`.", &
                                        D1I32_should_not_write_message_when_passed_input_quiet_T) &
                         , new_unittest("expect_same_shape_rank1_int32(), it should not write any messages "// &
                                        "when test failed and `quiet`=`.true.`.", &
                                        D1I32_should_not_write_message_when_failed_input_quiet_T) &
                         , new_unittest("expect_same_shape_rank1_int32(), stat should be passed-status when test passed.", &
                                        D1I32_stat_should_be_passed_status_when_test_passed) &
                         , new_unittest("expect_same_shape_rank1_int32(), stat should be failed-status when test failed.", &
                                        D1I32_stat_should_be_failed_status_when_test_failed) &
                         , new_unittest("expect_same_shape_rank2_int32(), it should write message"// &
                                        " with prefix 'PASSED: ' when test passed.", &
                                        D2I32_should_write_message_with_prefix_when_test_passed) &
                         , new_unittest("expect_same_shape_rank2_int32(), it should write message"// &
                                        " with prefix 'FAILED: ' when test failed.", &
                                        D2I32_should_write_message_with_prefix_when_test_failed) &
                         , new_unittest("expect_same_shape_rank2_int32(), it should not write any messages "// &
                                        "when test passed and `quiet`=`.true.`.", &
                                        D2I32_should_not_write_message_when_passed_input_quiet_T) &
                         , new_unittest("expect_same_shape_rank2_int32(), it should not write any messages "// &
                                        "when test failed and `quiet`=`.true.`.", &
                                        D2I32_should_not_write_message_when_failed_input_quiet_T) &
                         , new_unittest("expect_same_shape_rank2_int32(), stat should be passed-status when test passed.", &
                                        D2I32_stat_should_be_passed_status_when_test_passed) &
                         , new_unittest("expect_same_shape_rank2_int32(), stat should be failed-status when test failed.", &
                                        D2I32_stat_should_be_failed_status_when_test_failed) &
                         , new_unittest("expect_same_shape_rank3_int32(), it should write message"// &
                                        " with prefix 'PASSED: ' when test passed.", &
                                        D3I32_should_write_message_with_prefix_when_test_passed) &
                         , new_unittest("expect_same_shape_rank3_int32(), it should write message"// &
                                        " with prefix 'FAILED: ' when test failed.", &
                                        D3I32_should_write_message_with_prefix_when_test_failed) &
                         , new_unittest("expect_same_shape_rank3_int32(), it should not write any messages "// &
                                        "when test passed and `quiet`=`.true.`.", &
                                        D3I32_should_not_write_message_when_passed_input_quiet_T) &
                         , new_unittest("expect_same_shape_rank3_int32(), it should not write any messages "// &
                                        "when test failed and `quiet`=`.true.`.", &
                                        D3I32_should_not_write_message_when_failed_input_quiet_T) &
                         , new_unittest("expect_same_shape_rank3_int32(), stat should be passed-status when test passed.", &
                                        D3I32_stat_should_be_passed_status_when_test_passed) &
                         , new_unittest("expect_same_shape_rank3_int32(), stat should be failed-status when test failed.", &
                                        D3I32_stat_should_be_failed_status_when_test_failed) &
                         ]

        test_suite_real = [ &
                          new_unittest("expect_same_shape_rank1_real32(), it should write message"// &
                                       " with prefix 'PASSED: ' when test passed.", &
                                       D1R32_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank1_real32(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         D1R32_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank1_real32(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         D1R32_should_not_write_message_when_passed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank1_real32(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         D1R32_should_not_write_message_when_failed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank1_real32(), stat should be passed-status when test passed.", &
                                         D1R32_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank1_real32(), stat should be failed-status when test failed.", &
                                         D1R32_stat_should_be_failed_status_when_test_failed) &
                          , new_unittest("expect_same_shape_rank2_real32(), it should write message"// &
                                         " with prefix 'PASSED: ' when test passed.", &
                                         D2R32_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank2_real32(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         D2R32_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank2_real32(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         D2R32_should_not_write_message_when_passed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank2_real32(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         D2R32_should_not_write_message_when_failed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank2_real32(), stat should be passed-status when test passed.", &
                                         D2R32_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank2_real32(), stat should be failed-status when test failed.", &
                                         D2R32_stat_should_be_failed_status_when_test_failed) &
                          , new_unittest("expect_same_shape_rank3_real32(), it should write message"// &
                                         " with prefix 'PASSED: ' when test passed.", &
                                         D3R32_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank3_real32(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         D3R32_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank3_real32(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         D3R32_should_not_write_message_when_passed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank3_real32(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         D3R32_should_not_write_message_when_failed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank3_real32(), stat should be passed-status when test passed.", &
                                         D3R32_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank3_real32(), stat should be failed-status when test failed.", &
                                         D3R32_stat_should_be_failed_status_when_test_failed) &
                          , new_unittest("expect_same_shape_rank1_real64(), it should write message"// &
                                         " with prefix 'PASSED: ' when test passed.", &
                                         D1R64_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank1_real64(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         D1R64_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank1_real64(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         D1R64_should_not_write_message_when_passed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank1_real64(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         D1R64_should_not_write_message_when_failed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank1_real64(), stat should be passed-status when test passed.", &
                                         D1R64_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank1_real64(), stat should be failed-status when test failed.", &
                                         D1R64_stat_should_be_failed_status_when_test_failed) &
                          , new_unittest("expect_same_shape_rank2_real64(), it should write message"// &
                                         " with prefix 'PASSED: ' when test passed.", &
                                         D2R64_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank2_real64(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         D2R64_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank2_real64(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         D2R64_should_not_write_message_when_passed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank2_real64(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         D2R64_should_not_write_message_when_failed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank2_real64(), stat should be passed-status when test passed.", &
                                         D2R64_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank2_real64(), stat should be failed-status when test failed.", &
                                         D2R64_stat_should_be_failed_status_when_test_failed) &
                          , new_unittest("expect_same_shape_rank3_real64(), it should write message"// &
                                         " with prefix 'PASSED: ' when test passed.", &
                                         D3R64_should_write_message_with_prefix_when_test_passed) &
                          , new_unittest("expect_same_shape_rank3_real64(), it should write message"// &
                                         " with prefix 'FAILED: ' when test failed.", &
                                         D3R64_should_write_message_with_prefix_when_test_failed) &
                          , new_unittest("expect_same_shape_rank3_real64(), it should not write any messages "// &
                                         "when test passed and `quiet`=`.true.`.", &
                                         D3R64_should_not_write_message_when_passed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank3_real64(), it should not write any messages "// &
                                         "when test failed and `quiet`=`.true.`.", &
                                         D3R64_should_not_write_message_when_failed_input_quiet_T) &
                          , new_unittest("expect_same_shape_rank3_real64(), stat should be passed-status when test passed.", &
                                         D3R64_stat_should_be_passed_status_when_test_passed) &
                          , new_unittest("expect_same_shape_rank3_real64(), stat should be failed-status when test failed.", &
                                         D3R64_stat_should_be_failed_status_when_test_failed) &
                          ]

        test_suite = [test_suite_int, test_suite_real]
    end subroutine collect_expect_same_shape
end module test_sameShape_expectSameShape_collection
