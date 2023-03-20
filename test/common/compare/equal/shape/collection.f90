module test_common_compare_equal_shape_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_compare_equal_shape_unitTests_areSameShape_int
    use :: test_common_compare_equal_shape_unitTests_areSameShape_real
    use :: test_common_compare_equal_shape_unitTests_areSameLength
    implicit none
    private
    public :: collect_are_same_shape

contains
    subroutine collect_are_same_shape(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("are_same_shape_rank1() for int32, "// &
                                  "it should return .true. when input arrays having same shape.", &
                                  d1_i32_should_return_true_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank1() for int32, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    d1_i32_should_return_false_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank2() for int32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    d2_i32_should_return_true_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank2() for int32, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    d2_i32_should_return_false_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank3() for int32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    d3_i32_should_return_true_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank3() for int32, it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    d3_i32_should_return_false_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank1() for real32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    d1_r32_should_return_true_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank1() for real32, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    d1_r32_should_return_false_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank2() for real32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    d2_r32_should_return_true_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank2() for real32, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    d2_r32_should_return_false_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank3() for real32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    d3_r32_should_return_true_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank3() for real32, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    d3_r32_should_return_false_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank1() for real32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    d1_r32_should_return_true_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank1() for real64, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    d1_r64_should_return_true_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank1() for real64, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    d1_r64_should_return_false_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank2() for real64, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    d2_r64_should_return_true_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank2() for real64, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    d2_r64_should_return_false_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank3() for real64, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    d3_r64_should_return_true_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank3() for real64, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    d3_r64_should_return_false_when_input_different_shape) &
                     , new_unittest("are_same_length_str(), "// &
                                    "it should return .true. when input strings having same length.", &
                                    str_should_return_true_when_input_same_length_string) &
                     , new_unittest("are_same_length_str(), "// &
                                    "it should return .false. "// &
                                    "when input strings having different length.", &
                                    str_should_return_false_when_input_different_length_string) &
                     , new_unittest("are_same_length_str(), "// &
                                    "it should return .true. "// &
                                    "when input strings having different length due to trailing whitespace.", &
                                    str_should_return_true_when_input_diff_len_due_to_tailing_space) &
                     ]
    end subroutine collect_are_same_shape
end module test_common_compare_equal_shape_collection
