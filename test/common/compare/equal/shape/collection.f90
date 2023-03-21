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
                                  D1I32_should_return_T_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank1() for int32, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    D1I32_should_return_F_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank2() for int32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    D2I32_should_return_T_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank2() for int32, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    D2I32_should_return_F_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank3() for int32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    D3I32_should_return_T_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank3() for int32, it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    D3I32_should_return_F_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank1() for real32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    D1R32_should_return_T_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank1() for real32, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    D1R32_should_return_F_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank2() for real32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    D2R32_should_return_T_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank2() for real32, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    D2R32_should_return_F_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank3() for real32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    D3R32_should_return_T_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank3() for real32, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    D3R32_should_return_F_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank1() for real32, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    D1R32_should_return_T_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank1() for real64, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    D1R64_should_return_T_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank1() for real64, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    D1R64_should_return_F_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank2() for real64, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    D2R64_should_return_T_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank2() for real64, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    D2R64_should_return_F_when_input_different_shape) &
                     , new_unittest("are_same_shape_rank3() for real64, "// &
                                    "it should return .true. when input arrays having same shape.", &
                                    D3R64_should_return_T_when_input_same_shape) &
                     , new_unittest("are_same_shape_rank3() for real64, "// &
                                    "it should return .false. "// &
                                    "when input arrays having different shape.", &
                                    D3R64_should_return_F_when_input_different_shape) &
                     , new_unittest("are_same_length_str(), "// &
                                    "it should return .true. when input strings having same length.", &
                                    STR_should_return_T_when_input_same_length_string) &
                     , new_unittest("are_same_length_str(), "// &
                                    "it should return .false. "// &
                                    "when input strings having different length.", &
                                    STR_should_return_F_when_input_different_length_string) &
                     , new_unittest("are_same_length_str(), "// &
                                    "it should return .true. "// &
                                    "when input strings having different length due to trailing whitespace.", &
                                    STR_should_return_T_when_input_diff_len_due_to_tailing_space) &
                     ]
    end subroutine collect_are_same_shape
end module test_common_compare_equal_shape_collection
