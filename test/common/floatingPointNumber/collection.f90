module test_common_floatingPointNumber_collection
    use :: testdrive, only:new_unittest, unittest_type, to_string
    use :: fassert_common_userSpecified
    use :: test_common_floatingPointNumber_unitTests_real32
    use :: test_common_floatingPointNumber_unitTests_real64
    use :: test_common_floatingPointNumber_unitTests_real128
    use :: test_common_floatingPointNumber_unitTests_int128
    implicit none
    private
    public :: collect_fp

contains
    subroutine collect_fp(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("is_distance_less_than_n_ulp_real32(), it should return true when 2 values are within "// &
                                  to_string(ULP)//" ulp.", &
                                  is_distance_less_than_n_ulp_real32_returns_true) &
                     , new_unittest("is_distance_less_than_n_ulp_real32(), it should return false when 2 values are not within "// &
                                    to_string(ULP)//" ulp.", &
                                    is_distance_less_than_n_ulp_real32_returns_false) &
                     , new_unittest("sign_real32(), it should return 0 when input a positive number", &
                                    sign_real32_returns_0_when_input_positive_number) &
                     , new_unittest("sign_real32(), it should return 1 when input a negative number", &
                                    sign_real32_returns_1_when_input_negative_number) &
                     , new_unittest("is_distance_less_than_n_ulp_real64(), it should return true when 2 values are within "// &
                                    to_string(ULP)//" ulp.", &
                                    is_distance_less_than_n_ulp_real64_returns_true) &
                     , new_unittest("is_distance_less_than_n_ulp_real64(), it should return false when 2 values are not within "// &
                                    to_string(ULP)//" ulp.", &
                                    is_distance_less_than_n_ulp_real64_returns_false) &
                     , new_unittest("sign_real64(), it should return 0 when input a positive number", &
                                    sign_real64_returns_0_when_input_positive_number) &
                     , new_unittest("sign_real64(), it should return 1 when input a negative number", &
                                    sign_real64_returns_1_when_input_negative_number) &
#if !defined(NAGFOR)
                     , new_unittest("new_int128_type(), it should return int128_type instance", &
                                    construct_int128_returns_int128_type_instance) &
                     , new_unittest("to_string_int128(), it should return 128bit integer in string", &
                                    to_string_int128_returns_128bit_integer_in_string) &
                     , new_unittest("as_real128(), it should return 128bit floating-point number with the same bitset", &
                                    as_real128_returns_real128_with_the_same_bitset) &
                     , new_unittest("as_int128(), it should return int128_type with the same bitset", &
                                    as_int128_returns_int128_with_the_same_bitset) &
                     , new_unittest("raw_sign(), it should return 0 when input a positive value", &
                                    raw_sign_returns_0_when_input_positive_value) &
                     , new_unittest("raw_sign(), it should return 1 when input a negative value", &
                                    raw_sign_returns_1_when_input_negative_value) &
                     , new_unittest("abs(), it should return the same value when input a positive value", &
                                    abs_returns_same_value_when_input_positive_value) &
                     , new_unittest("abs(), it should return the absolute value when input a negative value", &
                                    abs_returns_absolute_value_when_input_negative_value) &
                     , new_unittest("is_distance_less_than_n_ulp_real128(), it should return true when 2 values are within "// &
                                    to_string(ULP)//" ulp.", &
                                    is_distance_less_than_n_ulp_real128_returns_true) &
                     , new_unittest("is_distance_less_than_n_ulp_real128(), it should return false "// &
                                    "when 2 values are not within "//to_string(ULP)//" ulp.", &
                                    is_distance_less_than_n_ulp_real128_returns_false) &
#endif
                     , new_unittest("are_close_real128(), it should return true when 2 values are within "// &
                                    "relative epsilon with factor "//to_string(ULP)//".", &
                                    are_close_real128_returns_T_when_within_relative_epsilon) &
                     , new_unittest("are_close_real128(), it should return false when 2 values are not within "// &
                                    "relative epsilon with factor "//to_string(ULP)//".", &
                                    are_close_real128_returns_F_when_not_within_relative_epsilon) &
                     , new_unittest("sign_real128(), it should return 0 when input a positive number", &
                                    sign_real128_returns_0_when_input_positive_number) &
                     , new_unittest("sign_real128(), it should return 1 when input a negative number", &
                                    sign_real128_returns_1_when_input_negative_number) &
                     ]
    end subroutine collect_fp
end module test_common_floatingPointNumber_collection
