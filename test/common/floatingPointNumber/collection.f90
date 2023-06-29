module test_common_floatingPointNumber_collection
    use :: testdrive, only:new_unittest, unittest_type, to_string
    use :: fassert_common_userSpecified
    use :: test_common_floatingPointNumber_unitTests_real32
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
                     , new_unittest("sign_real32(), it should return 0 when input a positive number", &
                                    sign_real32_returns_0_when_input_positive_number) &
                     , new_unittest("sign_real32(), it should return 1 when input a negative number", &
                                    sign_real32_returns_1_when_input_negative_number) &
                     ]
    end subroutine collect_fp
end module test_common_floatingPointNumber_collection
