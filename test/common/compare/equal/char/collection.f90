module test_common_compare_equal_char_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_compare_equal_char_unitTests_isEqual
    use :: test_common_compare_equal_char_unitTests_areEqual_D1
    use :: test_common_compare_equal_char_unitTests_areEqual_D2
    use :: test_common_compare_equal_char_unitTests_areEqual_D3
    implicit none
    private
    public :: collect_equals_char

contains
    subroutine collect_equals_char(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("is_equal(), parameterized test.", &
                                  isEqual_parameterized_test) &
                     , new_unittest("are_equal_rank1(), parameterized test.", &
                                    areEqual_D1_parameterized_test) &
                     , new_unittest("are_equal_rank2(), parameterized test.", &
                                    areEqual_D2_parameterized_test) &
                     , new_unittest("are_equal_rank3(), parameterized test.", &
                                    areEqual_D3_parameterized_test) &
                     ]
    end subroutine collect_equals_char
end module test_common_compare_equal_char_collection
