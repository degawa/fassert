module test_common_compare_equal_shape_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_compare_equal_shape_unitTests_areSameLength
    use :: test_common_compare_equal_shape_unitTests_areSameShape_D1
    use :: test_common_compare_equal_shape_unitTests_areSameShape_D2
    use :: test_common_compare_equal_shape_unitTests_areSameShape_D3
    implicit none
    private
    public :: collect_are_same_shape

contains
    subroutine collect_are_same_shape(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("are_same_shape_rank1(), parameterized test.", &
                                  areSameShapeD1_parameterized_test) &
                     , new_unittest("are_same_shape_rank2(), parameterized test.", &
                                    areSameShapeD2_parameterized_test) &
                     , new_unittest("are_same_shape_rank3(), parameterized test.", &
                                    areSameShapeD3_parameterized_test) &
                     , new_unittest("are_same_length_str(), parameterized test.", &
                                    areSameLengthStr_parameterized_test) &
                     ]
    end subroutine collect_are_same_shape
end module test_common_compare_equal_shape_collection
