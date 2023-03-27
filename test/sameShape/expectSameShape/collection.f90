module test_sameShape_expectSameShape_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_sameShape_expectSameShape_unitTests_expect_D1
    use :: test_sameShape_expectSameShape_unitTests_expect_D2
    use :: test_sameShape_expectSameShape_unitTests_expect_D3
    implicit none
    private
    public :: collect_expect_same_shape

contains
    subroutine collect_expect_same_shape(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("expect_same_shape_rank1(), parameterized test.", &
                                  expectSameShepeD1_parameterized_test) &
                     , new_unittest("expect_same_shape_rank2(), parameterized test.", &
                                    expectSameShepeD2_parameterized_test) &
                     , new_unittest("expect_same_shape_rank3(), parameterized test.", &
                                    expectSameShepeD3_parameterized_test) &
                     ]
    end subroutine collect_expect_same_shape
end module test_sameShape_expectSameShape_collection
