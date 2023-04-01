module test_equal_expectEqual_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_equal_expectEqual_unitTests_expect_int
    use :: test_equal_expectEqual_unitTests_expect_real
    use :: test_equal_expectEqual_unitTests_expect_intD1
    use :: test_equal_expectEqual_unitTests_expect_intD2
    use :: test_equal_expectEqual_unitTests_expect_intD3
    use :: test_equal_expectEqual_unitTests_expect_realD1
    use :: test_equal_expectEqual_unitTests_expect_realD2
    use :: test_equal_expectEqual_unitTests_expect_realD3
    implicit none
    private
    public :: collect_expect_equal

contains
    subroutine collect_expect_equal(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        type(unittest_type), allocatable :: test_suite_int(:)

        test_suite_int = [ &
                         new_unittest("expect_equal_int(), parameterized test.", &
                                      expectEqual_int_parameterized_test) &
                         , new_unittest("expect_equal_real(), parameterized test.", &
                                        expectEqual_real_parameterized_test) &
                         , new_unittest("expect_equal_int_rank1(), parameterized test.", &
                                        expectEqual_intD1_parameterized_test) &
                         , new_unittest("expect_equal_int_rank2(), parameterized test.", &
                                        expectEqual_intD2_parameterized_test) &
                         , new_unittest("expect_equal_int_rank3(), parameterized test.", &
                                        expectEqual_intD3_parameterized_test) &
                         , new_unittest("expect_equal_real_rank1(), parameterized test.", &
                                        expectEqual_realD1_parameterized_test) &
                         , new_unittest("expect_equal_real_rank2(), parameterized test.", &
                                        expectEqual_realD2_parameterized_test) &
                         , new_unittest("expect_equal_real_rank3(), parameterized test.", &
                                        expectEqual_realD3_parameterized_test) &
                         ]

        test_suite = [test_suite_int]
    end subroutine collect_expect_equal
end module test_equal_expectEqual_collection
