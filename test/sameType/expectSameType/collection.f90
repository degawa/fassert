module test_sameType_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_sameType_expectSameType_unitTests_expect
    use :: test_sameType_expectSameType_unitTests_expect_D1
    use :: test_sameType_expectSameType_unitTests_expect_D2
    use :: test_sameType_expectSameType_unitTests_expect_D3
    implicit none
    private
    public :: collect_expect_same_type

contains
    subroutine collect_expect_same_type(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("expect_same_type(), success cases.", &
                                  expect_same_type_tests_success_cases) &
                     , new_unittest("expect_same_type(), failure cases.", &
                                    expect_same_type_tests_failure_cases) &
                     , new_unittest("expect_same_type(), expected failure cases.", &
                                    expect_same_type_tests_expected_failure_cases) &
                     , new_unittest("expect_same_type_rank1(), success cases.", &
                                    expect_same_type_d1_tests_success_cases) &
                     , new_unittest("expect_same_type_rank1(), failure cases.", &
                                    expect_same_type_d1_tests_failure_cases) &
                     , new_unittest("expect_same_type_rank1(), expected failure cases.", &
                                    expect_same_type_d1_tests_expected_failure_cases) &
                     , new_unittest("expect_same_type_rank2(), success cases.", &
                                    expect_same_type_d2_tests_success_cases) &
                     , new_unittest("expect_same_type_rank2(), failure cases.", &
                                    expect_same_type_d2_tests_failure_cases) &
                     , new_unittest("expect_same_type_rank2(), expected failure cases.", &
                                    expect_same_type_d2_tests_expected_failure_cases) &
                     , new_unittest("expect_same_type_rank3(), success cases.", &
                                    expect_same_type_d3_tests_success_cases) &
                     , new_unittest("expect_same_type_rank3(), failure cases.", &
                                    expect_same_type_d3_tests_failure_cases) &
                     , new_unittest("expect_same_type_rank3(), expected failure cases.", &
                                    expect_same_type_d3_tests_expected_failure_cases) &
                     ]

    end subroutine collect_expect_same_type
end module test_sameType_collection
