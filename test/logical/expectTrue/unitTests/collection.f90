module test_logical_expectTrue_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_logical_expectTrue_unitTests_expectTrue
    use :: test_logical_expectTrue_unitTests_expectTrueExpectedFailure
    use :: test_logical_expectTrue_unitTests_expectTrueMsg
    use :: test_logical_expectTrue_unitTests_expectTrueExpectedFailureMsg
    implicit none
    private
    public :: collect_expect_true

contains
    subroutine collect_expect_true(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("expect_true(), parameterized test.", &
                                  expectTrue_parameterized_test) &
                     , new_unittest("expect_true(expected_failure=.false.), parameterized test.", &
                                    expectTrue_expected_failure_F_parameterized_test) &
                     , new_unittest("expect_true(expected_failure=.true.), parameterized test.", &
                                    expectTrue_expected_failure_T_parameterized_test) &
                     , new_unittest("expect_true_write_to_string(), parameterized test.", &
                                    expectTrueMsg_parameterized_test) &
                     , new_unittest("expect_true_write_to_string(expected_failure=.false.), parameterized test.", &
                                    expectTrueMsg_expected_failure_F_parameterized_test) &
                     , new_unittest("expect_true_write_to_string(expected_failure=.true.), parameterized test.", &
                                    expectTrueMsg_expected_failure_T_parameterized_test) &
                     ]
    end subroutine collect_expect_true
end module test_logical_expectTrue_collection
