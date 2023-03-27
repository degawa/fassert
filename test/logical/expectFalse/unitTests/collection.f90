module test_logical_expectFalse_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_logical_expectFalse_unitTests_expectFalse
    use :: test_logical_expectFalse_unitTests_expectFalseExpectedFailure
    use :: test_logical_expectFalse_unitTests_expectFalseMsg
    use :: test_logical_expectFalse_unitTests_expectFalseExpecFailMsg
    implicit none
    private
    public :: collect_expect_false

contains
    subroutine collect_expect_false(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("expect_false(), parameterized test.", &
                                  expectFalse_parameterized_test) &
                     , new_unittest("expect_false(expected_failure=.false.), parameterized test.", &
                                    expectFalse_expected_failure_F_parameterized_test) &
                     , new_unittest("expect_false(expected_failure=.true.), parameterized test.", &
                                    expectFalse_expected_failure_T_parameterized_test) &
                     , new_unittest("expect_false_write_to_message(), parameterized test.", &
                                    expectFalseMsg_parameterized_test) &
                     , new_unittest("expect_false(expected_failure=.false.), parameterized test.", &
                                    expectFalseMsg_expected_failure_F_parameterized_test) &
                     , new_unittest("expect_false(expected_failure=.true.), parameterized test.", &
                                    expectFalseMsg_expected_failure_T_parameterized_test) &
                     ]
    end subroutine collect_expect_false
end module test_logical_expectFalse_collection
