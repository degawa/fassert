module test_charEqual_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_charEqual_expectCharEqual_unitTests_expect
    implicit none
    private
    public :: collect_expect_char_equal

contains
    subroutine collect_expect_char_equal(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("expect_char_equal(), success cases.", &
                                  char_equal_tests_success_cases) &
                     , new_unittest("expect_char_equal(), failure cases.", &
                                    char_equal_tests_failure_cases) &
                     , new_unittest("expect_char_equal(), expected failure cases.", &
                                    char_equal_tests_expected_failure_cases) &
                     ]

    end subroutine collect_expect_char_equal
end module test_charEqual_collection
