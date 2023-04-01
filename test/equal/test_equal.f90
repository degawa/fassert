program test_assert_equal
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    use :: test_equal_expectEqual_collection
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("assert/equal/expectEqual", collect_expect_equal) &
                  ]
    call run_test(test_suites)
end program test_assert_equal
