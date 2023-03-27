program test_assert_logical
    use :: test_logical_expectTrue_collection
    use :: test_logical_expectFalse_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("assert/logical/expectTrue", collect_expect_true) &
                  , new_testsuite("assert/logical/expectFalse", collect_expect_false) &
                  ]
    call run_test(test_suites)
end program test_assert_logical
