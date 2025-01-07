program test_assert_same_type
    use :: test_sameType_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("assert/sameType/expectSameType", collect_expect_same_type) &
                  ]
    call run_test(test_suites)
end program test_assert_same_type
