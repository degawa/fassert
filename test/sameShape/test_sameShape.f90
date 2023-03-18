program test_assert_sameShape
    use :: test_sameShape_expectSameShape_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("assert/sameShape/expectSameShape", collect_expect_same_shape) &
                  ]
    call run_test(test_suites)
end program test_assert_sameShape
