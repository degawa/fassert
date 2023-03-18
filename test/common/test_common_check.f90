program test_assert_common_check
    use :: test_common_check_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("assert/common/check", collect_check) &
                  ]
    call run_test(test_suites)
end program test_assert_common_check
