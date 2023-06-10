program test_common_compare_approx_real
    use :: test_common_compare_approx_real_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("common/compare/approx/real", collect_is_approx_equal) &
                  ]
    call run_test(test_suites)
end program test_common_compare_approx_real
