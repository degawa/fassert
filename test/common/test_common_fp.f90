program test_common_fp
    use :: test_common_floatingPointNumber_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("common/floatingPointNumber", collect_fp) &
                  ]
    call run_test(test_suites)
end program test_common_fp
