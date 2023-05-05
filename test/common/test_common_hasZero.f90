program test_assert_common_hasZero
    use :: test_common_hasZero_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("assert/common/hasZero", collect_hasZero) &
                  ]
    call run_test(test_suites)
end program test_assert_common_hasZero
