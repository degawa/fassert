program test_common_compare_equal_shape
    use :: test_common_compare_equal_shape_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("common/compare/equal/shape", collect_are_same_shape) &
                  ]
    call run_test(test_suites)
end program test_common_compare_equal_shape
