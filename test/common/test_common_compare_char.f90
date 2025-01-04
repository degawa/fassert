program test_common_compare_char
    use :: test_common_compare_equal_char_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("common/compare/char", collect_equals_char) &
                  ]
    call run_test(test_suites)
end program test_common_compare_char
