program test_assert_char_equal
    use :: test_charEqual_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("assert/charEqual/expectCharEqual", collect_expect_char_equal) &
                  ]
    call run_test(test_suites)
end program test_assert_char_equal
