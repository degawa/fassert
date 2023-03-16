program test_assert_common
    use :: test_assert_common_unit_collection
    use :: test_assert_common_store_collection
    use :: test_assert_common_status_collection
    use :: test_assert_common_halt_collection
    use :: test_assert_common_optval_collection
    use :: test_assert_common_message_collection
    use :: test_assert_common_check_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("assert/common/unit", collect_unit) &
                  , new_testsuite("assert/common/store", collect_store) &
                  , new_testsuite("assert/common/status", collect_status) &
                  , new_testsuite("assert/common/halt", collect_halt) &
                  , new_testsuite("assert/common/optval", collect_optval) &
                  , new_testsuite("assert/common/message", collect_message) &
                  , new_testsuite("assert/common/check", collect_check) &
                  ]
    call run_test(test_suites)
end program test_assert_common
