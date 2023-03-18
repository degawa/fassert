program halt_on_failure_w_passed_status
    use :: fassert_common_halt
    use :: fassert_common_status
    implicit none

    call halt_on_failure(passed)
end program halt_on_failure_w_passed_status
