program halt_on_failure_w_failed_status
    use :: fassert_common_halt
    use :: fassert_common_status
    implicit none

    call halt_on_failure(failed)
end program halt_on_failure_w_failed_status
