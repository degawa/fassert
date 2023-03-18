module fassert_common_halt
    use :: fassert_common_status
    implicit none
    private
    public :: halt_on_failure

contains
    !>テスト結果が失敗（`test_status`が偽）の場合，直ちにテストを停止する．
    subroutine halt_on_failure(test_status)
        implicit none
        logical, intent(in) :: test_status
            !! テスト結果

        if (is_test_failed(test_status)) &
            error stop
    end subroutine halt_on_failure
end module fassert_common_halt
