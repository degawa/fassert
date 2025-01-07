module expectErrorStop
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    interface expect_error_stop
        procedure :: expect_error_stop_wo_exit_status
        procedure :: expect_error_stop_w_exit_status
    end interface
contains
    subroutine expect_error_stop_w_exit_status(executable, exit_status, test_name, stat, &
                                               verbose, expected_failure, quiet)
        use :: expectEqual
        implicit none
        character(*), intent(in) :: executable
            !! 実行されるコマンド
        integer(int32), intent(in) :: exit_status
            !! コマンドの終了コード
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の配列形状を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ

        integer(int32) :: actual_exit_status
        call execute_command_line(command=construct_command(executable), &
                                  wait=.true., exitstat=actual_exit_status)
        call expect_equal(actual_exit_status, exit_status, test_name, stat, verbose, expected_failure, quiet)
    end subroutine expect_error_stop_w_exit_status

    subroutine expect_error_stop_wo_exit_status(executable, test_name, stat, &
                                                verbose, expected_failure, quiet)
        use :: expectEqual
        implicit none
        character(*), intent(in) :: executable
            !! 実行されるコマンド
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の配列形状を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ
        ! call expect_not_equal(actual_exit_status, 0, )
    end subroutine expect_error_stop_wo_exit_status

    function construct_command(executable) result(command)
        implicit none
        character(*), intent(in) :: executable
        character(:), allocatable :: command

        command = executable//' > '//get_descriptor()
    end function construct_command

    function get_descriptor() result(descriptor)
        implicit none
        character(:), allocatable :: descriptor

        descriptor = "NUL 2>&1" ! windows OS
#if defined(__unix__) || (__linux__)
        descriptor = "/dev/null 2>&1" ! unix/linux OS
#endif
    end function get_descriptor
end module expectErrorStop
