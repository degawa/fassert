module fassette_common_check
    use, intrinsic :: iso_fortran_env
    use :: fassette_common_optval
    use :: fassette_common_store
    use :: fassette_common_unit, msg_unit => assertion_message_unit
    use :: fassette_common_message
    implicit none
    private
    public :: check_true
    public :: check_expected_failure

contains
    !>`condition`が真かを判別し，
    !>真であれば`PASSED: テスト名`，
    !>偽であれば`FAILED: テスト名`を出力する．
    !>
    !>`stat`が存在していれば，真偽を書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine check_true(condition, test_name, stat, quiet)
        logical, intent(in) :: condition
            !! 判別される条件
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out), optional :: stat
            !! 条件`condition`の真偽値<br>
            !! `condition`が真の場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: quiet
            !! `condition`が真の場合に
            !! 出力を抑制するフラグ

        call store(stat, val=condition)

        if (does_not_output_message(quiet)) return

        block
            character(:), allocatable :: msg_buffer
            if (condition) then
                msg_buffer = prefix_passed//test_name
            else
                msg_buffer = prefix_failed//test_name
            end if

            write (msg_unit, '(A)') msg_buffer
        end block
    end subroutine check_true

    !>`condition`が偽かを判別し，
    !>偽であれば`PASSED: テスト名 [expected failure]`，
    !>真であれば`FAILED: テスト名 [unexpected success]`を出力する．
    !>
    !>`stat`が存在していれば，真偽（偽の時に真，真の時に偽）を書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine check_expected_failure(condition, test_name, stat, quiet)
        use :: fassette_common_message
        logical, intent(in) :: condition
            !! 判別される条件
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out), optional :: stat
            !! 条件`condition`の真偽値<br>
            !! `expected_failure`が偽，`condition`が真の場合`.true.`，
            !! `expected_failure`が真，`condition`が偽の場合`.true.`，
            !! それ以外の場合`.false.`
        logical, intent(in), optional :: quiet
            !! `condition`が偽の場合に
            !! 出力を抑制するフラグ

        call store(stat, val=.not. condition)

        if (does_not_output_message(quiet)) return

        block
            character(:), allocatable :: msg_buffer
            if (.not. condition) then
                msg_buffer = prefix_passed//test_name//note_expected_failure
            else
                msg_buffer = prefix_failed//test_name//note_unexpected_pass
            end if

            write (msg_unit, '(A)') msg_buffer
        end block
    end subroutine check_expected_failure

    !>引数で渡された条件を判別し，
    !>テスト結果を出力する場合に`.true.`そうでない場合`.false.`を返す．
    logical function does_output_message(quiet)
        use :: fassette_common_message
        implicit none
        logical, intent(in), optional :: quiet
            !! 出力抑制フラグ

        does_output_message = .not. optval(quiet, default_quiet)
    end function does_output_message

    !>引数で渡された条件を判別し，
    !>テスト結果を出力しない場合に`.true.`そうでない場合`.false.`を返す．
    logical function does_not_output_message(quiet)
        implicit none
        logical, intent(in), optional :: quiet
            !! 出力抑制フラグ

        does_not_output_message = .not. does_output_message(quiet)
    end function does_not_output_message
end module fassette_common_check
