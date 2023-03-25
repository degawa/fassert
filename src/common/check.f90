module fassert_common_check
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_optval
    use :: fassert_common_store
    use :: fassert_common_unit, msg_unit => assertion_message_unit
    use :: fassert_common_message
    implicit none
    private
    public :: check_true
    public :: check_expected_failure

    interface check_true
        procedure :: check_true_write_to_unit
        procedure :: check_true_write_to_string
    end interface

    interface check_expected_failure
        procedure :: check_expected_failure_write_to_unit
        procedure :: check_expected_failure_write_to_string
    end interface
contains
    !>`condition`が真かを判別し，
    !>真であれば`PASSED: テスト名`，
    !>偽であれば`FAILED: テスト名`を出力する．
    !>
    !>`stat`が存在していれば，真偽を書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine check_true_write_to_unit(condition, test_name, stat, quiet)
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
            call check_true_write_to_string(condition, test_name, &
                                            ! stat is not passed
                                            quiet=quiet, &
                                            output_message=msg_buffer)
            write (msg_unit, '(A)') msg_buffer
        end block
    end subroutine check_true_write_to_unit

    !>`condition`が偽かを判別し，
    !>偽であれば`PASSED: テスト名 [expected failure]`，
    !>真であれば`FAILED: テスト名 [unexpected success]`を出力する．
    !>
    !>`stat`が存在していれば，真偽（偽の時に真，真の時に偽）を書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine check_expected_failure_write_to_unit(condition, test_name, stat, quiet)
        use :: fassert_common_message
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
            call check_expected_failure_write_to_string(condition, test_name, &
                                                        ! stat is not passed
                                                        quiet=quiet, &
                                                        output_message=msg_buffer)
            write (msg_unit, '(A)') msg_buffer
        end block
    end subroutine check_expected_failure_write_to_unit

    !>`condition`が真かを判別し，
    !>真であれば`PASSED: テスト名`，
    !>偽であれば`FAILED: テスト名`を`output_message`に書き込む．
    !>
    !>`stat`が存在していれば，真偽を書き込む．
    !>
    !>`quiet`が真の場合，`output_message`を割り付けず，何も書き込まない．
    pure subroutine check_true_write_to_string(condition, test_name, stat, &
                                               quiet, output_message)
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
        character(:), allocatable, intent(out) :: output_message
            !! 結果を格納する文字列<br>
            !! 追記ではないので，`intent(out)`でよい．

        call store(stat, val=condition)

        if (does_not_output_message(quiet)) return

        block
            character(:), allocatable :: msg_buffer
            if (condition) then
                msg_buffer = prefix_passed//test_name
            else
                msg_buffer = prefix_failed//test_name
            end if

            output_message = msg_buffer
        end block
    end subroutine check_true_write_to_string

    !>`condition`が偽かを判別し，
    !>偽であれば`PASSED: テスト名 [expected failure]`，
    !>真であれば`FAILED: テスト名 [unexpected success]`を
    !>`output_message`に書き込む．
    !>
    !>`stat`が存在していれば，真偽（偽の時に真，真の時に偽）を書き込む．
    !>
    !>`quiet`が真の場合，`output_message`を割り付けず，何も書き込まない．
    pure subroutine check_expected_failure_write_to_string(condition, test_name, stat, &
                                                           quiet, output_message)
        use :: fassert_common_message
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
        character(:), allocatable, intent(out) :: output_message
            !! 結果を格納する文字列<br>
            !! 追記ではないので，`intent(out)`でよい．

        call store(stat, val=.not. condition)

        if (does_not_output_message(quiet)) return

        block
            character(:), allocatable :: msg_buffer
            if (.not. condition) then
                msg_buffer = prefix_passed//test_name//note_expected_failure
            else
                msg_buffer = prefix_failed//test_name//note_unexpected_pass
            end if

            output_message = msg_buffer
        end block
    end subroutine check_expected_failure_write_to_string
end module fassert_common_check
