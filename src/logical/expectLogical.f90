module expectLogical
    use :: fassert_common_check
    use :: fassert_common_optval
    use :: fassert_common_status
    use :: fassert_common_message
    implicit none
    private
    public :: expect_true
    public :: expect_false

contains
    !>実測値`actual`が真であるか検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，実測値が偽であることを検査する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    subroutine expect_true(actual, test_name, stat, &
                           verbose, expected_failure, quiet)
        use :: fassert_common_message_outputOnFailure_equal
        implicit none
        logical, intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 検査結果の真偽値<br>
            !! 実測値が`.true.`の場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 出力を抑制するかのフラグ

        logical :: expected

        if (is_test_of_expected_failure(expected_failure)) then
            expected = .false.
            call check_expected_failure(actual, test_name, stat, quiet)
        else
            expected = .true.
            call check_true(actual, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_true

    !>実測値`actual`が偽であるか検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，実測値が真であることを検査する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    subroutine expect_false(actual, test_name, stat, &
                            verbose, expected_failure, quiet)
        use :: fassert_common_message_outputOnFailure_equal
        implicit none
        logical, intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 検査結果の真偽値<br>
            !! 実測値が`.false.`の場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 出力を抑制するかのフラグ

        logical :: expected

        if (is_test_of_expected_failure(expected_failure)) then
            expected = .true.
            call check_expected_failure(.not. actual, test_name, stat, quiet)
        else
            expected = .false.
            call check_true(.not. actual, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_false
end module expectLogical
