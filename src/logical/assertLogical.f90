module assertLogical
    use :: expectLogical
    use :: fassert_common_halt
    implicit none
    private
    public :: assert_true
    public :: assert_false

contains
    !>実測値`actual`が真であるか検査する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，実測値が偽であることを検査し，
    !>プログラムを停止しない．実測値が真の場合停止する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    subroutine assert_true(actual, test_name, verbose, expected_failure, quiet)
        implicit none
        logical, intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を標準出力に出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 出力を抑制するかのフラグ

        logical :: stat

        call expect_true(actual, test_name, stat, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_true

    !>実測値`actual`が偽であるか検査する．
    !>
    !>`expected_failure`が真の場合，実測値が真であることを検査し，
    !>プログラムを停止しない．実測値が偽の場合停止する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    subroutine assert_false(actual, test_name, verbose, expected_failure, quiet)
        implicit none
        logical, intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を標準出力に出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 出力を抑制するかのフラグ

        logical :: stat

        call expect_false(actual, test_name, stat, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_false
end module assertLogical
