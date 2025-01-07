module assertSameType
    use :: expectSameType
    use :: fassert_common_halt
    implicit none
    private
    public :: assert_same_type

    interface assert_same_type
        procedure :: assert_same_type_
        procedure :: assert_same_type_rank1
        procedure :: assert_same_type_rank2
        procedure :: assert_same_type_rank3
    end interface

contains
    !>実測値`actual`と予測値`expected`の型が同一かを検査する．
    !>検査結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`expected_failure`が真の場合，型が同一でないことを検査し，
    !>プログラムを停止しない．型が同一の場合，停止する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    subroutine assert_same_type_(actual, expected, test_name, &
                                 expected_failure, quiet)
        class(*), intent(in) :: actual
            !! 実測値
        class(*), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 出力を抑制するかのフラグ

        logical :: stat

        call expect_same_type(actual, expected, test_name, stat, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_same_type_

    !>実測値`actual`と予測値`expected`の型が同一かを検査する．
    !>検査結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`expected_failure`が真の場合，型が同一でないことを検査し，
    !>プログラムを停止しない．型が同一の場合，停止する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    subroutine assert_same_type_rank1(actual, expected, test_name, &
                                      expected_failure, quiet)
        class(*), intent(in) :: actual(:)
            !! 実測値
        class(*), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 出力を抑制するかのフラグ

        logical :: stat

        call expect_same_type(actual, expected, test_name, stat, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_same_type_rank1

    !>実測値`actual`と予測値`expected`の型が同一かを検査する．
    !>検査結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`expected_failure`が真の場合，型が同一でないことを検査し，
    !>プログラムを停止しない．型が同一の場合，停止する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    subroutine assert_same_type_rank2(actual, expected, test_name, &
                                      expected_failure, quiet)
        class(*), intent(in) :: actual(:, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 出力を抑制するかのフラグ

        logical :: stat

        call expect_same_type(actual, expected, test_name, stat, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_same_type_rank2

    !>実測値`actual`と予測値`expected`の型が同一かを検査する．
    !>検査結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`expected_failure`が真の場合，型が同一でないことを検査し，
    !>プログラムを停止しない．型が同一の場合，停止する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    subroutine assert_same_type_rank3(actual, expected, test_name, &
                                      expected_failure, quiet)
        class(*), intent(in) :: actual(:, :, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 出力を抑制するかのフラグ

        logical :: stat

        call expect_same_type(actual, expected, test_name, stat, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_same_type_rank3

end module assertSameType
