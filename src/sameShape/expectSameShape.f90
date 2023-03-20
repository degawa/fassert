module expectSameShape
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_check
    implicit none
    private
    public :: expect_same_shape

    interface expect_same_shape
        procedure :: expect_same_shape_rank1
        procedure :: expect_same_shape_rank2
        procedure :: expect_same_shape_rank3
    end interface

    interface
        !>二つの配列が同じ形状かを検査する．
        !>
        !>検査結果を`stat`に書き込む．
        !>
        !>`quiet`が真の場合，表示を抑制する．
        module subroutine expect_same_shape_rank1(actual, expected, test_name, stat, &
                                                  verbose, quiet)
            class(*), intent(in) :: actual(:)
                !! 実測値
            class(*), intent(in) :: expected(:)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値の配列形状が同じの場合`.true.`，
                !! そうでない場合`.false.`
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の配列形状を出力するフラグ
            logical, intent(in), optional :: quiet
                !! 表示を抑制するかのフラグ
        end subroutine expect_same_shape_rank1

        !>二つの配列が同じ形状かを検査する．
        !>
        !>検査結果を`stat`に書き込む．
        !>
        !>`quiet`が真の場合，表示を抑制する．
        module subroutine expect_same_shape_rank2(actual, expected, test_name, stat, &
                                                  verbose, quiet)
            class(*), intent(in) :: actual(:, :)
                !! 実測値
            class(*), intent(in) :: expected(:, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値の配列形状が同じの場合`.true.`，
                !! そうでない場合`.false.`
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の配列形状を出力するフラグ
            logical, intent(in), optional :: quiet
                !! 表示を抑制するかのフラグ
        end subroutine expect_same_shape_rank2

        !>二つの配列が同じ形状かを検査する．
        !>
        !>検査結果を`stat`に書き込む．
        !>
        !>`quiet`が真の場合，表示を抑制する．
        module subroutine expect_same_shape_rank3(actual, expected, test_name, stat, &
                                                  verbose, quiet)
            class(*), intent(in) :: actual(:, :, :)
                !! 実測値
            class(*), intent(in) :: expected(:, :, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値の配列形状が同じの場合`.true.`，
                !! そうでない場合`.false.`
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の配列形状を出力するフラグ
            logical, intent(in), optional :: quiet
                !! 表示を抑制するかのフラグ
        end subroutine expect_same_shape_rank3
    end interface
end module expectSameShape
