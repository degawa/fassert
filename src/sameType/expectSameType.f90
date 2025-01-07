module expectSameType
    use :: fassert_common_check
    use :: fassert_common_status
    use :: fassert_common_message
    implicit none
    private
    public :: expect_same_type

    interface expect_same_type
        procedure :: expect_same_type_
        procedure :: expect_same_type_msg
        procedure :: expect_same_type_rank1
        procedure :: expect_same_type_rank1_msg
        procedure :: expect_same_type_rank2
        procedure :: expect_same_type_rank2_msg
        procedure :: expect_same_type_rank3
        procedure :: expect_same_type_rank3_msg
    end interface

    interface
        !>実測値`actual`と予測値`expected`の型が同一かを検査する．
        !>
        !>検査結果を`stat`に書き込む．
        !>
        !>`expected_failure`が真の場合，型が同一でないことを検査する．
        !>
        !>`quiet`が真の場合，出力を抑制する．
        !>
        module subroutine expect_same_type_(actual, expected, test_name, stat, &
                                            expected_failure, quiet)
            class(*), intent(in) :: actual
                !! 実測値
            class(*), intent(in) :: expected
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 検査結果の真偽値<br>
                !! 実測値が`.true.`の場合`.true.`，
                !! そうでない場合`.false.`
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 出力を抑制するかのフラグ
        end subroutine expect_same_type_

        !>実測値`actual`と予測値`expected`の型が同一かを検査する．
        !>
        !>検査結果を`stat`に書き込む．
        !>
        !>`expected_failure`が真の場合，型が同一でないことを検査する．
        !>
        !>`quiet`が真の場合，出力を抑制する．
        !>
        module subroutine expect_same_type_rank1(actual, expected, test_name, stat, &
                                                 expected_failure, quiet)
            class(*), intent(in) :: actual(:)
                !! 実測値
            class(*), intent(in) :: expected(:)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 検査結果の真偽値<br>
                !! 実測値が`.true.`の場合`.true.`，
                !! そうでない場合`.false.`
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 出力を抑制するかのフラグ
        end subroutine expect_same_type_rank1

        !>実測値`actual`と予測値`expected`の型が同一かを検査する．
        !>
        !>検査結果を`stat`に書き込む．
        !>
        !>`expected_failure`が真の場合，型が同一でないことを検査する．
        !>
        !>`quiet`が真の場合，出力を抑制する．
        !>
        module subroutine expect_same_type_rank2(actual, expected, test_name, stat, &
                                                 expected_failure, quiet)
            class(*), intent(in) :: actual(:, :)
                !! 実測値
            class(*), intent(in) :: expected(:, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 検査結果の真偽値<br>
                !! 実測値が`.true.`の場合`.true.`，
                !! そうでない場合`.false.`
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 出力を抑制するかのフラグ
        end subroutine expect_same_type_rank2

        !>実測値`actual`と予測値`expected`の型が同一かを検査する．
        !>
        !>検査結果を`stat`に書き込む．
        !>
        !>`expected_failure`が真の場合，型が同一でないことを検査する．
        !>
        !>`quiet`が真の場合，出力を抑制する．
        !>
        module subroutine expect_same_type_rank3(actual, expected, test_name, stat, &
                                                 expected_failure, quiet)
            class(*), intent(in) :: actual(:, :, :)
                !! 実測値
            class(*), intent(in) :: expected(:, :, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 検査結果の真偽値<br>
                !! 実測値が`.true.`の場合`.true.`，
                !! そうでない場合`.false.`
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 出力を抑制するかのフラグ
        end subroutine expect_same_type_rank3

    end interface

    interface
        !>実測値`actual`と予測値`expected`の型が同一かを検査し，
        !>出力を`output_message`に書き込む．
        !>
        !>検査結果を`stat`に書き込む．
        !>
        !>`expected_failure`が真の場合，型が同一でないことを検査する．
        !>
        !>`quiet`が真の場合，出力を抑制する．
        !>
        pure module subroutine expect_same_type_msg(actual, expected, test_name, stat, &
                                                    expected_failure, quiet, &
                                                    output_message)
            class(*), intent(in) :: actual
                !! 実測値
            class(*), intent(in) :: expected
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 検査結果の真偽値<br>
                !! 実測値が`.true.`の場合`.true.`，
                !! そうでない場合`.false.`
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_same_type_msg

        !>実測値`actual`と予測値`expected`の型が同一かを検査し，
        !>出力を`output_message`に書き込む．
        !>
        !>検査結果を`stat`に書き込む．
        !>
        !>`expected_failure`が真の場合，型が同一でないことを検査する．
        !>
        !>`quiet`が真の場合，出力を抑制する．
        !>
        pure module subroutine expect_same_type_rank1_msg(actual, expected, test_name, stat, &
                                                          expected_failure, quiet, &
                                                          output_message)
            class(*), intent(in) :: actual(:)
                !! 実測値
            class(*), intent(in) :: expected(:)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 検査結果の真偽値<br>
                !! 実測値が`.true.`の場合`.true.`，
                !! そうでない場合`.false.`
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_same_type_rank1_msg

        !>実測値`actual`と予測値`expected`の型が同一かを検査し，
        !>出力を`output_message`に書き込む．
        !>
        !>検査結果を`stat`に書き込む．
        !>
        !>`expected_failure`が真の場合，型が同一でないことを検査する．
        !>
        !>`quiet`が真の場合，出力を抑制する．
        !>
        pure module subroutine expect_same_type_rank2_msg(actual, expected, test_name, stat, &
                                                          expected_failure, quiet, &
                                                          output_message)
            class(*), intent(in) :: actual(:, :)
                !! 実測値
            class(*), intent(in) :: expected(:, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 検査結果の真偽値<br>
                !! 実測値が`.true.`の場合`.true.`，
                !! そうでない場合`.false.`
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_same_type_rank2_msg

        !>実測値`actual`と予測値`expected`の型が同一かを検査し，
        !>出力を`output_message`に書き込む．
        !>
        !>検査結果を`stat`に書き込む．
        !>
        !>`expected_failure`が真の場合，型が同一でないことを検査する．
        !>
        !>`quiet`が真の場合，出力を抑制する．
        !>
        pure module subroutine expect_same_type_rank3_msg(actual, expected, test_name, stat, &
                                                          expected_failure, quiet, &
                                                          output_message)
            class(*), intent(in) :: actual(:, :, :)
                !! 実測値
            class(*), intent(in) :: expected(:, :, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 検査結果の真偽値<br>
                !! 実測値が`.true.`の場合`.true.`，
                !! そうでない場合`.false.`
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_same_type_rank3_msg

    end interface
end module expectSameType
