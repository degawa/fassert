module assertCharEqual
    use, intrinsic :: iso_fortran_env
    use :: expectCharEqual
    use :: fassert_common_halt
    implicit none
    private
    public :: assert_char_equal

    interface assert_char_equal
        procedure :: assert_char_equal_
        procedure :: assert_char_equal_rank1
        procedure :: assert_char_equal_rank2
        procedure :: assert_char_equal_rank3
        procedure :: assert_char_equal_msg
        procedure :: assert_char_equal_rank1_msg
        procedure :: assert_char_equal_rank2_msg
        procedure :: assert_char_equal_rank3_msg
    end interface

contains
    !>実測値`actual`と予測値`expected`が同じ文字列かを検査する．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_char_equal_(actual, expected, test_name, &
                                  ignore_case, &
                                  verbose, expected_failure, quiet)
        character(*), intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: ignore_case
            !! 大文字小文字を区別するか否かのフラグ
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_char_equal(actual, expected, test_name, stat, &
                               ignore_case, &
                               verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_char_equal_

    !>実測値`actual`と予測値`expected`が同じ文字列かを検査する．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_char_equal_rank1(actual, expected, test_name, &
                                       ignore_case, &
                                       verbose, expected_failure, quiet)
        character(*), intent(in) :: actual(:)
            !! 実測値
        character(*), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: ignore_case
            !! 大文字小文字を区別するか否かのフラグ
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_char_equal(actual, expected, test_name, stat, &
                               ignore_case, &
                               verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_char_equal_rank1

    !>実測値`actual`と予測値`expected`が同じ文字列かを検査する．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_char_equal_rank2(actual, expected, test_name, &
                                       ignore_case, &
                                       verbose, expected_failure, quiet)
        character(*), intent(in) :: actual(:, :)
            !! 実測値
        character(*), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: ignore_case
            !! 大文字小文字を区別するか否かのフラグ
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_char_equal(actual, expected, test_name, stat, &
                               ignore_case, &
                               verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_char_equal_rank2

    !>実測値`actual`と予測値`expected`が同じ文字列かを検査する．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_char_equal_rank3(actual, expected, test_name, &
                                       ignore_case, &
                                       verbose, expected_failure, quiet)
        character(*), intent(in) :: actual(:, :, :)
            !! 実測値
        character(*), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: ignore_case
            !! 大文字小文字を区別するか否かのフラグ
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_char_equal(actual, expected, test_name, stat, &
                               ignore_case, &
                               verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_char_equal_rank3

    !>実測値`actual`と予測値`expected`が同じ文字列かを検査し，
    !>出力を`output_message`に書き込む．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_char_equal_msg(actual, expected, test_name, &
                                     ignore_case, &
                                     verbose, expected_failure, quiet, &
                                     output_message)
        character(*), intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: ignore_case
            !! 大文字小文字を区別するか否かのフラグ
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ
        character(:), allocatable, intent(out) :: output_message
            !! 出力を格納する文字列

        logical :: stat

        call expect_char_equal(actual, expected, test_name, stat, &
                               ignore_case, &
                               verbose, expected_failure, quiet, &
                               output_message)
        call halt_on_failure(stat)
    end subroutine assert_char_equal_msg

    !>実測値`actual`と予測値`expected`が同じ文字列かを検査し，
    !>出力を`output_message`に書き込む．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_char_equal_rank1_msg(actual, expected, test_name, &
                                           ignore_case, &
                                           verbose, expected_failure, quiet, &
                                           output_message)
        character(*), intent(in) :: actual(:)
            !! 実測値
        character(*), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: ignore_case
            !! 大文字小文字を区別するか否かのフラグ
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ
        character(:), allocatable, intent(out) :: output_message
            !! 出力を格納する文字列

        logical :: stat

        call expect_char_equal(actual, expected, test_name, stat, &
                               ignore_case, &
                               verbose, expected_failure, quiet, &
                               output_message)
        call halt_on_failure(stat)
    end subroutine assert_char_equal_rank1_msg

    !>実測値`actual`と予測値`expected`が同じ文字列かを検査し，
    !>出力を`output_message`に書き込む．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_char_equal_rank2_msg(actual, expected, test_name, &
                                           ignore_case, &
                                           verbose, expected_failure, quiet, &
                                           output_message)
        character(*), intent(in) :: actual(:, :)
            !! 実測値
        character(*), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: ignore_case
            !! 大文字小文字を区別するか否かのフラグ
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ
        character(:), allocatable, intent(out) :: output_message
            !! 出力を格納する文字列

        logical :: stat

        call expect_char_equal(actual, expected, test_name, stat, &
                               ignore_case, &
                               verbose, expected_failure, quiet, &
                               output_message)
        call halt_on_failure(stat)
    end subroutine assert_char_equal_rank2_msg

    !>実測値`actual`と予測値`expected`が同じ文字列かを検査し，
    !>出力を`output_message`に書き込む．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_char_equal_rank3_msg(actual, expected, test_name, &
                                           ignore_case, &
                                           verbose, expected_failure, quiet, &
                                           output_message)
        character(*), intent(in) :: actual(:, :, :)
            !! 実測値
        character(*), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: ignore_case
            !! 大文字小文字を区別するか否かのフラグ
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ
        character(:), allocatable, intent(out) :: output_message
            !! 出力を格納する文字列

        logical :: stat

        call expect_char_equal(actual, expected, test_name, stat, &
                               ignore_case, &
                               verbose, expected_failure, quiet, &
                               output_message)
        call halt_on_failure(stat)
    end subroutine assert_char_equal_rank3_msg
end module assertCharEqual
