module assert_common_message
    implicit none
    private
    public :: to_string

    interface to_string
        procedure :: to_string_logical
    end interface

    character(*), private, parameter :: prefix_sep = ": "
        !! テスト結果とテスト名の間に入れる区切り文字
    character(*), public, parameter :: prefix_passed = "PASSED"//prefix_sep
        !! テストが成功した場合の接頭辞
    character(*), public, parameter :: prefix_failed = "FAILED"//prefix_sep
        !! テストが失敗した場合の接頭辞

    character(*), private, parameter :: note_sep = " "
        !! テスト名と注釈の間に入れる区切り文字
    character(*), private, parameter :: note_paren_open = "["
        !! 注釈を囲む括弧の開き側
    character(*), private, parameter :: note_paren_close = "]"
        !! 注釈を囲む括弧の閉じ側

    character(*), public, parameter :: note_expected_failure = note_sep//note_paren_open//"expected failure"//note_paren_close
        !! テストが予測通り失敗した場合に付ける注釈
    character(*), public, parameter :: note_unexpected_pass = note_sep//note_paren_open//"unexpected pass"//note_paren_close
        !! テストが予測に反して成功した場合に付ける注釈
    character(*), public, parameter :: note_shape_check = note_sep//note_paren_open//"shape check"//note_paren_close
        !! 配列の形状が一致していることを確認するテストに付ける注釈
    character(*), public, parameter :: note_length_check = note_sep//note_paren_open//"length check"//note_paren_close
        !! 文字列の長さが一致していることを確認するテストに付ける注釈

    character(*), public, parameter :: default_verbose_format_indent = "4x"
        !! `verbose`オプションで出力する情報の左インデント

    logical, public, parameter :: default_verbose = .false.
        !! 実測値と予測値の出力フラグの既定値
    logical, public, parameter :: default_quiet = .false.
        !! assertion message出力抑制フラグの既定値
    logical, public, parameter :: default_quiet_shape_check = .true.
        !! 形状チェック時のassertion message出力抑制フラグの既定値
contains
    !>`val`が`.true.`のとき文字列"T"，`.false.`のとき文字列"F"を返す．
    pure elemental character(1) function to_string_logical(val) result(string)
        implicit none
        logical, intent(in) :: val
            !! 文字列に変換する論理値

        if (val) then
            string = "T"
        else
            string = "F"
        end if
    end function to_string_logical
end module assert_common_message
