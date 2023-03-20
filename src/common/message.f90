module fassert_common_message
    use :: fassert_common_optval
    implicit none
    private
    public :: to_string
    public :: is_verbose_output
    public :: does_not_output_message
    public :: does_output_message

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

    character(*), public, parameter :: string_true = "T"
        !! 論理値`.ture.`を文字列に変換した値
    character(*), public, parameter :: string_false = "F"
        !! 論理値`.false.`を文字列に変換した値

    character(*), public, parameter :: NL = new_line("a")
        !! new line character
contains
    !>`val`が`.true.`のとき文字列"T"，`.false.`のとき文字列"F"を返す．
    pure elemental character(1) function to_string_logical(val) result(string)
        implicit none
        logical, intent(in) :: val
            !! 文字列に変換する論理値

        if (val) then
            string = string_true
        else
            string = string_false
        end if
    end function to_string_logical

    !>予測値や実測値を出力する条件の場合に`.true.`，
    !>そうでない場合`.false.`を返す．
    !>
    !>| test result | `verbose` | `quiet` | retval |
    !>| :---------: | :-------: | :-----: | :----: |
    !>|      f      |     -     |    -    |   T    |
    !>|      p      |     -     |    -    |   F    |
    !>|      f      |     T     |    -    |   T    |
    !>|      f      |     F     |    -    |   T    |
    !>|      p      |     T     |    -    |   T    |
    !>|      p      |     F     |    -    |   F    |
    !>|      f      |     -     |    T    |   T    |
    !>|      f      |     -     |    F    |   T    |
    !>|      f      |     T     |    T    |   T    |
    !>|      f      |     T     |    F    |   T    |
    !>|      f      |     F     |    T    |   F    |
    !>|      f      |     F     |    F    |   T    |
    !>|      p      |     -     |    T    |   F    |
    !>|      p      |     -     |    F    |   F    |
    !>|      p      |     T     |    T    |   T    |
    !>|      p      |     T     |    F    |   T    |
    !>|      p      |     F     |    T    |   F    |
    !>|      p      |     F     |    F    |   F    |
    !> p: test passed, f: test failed
    !> T: `.true.`, F: `.false.`, -: not present
    !>
    logical function is_verbose_output(stat, verbose, quiet)
        implicit none
        logical, intent(in) :: stat
        logical, intent(in), optional :: verbose
        logical, intent(in), optional :: quiet

        is_verbose_output = (.not. stat) .or. optval(verbose, default_verbose)

        if (present(verbose) .and. present(quiet)) then
            if (.not. verbose .and. quiet) then
                is_verbose_output = .false.
                return
            end if
        end if
    end function is_verbose_output

    !>引数で渡された条件を判別し，
    !>テスト結果を出力する場合に`.true.`そうでない場合`.false.`を返す．
    logical function does_output_message(quiet)
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
end module fassert_common_message
