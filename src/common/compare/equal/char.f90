module fassert_common_compare_equal_char
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_character_toUpper
    use :: fassert_common_optval
    implicit none
    private
    public :: is_equal
    public :: are_equal

    interface is_equal
        procedure :: is_equal_char_
    end interface

    interface are_equal
        procedure :: are_all_values_equal_char_rank1
        procedure :: are_all_values_equal_char_rank2
        procedure :: are_all_values_equal_char_rank3
    end interface

contains
    !>二つの文字列が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    !>
    !>`check_len`が`.true.`の場合，まず二つの文字列の長さを比較する．
    !>そのため，比較結果はFortranの組み込み比較演算子`==`と異なる場合がある．
    !>`check_len`が`.false.`の場合は文字列の長さを比較しないので，
    !>文字列末尾の空白は比較結果に影響しない．
    !>`ignore_case`が`.true.`の場合，英字の大文字小文字を区別しない．
    pure elemental function is_equal_char_(val1, val2, ignore_case, check_len) result(is_equal)
        implicit none
        character(*), intent(in) :: val1
            !! 比較される文字列
        character(*), intent(in) :: val2
            !! 比較される文字列
        logical, intent(in) :: ignore_case
        !! 英字の大文字小文字を区別しない事を示すフラグ
        logical, intent(in), optional :: check_len
            !! 文字列の長さが等しいかを確認するフラグ
        logical :: is_equal
            !! 比較結果

        ! 文字列の長さが異なっていれば，等しくないとする
        if (optval(check_len, default=.false.) &
            .and. (len(val1) /= len(val2))) then
            is_equal = .false.
            return
        end if

        ! 大文字小文字を無視する場合は，全て大文字に変換して比較する
        if (ignore_case) then
            is_equal = (to_upper(val1) == to_upper(val2))
            return
        end if

        ! 大文字小文字を区別する場合は，そのまま比較する
        is_equal = (val1 == val2)
    end function is_equal_char_

    !>二つの文字列が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    !>
    !>`check_len`が`.true.`の場合，まず二つの文字列の長さを比較する．
    !>そのため，比較結果はFortranの組み込み比較演算子`==`と異なる場合がある．
    !>`check_len`が`.false.`の場合は文字列の長さを比較しないので，
    !>文字列末尾の空白は比較結果に影響しない．
    !>`ignore_case`が`.true.`の場合，英字の大文字小文字を区別しない．
    pure function are_all_values_equal_char_rank1(val1, val2, ignore_case, check_len) result(are_equal)
        implicit none
        character(*), intent(in) :: val1(1:)
            !! 比較される文字列
        character(*), intent(in) :: val2(1:)
            !! 比較される文字列
        logical, intent(in) :: ignore_case
            !! 英字の大文字小文字を区別しない事を示すフラグ
        logical, intent(in), optional :: check_len
            !! 文字列の長さが等しいかを確認するフラグ
        logical :: are_equal
            !! 比較結果

        ! 配列サイズが異なっていれば，等しくないとする
        if (any(shape(val1) /= shape(val2))) then
            are_equal = .false.
            return
        end if

        ! 文字列の長さが異なっていれば，等しくないとする
        if (optval(check_len, default=.false.) &
            .and. (len(val1) /= len(val2))) then
            are_equal = .false.
            return
        end if

        ! 大文字小文字を無視する場合は，全て大文字に変換して比較する
        if (ignore_case) then
            are_equal = all(to_upper(val1) == to_upper(val2))
            return
        end if

        ! 大文字小文字を区別する場合は，そのまま比較する
        are_equal = all(val1 == val2)
    end function are_all_values_equal_char_rank1

    !>二つの文字列が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    !>
    !>`check_len`が`.true.`の場合，まず二つの文字列の長さを比較する．
    !>そのため，比較結果はFortranの組み込み比較演算子`==`と異なる場合がある．
    !>`check_len`が`.false.`の場合は文字列の長さを比較しないので，
    !>文字列末尾の空白は比較結果に影響しない．
    !>`ignore_case`が`.true.`の場合，英字の大文字小文字を区別しない．
    pure function are_all_values_equal_char_rank2(val1, val2, ignore_case, check_len) result(are_equal)
        implicit none
        character(*), intent(in) :: val1(1:, 1:)
            !! 比較される文字列
        character(*), intent(in) :: val2(1:, 1:)
            !! 比較される文字列
        logical, intent(in) :: ignore_case
            !! 英字の大文字小文字を区別しない事を示すフラグ
        logical, intent(in), optional :: check_len
            !! 文字列の長さが等しいかを確認するフラグ
        logical :: are_equal
            !! 比較結果

        ! 配列サイズが異なっていれば，等しくないとする
        if (any(shape(val1) /= shape(val2))) then
            are_equal = .false.
            return
        end if

        ! 文字列の長さが異なっていれば，等しくないとする
        if (optval(check_len, default=.false.) &
            .and. (len(val1) /= len(val2))) then
            are_equal = .false.
            return
        end if

        ! 大文字小文字を無視する場合は，全て大文字に変換して比較する
        if (ignore_case) then
            are_equal = all(to_upper(val1) == to_upper(val2))
            return
        end if

        ! 大文字小文字を区別する場合は，そのまま比較する
        are_equal = all(val1 == val2)
    end function are_all_values_equal_char_rank2

    !>二つの文字列が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    !>
    !>`check_len`が`.true.`の場合，まず二つの文字列の長さを比較する．
    !>そのため，比較結果はFortranの組み込み比較演算子`==`と異なる場合がある．
    !>`check_len`が`.false.`の場合は文字列の長さを比較しないので，
    !>文字列末尾の空白は比較結果に影響しない．
    !>`ignore_case`が`.true.`の場合，英字の大文字小文字を区別しない．
    pure function are_all_values_equal_char_rank3(val1, val2, ignore_case, check_len) result(are_equal)
        implicit none
        character(*), intent(in) :: val1(1:, 1:, 1:)
            !! 比較される文字列
        character(*), intent(in) :: val2(1:, 1:, 1:)
            !! 比較される文字列
        logical, intent(in) :: ignore_case
            !! 英字の大文字小文字を区別しない事を示すフラグ
        logical, intent(in), optional :: check_len
            !! 文字列の長さが等しいかを確認するフラグ
        logical :: are_equal
            !! 比較結果

        ! 配列サイズが異なっていれば，等しくないとする
        if (any(shape(val1) /= shape(val2))) then
            are_equal = .false.
            return
        end if

        ! 文字列の長さが異なっていれば，等しくないとする
        if (optval(check_len, default=.false.) &
            .and. (len(val1) /= len(val2))) then
            are_equal = .false.
            return
        end if

        ! 大文字小文字を無視する場合は，全て大文字に変換して比較する
        if (ignore_case) then
            are_equal = all(to_upper(val1) == to_upper(val2))
            return
        end if

        ! 大文字小文字を区別する場合は，そのまま比較する
        are_equal = all(val1 == val2)
    end function are_all_values_equal_char_rank3
end module fassert_common_compare_equal_char
