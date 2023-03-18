module fassert_common_compare_equal_length
    implicit none
    private
    public :: are_same_length

    interface are_same_length
        procedure :: are_same_length_str
    end interface

contains
    !>二つの文字列の長さが等しい場合に`.true.`，そうでない場合`.false.`を返す．
    !>文字列の長さには，後方の空白は含まれない．
    pure function are_same_length_str(string1, string2) result(are_same)
        implicit none
        character(*), intent(in) :: string1
        character(*), intent(in) :: string2
        logical :: are_same

        are_same = (len_trim(string1) == len_trim(string2))
    end function are_same_length_str
end module fassert_common_compare_equal_length
