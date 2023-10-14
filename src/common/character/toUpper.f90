module fassert_common_character_toUpper
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_character_alphabeticalCharacter
    implicit none
    private
    public :: to_upper

contains
    !>文字列に含まれる英字を大文字に変換して返す．
    pure elemental function to_upper(str) result(upper_str)
        implicit none
        character(*), intent(in) :: str
            !! 文字列
        character(len=len(str)) :: upper_str
            !! 英字が大文字に変換された文字列

        integer(int32) :: pos, idx

        upper_str = str

        do pos = 1, len(str)
            idx = index(lower_case, str(pos:pos))

            if (idx > 0) then
                upper_str(pos:pos) = upper_case(idx:idx)
            end if
        end do
    end function to_upper
end module fassert_common_character_toUpper
