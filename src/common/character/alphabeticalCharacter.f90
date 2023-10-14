module fassert_common_character_alphabeticalCharacter
    implicit none
    private

    character(*), public, parameter :: lower_case = "abcdefghijklmnopqrstuvwxyz"
        !! 小文字英字の集合．順序は昇順
    character(*), public, parameter :: upper_case = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        !! 大文字英字の集合．順序は昇順
end module fassert_common_character_alphabeticalCharacter
