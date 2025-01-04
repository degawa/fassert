module test_common_character_unitTests_alphabeticalCharacter
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_character_alphabeticalCharacter
    implicit none
    private
    public :: lower_cases_have_26_alphabetical_characters
    public :: upper_cases_have_26_alphabetical_characters

contains
    subroutine lower_cases_have_26_alphabetical_characters(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, len(lower_case), 26, &
                   "lower_case should have 26 characters")

        call check(error, index(lower_case, "a") > 0, &
                   "lower_case should have 'a' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "b") > 0, &
                   "lower_case should have 'b' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "c") > 0, &
                   "lower_case should have 'c' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "d") > 0, &
                   "lower_case should have 'd' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "e") > 0, &
                   "lower_case should have 'e' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "f") > 0, &
                   "lower_case should have 'f' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "g") > 0, &
                   "lower_case should have 'g' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "h") > 0, &
                   "lower_case should have 'h' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "i") > 0, &
                   "lower_case should have 'i' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "j") > 0, &
                   "lower_case should have 'j' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "k") > 0, &
                   "lower_case should have 'k' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "l") > 0, &
                   "lower_case should have 'l' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "m") > 0, &
                   "lower_case should have 'm' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "n") > 0, &
                   "lower_case should have 'n' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "o") > 0, &
                   "lower_case should have 'o' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "p") > 0, &
                   "lower_case should have 'p' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "q") > 0, &
                   "lower_case should have 'q' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "r") > 0, &
                   "lower_case should have 'r' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "s") > 0, &
                   "lower_case should have 's' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "t") > 0, &
                   "lower_case should have 't' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "u") > 0, &
                   "lower_case should have 'u' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "v") > 0, &
                   "lower_case should have 'v' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "w") > 0, &
                   "lower_case should have 'w' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "x") > 0, &
                   "lower_case should have 'x' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "y") > 0, &
                   "lower_case should have 'y' but not")
        if (occurred(error)) return

        call check(error, index(lower_case, "z") > 0, &
                   "lower_case should have 'z' but not")
        if (occurred(error)) return
    end subroutine lower_cases_have_26_alphabetical_characters

    subroutine upper_cases_have_26_alphabetical_characters(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, len(upper_case), 26, &
                   "upper_case should have 26 characters")

        call check(error, index(upper_case, "A") > 0, &
                   "upper_case should have 'A' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "B") > 0, &
                   "upper_case should have 'B' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "C") > 0, &
                   "upper_case should have 'C' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "D") > 0, &
                   "upper_case should have 'D' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "E") > 0, &
                   "upper_case should have 'E' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "F") > 0, &
                   "upper_case should have 'F' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "G") > 0, &
                   "upper_case should have 'G' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "H") > 0, &
                   "upper_case should have 'H' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "I") > 0, &
                   "upper_case should have 'I' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "J") > 0, &
                   "upper_case should have 'J' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "K") > 0, &
                   "upper_case should have 'K' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "L") > 0, &
                   "upper_case should have 'L' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "M") > 0, &
                   "upper_case should have 'M' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "N") > 0, &
                   "upper_case should have 'N' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "O") > 0, &
                   "upper_case should have 'O' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "P") > 0, &
                   "upper_case should have 'P' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "Q") > 0, &
                   "upper_case should have 'Q' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "R") > 0, &
                   "upper_case should have 'R' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "S") > 0, &
                   "upper_case should have 'S' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "T") > 0, &
                   "upper_case should have 'T' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "U") > 0, &
                   "upper_case should have 'U' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "V") > 0, &
                   "upper_case should have 'V' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "W") > 0, &
                   "upper_case should have 'W' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "X") > 0, &
                   "upper_case should have 'X' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "Y") > 0, &
                   "upper_case should have 'Y' but not")
        if (occurred(error)) return

        call check(error, index(upper_case, "Z") > 0, &
                   "upper_case should have 'Z' but not")
        if (occurred(error)) return
    end subroutine upper_cases_have_26_alphabetical_characters
end module test_common_character_unitTests_alphabeticalCharacter
