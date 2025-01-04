module test_common_character_unitTests_toUpper
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_character_toUpper
    implicit none
    private
    public :: to_upper_converts_lower_case_to_upper_case

contains
    subroutine to_upper_converts_lower_case_to_upper_case(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, to_upper("a"), "A", &
                   "to_upper('a') should return 'A' but not")
        if (occurred(error)) return

        call check(error, to_upper("b"), "B", &
                   "to_upper('b') should return 'B' but not")
        if (occurred(error)) return

        call check(error, to_upper("c"), "C", &
                   "to_upper('c') should return 'C' but not")
        if (occurred(error)) return

        call check(error, to_upper("d"), "D", &
                   "to_upper('d') should return 'D' but not")
        if (occurred(error)) return

        call check(error, to_upper("e"), "E", &
                   "to_upper('e') should return 'E' but not")
        if (occurred(error)) return

        call check(error, to_upper("f"), "F", &
                   "to_upper('f') should return 'F' but not")
        if (occurred(error)) return

        call check(error, to_upper("g"), "G", &
                   "to_upper('g') should return 'G' but not")
        if (occurred(error)) return

        call check(error, to_upper("h"), "H", &
                   "to_upper('h') should return 'H' but not")
        if (occurred(error)) return

        call check(error, to_upper("i"), "I", &
                   "to_upper('i') should return 'I' but not")
        if (occurred(error)) return

        call check(error, to_upper("j"), "J", &
                   "to_upper('j') should return 'J' but not")
        if (occurred(error)) return

        call check(error, to_upper("k"), "K", &
                   "to_upper('k') should return 'K' but not")
        if (occurred(error)) return

        call check(error, to_upper("l"), "L", &
                   "to_upper('l') should return 'L' but not")
        if (occurred(error)) return

        call check(error, to_upper("m"), "M", &
                   "to_upper('m') should return 'M' but not")
        if (occurred(error)) return

        call check(error, to_upper("n"), "N", &
                   "to_upper('n') should return 'N' but not")
        if (occurred(error)) return

        call check(error, to_upper("o"), "O", &
                   "to_upper('o') should return 'O' but not")
        if (occurred(error)) return

        call check(error, to_upper("p"), "P", &
                   "to_upper('p') should return 'P' but not")
        if (occurred(error)) return

        call check(error, to_upper("q"), "Q", &
                   "to_upper('q') should return 'Q' but not")
        if (occurred(error)) return

        call check(error, to_upper("r"), "R", &
                   "to_upper('r') should return 'R' but not")
        if (occurred(error)) return

        call check(error, to_upper("s"), "S", &
                   "to_upper('s') should return 'S' but not")
        if (occurred(error)) return

        call check(error, to_upper("t"), "T", &
                   "to_upper('t') should return 'T' but not")
        if (occurred(error)) return

        call check(error, to_upper("u"), "U", &
                   "to_upper('u') should return 'U' but not")
        if (occurred(error)) return

        call check(error, to_upper("v"), "V", &
                   "to_upper('v') should return 'V' but not")
        if (occurred(error)) return

        call check(error, to_upper("w"), "W", &
                   "to_upper('w') should return 'W' but not")
        if (occurred(error)) return

        call check(error, to_upper("x"), "X", &
                   "to_upper('x') should return 'X' but not")
        if (occurred(error)) return

        call check(error, to_upper("y"), "Y", &
                   "to_upper('y') should return 'Y' but not")
        if (occurred(error)) return

        call check(error, to_upper("z"), "Z", &
                   "to_upper('z') should return 'Z' but not")
        if (occurred(error)) return

        call check(error, &
                   to_upper("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()-_=+\|`~[{]};:',<.>/?"), &
                   "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()-_=+\|`~[{]};:',<.>/?", &
                   "to_upper() should convert lower case to upper case")
        if (occurred(error)) return
    end subroutine to_upper_converts_lower_case_to_upper_case
end module test_common_character_unitTests_toUpper
