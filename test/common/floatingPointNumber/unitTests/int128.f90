module test_common_floatingPointNumber_unitTests_int128
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_floatingPointNumber_int128
    implicit none
    private
#if !defined(NAGFOR)
    public :: construct_int128_returns_int128_type_instance
    public :: to_string_int128_returns_128bit_integer_in_string
    public :: as_real128_returns_real128_with_the_same_bitset
    public :: as_int128_returns_int128_with_the_same_bitset
    public :: raw_sign_returns_0_when_input_positive_value
    public :: raw_sign_returns_1_when_input_negative_value
    public :: abs_returns_same_value_when_input_positive_value
    public :: abs_returns_absolute_value_when_input_negative_value
    public :: subtract_returns_result_of_subtracting_each_part

contains
    subroutine construct_int128_returns_int128_type_instance(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: q
        type(int128_type) :: int128

        call check(error, same_type_as(new_int128_type(q), int128), &
                   "return value type from new_int128_from_real128() is not int128_type")
        if (occurred(error)) return

        call check(error, same_type_as(new_int128_type(0, 0, 0, 0), int128), &
                   "return value type from new_int128_from_int32s() is not int128_type")
        if (occurred(error)) return
    end subroutine construct_int128_returns_int128_type_instance

    subroutine to_string_int128_returns_128bit_integer_in_string(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(int128_type) :: int128
        character(:), allocatable :: str

        int128%parts(1) = int(Z"7FFFFFFF", kind=int32)
        int128%parts(2:4) = 0
        str = to_string(int128, remove_0_padding=.true.)
        call check(error, str == "2147483647", &
                   " converted string "//str//" from 0x 000000000 000000000 000000000 7FFFFFFF is not 2147483647")
        if (occurred(error)) return

        int128%parts(1) = int(Z"FFFFFFFF", kind=int32)
        int128%parts(2) = int(Z"7FFFFFFF", kind=int32)
        int128%parts(3:4) = 0
        str = to_string(int128, remove_0_padding=.true.)
        call check(error, str == "9223372036854775807", &
                   " converted string "//str//" from 0x 000000000 000000000 7FFFFFFF FFFFFFFF is not 9223372036854775807")
        if (occurred(error)) return

        int128%parts(1:3) = int(Z"FFFFFFFF", kind=int32)
        int128%parts(4) = int(Z"7FFFFFFF", kind=int32)
        str = to_string(int128, remove_0_padding=.true.)
        call check(error, str == "170141183460469231731687303715884105727", &
                   "converted string "//str//" from 0x 7FFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF&
                   &is not 170141183460469231731687303715884105727")
        if (occurred(error)) return
    end subroutine to_string_int128_returns_128bit_integer_in_string

    subroutine as_real128_returns_real128_with_the_same_bitset(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(int128_type) :: int128
        real(real128) :: q
        character(len=32) :: q_HEX

        int128 = new_int128_type(int(Z"12345678"), int(Z"90ABCDEF"), int(Z"FEDCBA09"), int(Z"87654321"))
        q = as_real128(int128)

        write (q_HEX, "(Z32.32)") q
        call check(error, q_HEX == "1234567890ABCDEFFEDCBA0987654321", &
                   "converted real128 "//q_HEX//" from 0x 12345678 90ABCDEF FEDCBA09 87654321&
                   &is not 1234567890ABCDEFFEDCBA0987654321")
        if (occurred(error)) return
    end subroutine as_real128_returns_real128_with_the_same_bitset

    subroutine as_int128_returns_int128_with_the_same_bitset(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: q
        type(int128_type) :: int128
        character(len=8) :: i32s(4)

        q = transfer([int(Z"9876543210FEDCBA", kind=int64), int(Z"ABCDEF0123456789", kind=int64)], q)

        int128 = as_int128(q)

        write (i32s(1), "(Z8.8)") int128%parts(1)
        write (i32s(2), "(Z8.8)") int128%parts(2)
        write (i32s(3), "(Z8.8)") int128%parts(3)
        write (i32s(4), "(Z8.8)") int128%parts(4)

        call check(error, all([(i32s(1) == "10FEDCBA"), &
                               (i32s(2) == "98765432"), &
                               (i32s(3) == "23456789"), &
                               (i32s(4) == "ABCDEF01")]), &
                   "converted int128 "//i32s(1)//i32s(2)//i32s(3)//i32s(4)// &
                   " from 0x ABCDEF01 23456789 98765432 10FEDCBA is not 10FEDCBA 98765432 23456789 ABCDEF01")
        if (occurred(error)) return
    end subroutine as_int128_returns_int128_with_the_same_bitset

    subroutine raw_sign_returns_0_when_input_positive_value(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: q
        type(int128_type) :: int128
        integer(int32) :: sign_value

        int128 = as_int128(huge(q))
        sign_value = raw_sign(int128)
        call check(error, sign_value == 0, &
                   "expected the sign bit of "//to_string(int128, remove_0_padding=.true.)// &
                   " is 0, but got "//to_string(sign_value))
        if (occurred(error)) return

        int128 = as_int128(tiny(q))
        sign_value = raw_sign(int128)
        call check(error, sign_value == 0, &
                   "expected the sign bit of "//to_string(int128, remove_0_padding=.true.)// &
                   " is 0, but got "//to_string(sign_value))
        if (occurred(error)) return

        int128 = as_int128(real(0, real128))
        sign_value = raw_sign(int128)
        call check(error, sign_value == 0, &
                   "expected the sign bit of "//to_string(int128, remove_0_padding=.true.)// &
                   " is 0, but got "//to_string(sign_value))
        if (occurred(error)) return
    end subroutine raw_sign_returns_0_when_input_positive_value

    subroutine raw_sign_returns_1_when_input_negative_value(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: q
        type(int128_type) :: int128
        integer(int32) :: sign_value

        int128 = as_int128(-huge(q))
        sign_value = raw_sign(int128)
        call check(error, sign_value == 1, &
                   "expected the sign bit of "//to_string(int128, remove_0_padding=.true.)// &
                   " is 1, but got "//to_string(sign_value))
        if (occurred(error)) return

        int128 = as_int128(-tiny(q))
        sign_value = raw_sign(int128)
        call check(error, sign_value == 1, &
                   "expected the sign bit of "//to_string(int128, remove_0_padding=.true.)// &
                   " is 1, but got "//to_string(sign_value))
        if (occurred(error)) return

        int128 = as_int128(-real(0, real128)) ! real(-0, real128) returns positive 0
        sign_value = raw_sign(int128)
        call check(error, sign_value == 1, &
                   "expected the sign bit of "//to_string(int128, remove_0_padding=.true.)// &
                   " is 1, but got "//to_string(sign_value))
        if (occurred(error)) return
    end subroutine raw_sign_returns_1_when_input_negative_value

    subroutine abs_returns_same_value_when_input_positive_value(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: q
        type(int128_type) :: int128, abs_int128
        integer(int32) :: sign_value
        character(len=32) :: q_HEX

        int128 = new_int128_type(int(Z"72345678"), int(Z"90ABCDEF"), int(Z"FEDCBA09"), int(Z"87654321"))

        abs_int128 = abs(int128)
        sign_value = raw_sign(abs_int128)
        q = as_real128(abs_int128)
        write (q_HEX, "(Z32.32)") q
        call check(error, sign_value == 0 .and. q_HEX == "7234567890ABCDEFFEDCBA0987654321", &
                   "expected 0x7234567890ABCDEFFEDCBA0987654321, but got "//q_HEX)
        if (occurred(error)) return
    end subroutine abs_returns_same_value_when_input_positive_value

    subroutine abs_returns_absolute_value_when_input_negative_value(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real128) :: q
        type(int128_type) :: int128, abs_int128
        integer(int32) :: sign_value
        character(len=32) :: q_HEX

        int128 = new_int128_type(int(Z"82345678"), int(Z"90ABCDEF"), int(Z"FEDCBA09"), int(Z"87654321"))

        abs_int128 = abs(int128)
        sign_value = raw_sign(abs_int128)
        q = as_real128(abs_int128)
        write (q_HEX, "(Z32.32)") q
        call check(error, sign_value == 0 .and. q_HEX == "0234567890ABCDEFFEDCBA0987654321", &
                   "expected 0x0234567890ABCDEFFEDCBA0987654321, but got "//q_HEX)
        if (occurred(error)) return
    end subroutine abs_returns_absolute_value_when_input_negative_value

    subroutine subtract_returns_result_of_subtracting_each_part(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(int128_type) :: a, b, c

        a = new_int128_type(int(Z"FFFFFFFF"), int(Z"FFFFFFFF"), int(Z"FFFFFFFF"), int(Z"FFFFFFFF"))
        b = new_int128_type(int(Z"00000000"), int(Z"00000000"), int(Z"00000000"), int(Z"00000000"))
        c = a - b
        call check(error, all(c%parts == -1), &
                   "expected [-1, -1, -1, -1], but got "// &
                   to_string(c%parts(1))//", "// &
                   to_string(c%parts(2))//", "// &
                   to_string(c%parts(3))//", "// &
                   to_string(c%parts(4)))
        if (occurred(error)) return

        a = new_int128_type(int(Z"00000000"), int(Z"00000000"), int(Z"00000000"), int(Z"00000000"))
        b = new_int128_type(int(Z"FFFFFFF0"), int(Z"FFFFFFF1"), int(Z"FFFFFFF2"), int(Z"FFFFFFF3"))
        c = a - b
        call check(error, all(c%parts == [13, 14, 15, 16]), &
                   "expected [13, 14, 15, 16], but got ["// &
                   to_string(c%parts(1))//", "// &
                   to_string(c%parts(2))//", "// &
                   to_string(c%parts(3))//", "// &
                   to_string(c%parts(4))//"]")
        if (occurred(error)) return
    end subroutine subtract_returns_result_of_subtracting_each_part
#endif
end module test_common_floatingPointNumber_unitTests_int128
