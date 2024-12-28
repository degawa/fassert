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
    public :: to_string

    interface to_string
        procedure :: to_string_int128
    end interface

contains
    !>`int128_type`変数を変換した文字列を返す．
    pure function to_string_int128(int128, remove_0_padding) result(str)
        use :: strith, only:strith__udt_to_string => to_string
        implicit none
        type(int128_type), intent(in) :: int128
            !! 文字列に変換したい`int128_type`
        logical, intent(in), optional :: remove_0_padding
            !! 変換された文字列から先頭の0埋めを削除するかのフラグ
        character(:), allocatable :: str
            !! 文字列に変換された`int128_type`

        str = strith__udt_to_string(int128, int128_to_string, remove_0_padding)
    contains
        pure subroutine int128_to_string(var, as_unsigned, strint)
            use :: strith, only: &
                weights_of_digits, digits, zero, &
                assignment(=), operator(+), operator(-)
            implicit none
            class(*), intent(in) :: var
                    !! 文字列に変換される任意型の変数
            logical, intent(in) :: as_unsigned
                    !! 符号なし整数として扱うかのフラグ．<br>
                    !! `to_string`の引数`as_unsigned`を引き継ぐ．
                    !! 引数が省略された場合は標準値として`.false.`が渡される．
            character(len=digits), intent(inout) :: strint
                    !! 文字列に変換された`var`

            integer(int32) :: i, bit
            integer(int32), parameter :: lower_digit_bit_sizes(2:4) = [bit_size(int128%parts(1)), &
                                                                       bit_size(int128%parts(1)) + &
                                                                       bit_size(int128%parts(2)), &
                                                                       bit_size(int128%parts(1)) + &
                                                                       bit_size(int128%parts(2)) + &
                                                                       bit_size(int128%parts(3))]

            strint = zero
            select type (var); type is (int128_type)
                ! 0-31 bits
                do i = 0, bit_size(var%parts(1)) - 1
                    bit = ibits(var%parts(1), pos=i, len=1)
                    if (bit == 1) strint = weights_of_digits(i) + strint
                end do
                ! 32-63 bits
                do i = 0, bit_size(var%parts(2)) - 1
                    bit = ibits(var%parts(2), pos=i, len=1)
                    if (bit == 1) strint = weights_of_digits(i + lower_digit_bit_sizes(2)) + strint
                end do
                ! 64-95 bits
                do i = 0, bit_size(var%parts(3)) - 1
                    bit = ibits(var%parts(3), pos=i, len=1)
                    if (bit == 1) strint = weights_of_digits(i + lower_digit_bit_sizes(3)) + strint
                end do
                ! 96-126 bits
                do i = 0, bit_size(var%parts(4)) - 2
                    bit = ibits(var%parts(4), pos=i, len=1)
                    if (bit == 1) strint = weights_of_digits(i + lower_digit_bit_sizes(4)) + strint
                end do
                ! 127 bit
                i = bit_size(var%parts(4)) - 1
                bit = ibits(var%parts(4), pos=i, len=1)
                if (as_unsigned) then
                    if (bit == 1) strint = weights_of_digits(i + lower_digit_bit_sizes(4)) + strint
                else
                    if (bit == 1) strint = strint - weights_of_digits(i + lower_digit_bit_sizes(4))
                end if
            end select
        end subroutine int128_to_string
    end function to_string_int128

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
#endif
end module test_common_floatingPointNumber_unitTests_int128
