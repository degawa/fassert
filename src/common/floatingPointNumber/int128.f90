module fassert_common_floatingPointNumber_int128
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_floatingPointNumber_bitLength
    implicit none
    private
    public :: new_int128_type
    public :: raw_sign
    public :: abs
    public :: as_real128, as_int128
    public :: to_string

    !>128ビット整数を取り扱うための派生型．
    !>4倍精度実数の差をULP単位で計算する際に，
    !>4倍精度実数を整数に変換するために用いる事を想定．
    !>
    !>```console
    !>sign bit (1 bit)
    !>v   (15 bits)
    !>v +--exponent---+ +--------------------------------------------- mantissa (112 bits) --------------------------------------------+
    !>0 000000000000000 0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    !>^ --------------- ---------------^^------------------------------^^------------------------------^^------------------------------^
    !>              parts(4)                        parts(3)                         parts(2)                       parts(1)
    !>```
    type, public :: int128_type
        integer(int32), public :: parts(4)
            !! 4倍精度実数のビット列を格納するための配列．
    contains
        procedure, public, pass :: raw_sign
        !* 符号ビットの値を返却
        procedure, public, pass(lhs) :: subtract_each_part
        !* 減算を計算
        generic :: operator(-) => subtract_each_part
    end type int128_type

    interface abs
        procedure :: abs_int128
    end interface

    interface new_int128_type
        procedure :: construct_int128_from_real128
        procedure :: construct_int128_from_int32s
    end interface

    interface as_int128
        procedure :: construct_int128_from_real128
    end interface

    interface to_string
        procedure :: to_string_int128
    end interface

    integer(int32), private, parameter :: to_int32 = int(0, kind=int32)
        !! `transfer`を用いて4倍精度実数を
        !! 32ビット整数配列に変換するための指定元式

contains
    !>4倍精度実数を`int128_type`に変換した変数を返す．
    pure function construct_int128_from_real128(q) result(new_int128)
        implicit none
        real(real128), intent(in) :: q
            !! 4倍精度実数
        type(int128_type) :: new_int128
            !! 4倍精度実数を変換した`int128_type`変数

        new_int128%parts(:) = transfer(q, to_int32, size=4)
    end function construct_int128_from_real128

    !>4個の32 bit整数を`int128_type`に変換した変数を返す．
    pure function construct_int128_from_int32s(bits_96_127, bits_64_95, bits_32_63, bits_0_31) result(new_int128)
        implicit none
        integer(int32), intent(in) :: bits_96_127
        integer(int32), intent(in) :: bits_64_95
        integer(int32), intent(in) :: bits_32_63
        integer(int32), intent(in) :: bits_0_31
            !! 128 bit整数を構成する32 bit整数
        type(int128_type) :: new_int128
            !! 4倍精度実数を変換した`int128_type`変数

        new_int128%parts(1) = bits_0_31
        new_int128%parts(2) = bits_32_63
        new_int128%parts(3) = bits_64_95
        new_int128%parts(4) = bits_96_127
    end function construct_int128_from_int32s

    !>符号ビットの値を整数で返す．
    pure function raw_sign(this)
        implicit none
        class(int128_type), intent(in) :: this
            !! 当該実体仮引数
        integer(int32) :: raw_sign
            !! 符号ビットの値

        ! 最上位部分（parts(4)）の31ビット目が符号ビット
        raw_sign = ibits(this%parts(4), pos=31, len=1)
    end function raw_sign

    !>二つの`int128_type`変数の減算を計算して結果を`int128_type`で返す．
    !>
    !>二つの値の差がユーザ指定のULP以内であるかを調べることが目的のため，
    !>最下位部分の減算ができればよく，繰り下がりは考慮していない．
    !>
    !>@warning
    !>`f = 67329.2343750000000000000000000000000, g = g = f - spacing(f)`
    !>と設定した場合，各成分の値は，
    !>`as_int128(f)%parts = 00000000 00000000 3C000000 400F0701`,
    !>`as_int128(g)%parts = FFFFFFFF FFFFFFFF 3BFFFFFF 400F0701`,
    !>であり，各成分の減算結果は
    !>`00000001 00000001 00000001 00000000`
    !>となるので，最下位部分の減算をするだけでは不十分である．
    !>@endwarning
    !>
    pure elemental function subtract_each_part(lhs, rhs) result(sub)
        implicit none
        class(int128_type), intent(in) :: lhs
            !! 減算記号の左側の値．<br>
            !! 当該実体仮引数
        class(int128_type), intent(in) :: rhs
            !! 減算記号の右側の値．
        type(int128_type) :: sub
            !! 減算結果

        sub%parts = lhs%parts - rhs%parts
    end function subtract_each_part

    !>`int128_type`変数の絶対値を返す
    pure elemental function abs_int128(int128)
        implicit none
        class(int128_type), intent(in) :: int128
            !! 当該実体仮引数
        type(int128_type) :: abs_int128
            !! 絶対値

        abs_int128 = int128

        ! 符号ビットを0にクリアすることで絶対値を得る
        abs_int128%parts(4) = ibclr(abs_int128%parts(4), 31)
    end function abs_int128

    pure function to_string_int128(int128, remove_0_padding) result(str)
        use :: strith
        implicit none
        type(int128_type), intent(in) :: int128
            !! 文字列に変換したい`int128_type`
        logical, intent(in), optional :: remove_0_padding
        character(:), allocatable :: str

        str = to_string(int128, int128_to_string, remove_0_padding)
    contains
        pure subroutine int128_to_string(var, as_unsigned, strint)
            implicit none
            class(*), intent(in) :: var
            logical, intent(in) :: as_unsigned
            character(len=digits), intent(inout) :: strint

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

    pure elemental function as_real128(int128) result(q)
        implicit none
        type(int128_type), intent(in) :: int128
        real(real128) :: q

        q = transfer(int128%parts, q)
    end function as_real128
end module fassert_common_floatingPointNumber_int128
