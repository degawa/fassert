module fassert_common_floatingPointNumber_int128
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: new_int128_type
    public :: as_real128, as_int128

    !>128ビット整数を取り扱うための派生型．
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
        integer(int32), public :: parts(128/32)
            !! 4倍精度実数のビット列を格納するための配列．
    end type int128_type

    interface new_int128_type
        procedure :: construct_int128_from_real128
        procedure :: construct_int128_from_int32s
    end interface

    interface as_int128
        procedure :: construct_int128_from_real128
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

        new_int128%parts(:) = transfer(q, to_int32, size=128/32)
    end function construct_int128_from_real128

    !>4個の32 bit整数を`int128_type`に変換した変数を返す．
    pure function construct_int128_from_int32s(bits_96_127, bits_64_95, bits_32_63, bits_0_31) result(new_int128)
        implicit none
        integer(int32), intent(in) :: bits_96_127, bits_64_95, bits_32_63, bits_0_31
            !! 128 bit整数を構成する32 bit整数
        type(int128_type) :: new_int128
            !! 4倍精度実数を変換した`int128_type`変数

        new_int128%parts(1) = bits_0_31
        new_int128%parts(2) = bits_32_63
        new_int128%parts(3) = bits_64_95
        new_int128%parts(4) = bits_96_127
    end function construct_int128_from_int32s

    !>`int128_type`から変換された4倍精度実数を返す．
    pure elemental function as_real128(int128) result(q)
        implicit none
        type(int128_type), intent(in) :: int128
            !! 変換元の`int128_type`
        real(real128) :: q
            !! 変換後の実数

        q = transfer(int128%parts, q)
    end function as_real128
end module fassert_common_floatingPointNumber_int128
