module fassert_common_floatingPointNumber_bitLength
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    !>IEEE[^ieee754]で定義されている浮動小数点数の
    !>符号，指数，仮数部のビット数を取り扱う派生型
    !>
    !>[^ieee754]: IEEE Computer Society, IEEE Standard for Floating-Point Arithmetic,
    !>IEEE, doi:10.1109/IEEESTD.2008.4610935, IEEE Std 754-2008
    type, public :: ieee_binary_bit_length_type
        integer(int32), public :: mantissa
            !! 仮数部のビット数
        integer(int32), public :: exponent
            !! 指数部のビット数
        integer(int32), public :: sign
            !! 符号ビットのビット数
    end type ieee_binary_bit_length_type

    type(ieee_binary_bit_length_type), public, parameter :: &
        r32 = ieee_binary_bit_length_type(mantissa=23, exponent=8, sign=1)
        !! 単精度実数の符号，指数，仮数部のビット数

    type(ieee_binary_bit_length_type), public, parameter :: &
        r64 = ieee_binary_bit_length_type(mantissa=52, exponent=11, sign=1)
        !! 倍精度実数の符号，指数，仮数部のビット数

    type(ieee_binary_bit_length_type), public, parameter :: &
        r128 = ieee_binary_bit_length_type(mantissa=112, exponent=15, sign=1)
        !! 4倍精度実数の符号，指数，仮数部のビット数
end module fassert_common_floatingPointNumber_bitLength
