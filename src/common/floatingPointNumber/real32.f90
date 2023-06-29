module fassert_common_floatingPointNumber_real32
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_floatingPointNumber_bitLength, only:r32
    implicit none
    private
    public :: is_distance_less_than_n_ulp
    public :: sign

    integer(int32), private, parameter :: to_int32 = int(0, kind=int32)
        !! `transfer`を用いて単精度実数を
        !! 32ビット整数に変換するための指定元式

    interface is_distance_less_than_n_ulp
        procedure :: is_distance_less_than_n_ulp_real32
    end interface

    interface absolute_distance_in_ulp
        procedure :: absolute_distance_in_ulp_real32
    end interface

    interface as_int
        procedure :: real32_as_int32
    end interface

    interface sign
        procedure :: real32_raw_sign
    end interface
contains
    !>二つの単精度実数が`ulp`ULP未満であれば`.true.`を返し，
    !>そうでない場合は`.false.`を返す．
    pure elemental function is_distance_less_than_n_ulp_real32(lhs, rhs, ulp)
        implicit none
        real(real32), intent(in) :: lhs
            !! 比較される値
        real(real32), intent(in) :: rhs
            !! 比較される値
        integer(int32), intent(in) :: ulp
            !! 二つの単精度実数を等しいと判定するULP
        logical :: is_distance_less_than_n_ulp_real32
            !! 比較結果

        is_distance_less_than_n_ulp_real32 = (absolute_distance_in_ulp(lhs, rhs) < ulp)
    end function is_distance_less_than_n_ulp_real32

    !>二つの単精度実数の差の絶対値を計算し，ULP単位で返す．
    pure elemental function absolute_distance_in_ulp_real32(lhs, rhs) result(dist_in_ulp)
        implicit none
        real(real32), intent(in) :: lhs
            !! 比較される値
        real(real32), intent(in) :: rhs
            !! 比較される値
        integer(int32) :: dist_in_ulp
            !! 差の絶対値（ULP単位）

        dist_in_ulp = abs(as_int(lhs) - as_int(rhs))
    end function absolute_distance_in_ulp_real32

    !>単精度実数を32ビット整数に変換して返す．
    pure elemental function real32_as_int32(f) result(i32)
        implicit none
        real(real32), intent(in) :: f
            !! 単精度実数
        integer(int32) :: i32
            !! 32ビット整数

        i32 = transfer(f, to_int32)
    end function real32_as_int32

    !>符号ビットの値を整数で返す．
    pure elemental function real32_raw_sign(f) result(raw_sign)
        implicit none
        real(real32), intent(in) :: f
            !! 単精度実数
        integer(int32) :: raw_sign
            !! 符号ビットの値

        raw_sign = ibits(as_int(f), pos=r32%mantissa + r32%exponent, len=r32%sign)
    end function real32_raw_sign
end module fassert_common_floatingPointNumber_real32
