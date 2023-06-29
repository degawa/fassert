module fassert_common_floatingPointNumber_real64
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_floatingPointNumber_bitLength, only:r64
    implicit none
    private
    public :: is_distance_less_than_n_ulp
    public :: sign

    integer(int64), private, parameter :: to_int64 = int(0, kind=int64)
        !! `transfer`を用いて倍精度実数を
        !! 64ビット整数に変換するための指定元式

    interface is_distance_less_than_n_ulp
        procedure :: is_distance_less_than_n_ulp_real64
    end interface

    interface absolute_distance_in_ulp
        procedure :: absolute_distance_in_ulp_real64
    end interface

    interface as_int
        procedure :: real64_as_int64
    end interface

    interface sign
        procedure :: real64_raw_sign
    end interface
contains
    !>二つの倍精度実数が`ulp`ULP未満であれば`.true.`を返し，
    !>そうでない場合は`.false.`を返す．
    pure elemental function is_distance_less_than_n_ulp_real64(lhs, rhs, ulp)
        implicit none
        real(real64), intent(in) :: lhs
            !! 比較される値
        real(real64), intent(in) :: rhs
            !! 比較される値
        integer(int32), intent(in) :: ulp
            !! 二つの倍精度実数を等しいと判定するULP
        logical :: is_distance_less_than_n_ulp_real64
            !! 比較結果

        is_distance_less_than_n_ulp_real64 = (absolute_distance_in_ulp(lhs, rhs) < ulp)
    end function is_distance_less_than_n_ulp_real64

    !>二つの倍精度実数の差の絶対値を計算し，ULP単位で返す．
    pure elemental function absolute_distance_in_ulp_real64(lhs, rhs) result(dist_in_ulp)
        implicit none
        real(real64), intent(in) :: lhs
            !! 比較される値
        real(real64), intent(in) :: rhs
            !! 比較される値
        integer(int64) :: dist_in_ulp
            !! 差の絶対値（ULP単位）

        dist_in_ulp = abs(as_int(lhs) - as_int(rhs))
    end function absolute_distance_in_ulp_real64

    !>倍精度実数を64ビット整数に変換して返す．
    pure elemental function real64_as_int64(d) result(i64)
        implicit none
        real(real64), intent(in) :: d
            !! 倍精度実数
        integer(int64) :: i64
            !! 64ビット整数

        i64 = transfer(d, to_int64)
    end function real64_as_int64

    !>符号ビットの値を整数で返す．
    pure elemental function real64_raw_sign(d) result(raw_sign)
        implicit none
        real(real64), intent(in) :: d
            !! 倍精度実数
        integer(int32) :: raw_sign
            !! 符号ビットの値

        raw_sign = int(ibits(as_int(d), pos=r64%mantissa + r64%exponent, len=r64%sign))
    end function real64_raw_sign
end module fassert_common_floatingPointNumber_real64
