module fassert_common_floatingPointNumber_real128
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_floatingPointNumber_int128
    implicit none
    private
    public :: is_distance_less_than_n_ulp
    public :: absolute_distance_in_ulp_real128
    public :: are_close
    public :: sign

    interface is_distance_less_than_n_ulp
        procedure :: is_distance_less_than_n_ulp_real128
    end interface

    interface absolute_distance_in_ulp
        procedure :: absolute_distance_in_ulp_real128
    end interface

    interface as_int
        procedure :: real128_as_int128
    end interface

    interface are_close
        procedure :: are_close_real128
    end interface

    interface sign
        procedure :: real128_raw_sign
    end interface

contains
    !>二つの4倍精度実数が`ulp`ULP未満であれば`.true.`を返し，
    !>そうでない場合は`.false.`を返す．
    pure elemental function is_distance_less_than_n_ulp_real128(lhs, rhs, ulp)
        implicit none
        real(real128), intent(in) :: lhs
            !! 比較される値
        real(real128), intent(in) :: rhs
            !! 比較される値
        integer(int32), intent(in) :: ulp
            !! 二つの4倍精度実数を等しいと判定するULP
        logical :: is_distance_less_than_n_ulp_real128
            !! 比較結果

        integer(int64) :: dist_in_ulp
        dist_in_ulp = absolute_distance_in_ulp(lhs, rhs)

        is_distance_less_than_n_ulp_real128 = (dist_in_ulp < int(ulp, kind=int64))
    end function is_distance_less_than_n_ulp_real128

    !>二つの4倍精度実数の差の絶対値を計算し，ULP単位で返す．
    pure elemental function absolute_distance_in_ulp_real128(lhs, rhs) result(dist_in_ulp)
        use :: strith
        implicit none
        real(real128), intent(in) :: lhs
            !! 比較される値
        real(real128), intent(in) :: rhs
            !! 比較される値
        integer(int64) :: dist_in_ulp
            !! 差の絶対値（ULP単位）

        character(len=digits) :: abs_dist

        abs_dist = abs(to_string(as_int128(lhs)) - to_string(as_int128(rhs)))
        if (abs_dist .strithgt. weights_of_digits(64)-one)then !&
            dist_in_ulp = huge(dist_in_ulp)
        else
            dist_in_ulp = to_int64(abs_dist)
        end if
    end function absolute_distance_in_ulp_real128

    !>4倍精度実数を`int128_type`に変換して返す．
    pure elemental function real128_as_int128(q) result(i128)
        implicit none
        real(real128), intent(in) :: q
            !! 4倍精度実数
        type(int128_type) :: i128
            !! `int128_type`

        i128 = new_int128_type(q)
    end function real128_as_int128

    !>二つの4倍精度実数が，factor*min(相対イプシロン, spacing)未満であれば
    !>`.true.`を返し，そうでない場合は`.false.`を返す．
    pure elemental function are_close_real128(lhs, rhs, factor)
        implicit none
        real(real128), intent(in) :: lhs
            !! 比較される値
        real(real128), intent(in) :: rhs
            !! 比較される値
        integer(int32), intent(in) :: factor
            !! 基準値を調整するための因数
        logical :: are_close_real128
            !! 比較結果

        real(real128) :: abs_diff, relative_epsilon, space, f

        ! 相対イプシロン
        relative_epsilon = epsilon(lhs)*max(abs(lhs), abs(rhs))
        ! 浮動小数点数の区間
        space = max(spacing(lhs), spacing(rhs)) ! use max, not min
        f = real(factor, kind=real128)

        abs_diff = abs(lhs - rhs)
        are_close_real128 = (abs_diff < f*min(space, relative_epsilon))
    end function are_close_real128

    !>符号ビットの値を整数で返す．
    pure elemental function real128_raw_sign(q) result(sgn)
        implicit none
        real(real128), intent(in) :: q
            !! 4倍精度実数
        integer(int32) :: sgn
            !! 符号ビットの値

#if !defined(NAGFOR)
        sgn = raw_sign(new_int128_type(q))

#elif defined(NAGFOR)
        ! NAG Fortranの4倍精度実数はdouble-doubleであり，
        ! 下位64 bitが主に使われている．
        block
            integer(int64) :: i64(2)
            i64 = transfer(q, i64, size=2)
            sgn = ibits(i64(1), pos=63, len=1)
        end block

#endif
    end function real128_raw_sign
end module fassert_common_floatingPointNumber_real128
