module fassert_common_floatingPointNumber_real64
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_floatingPointNumber_range
    implicit none
    private
    public :: is_distance_less_than_n_ulp

    interface is_distance_less_than_n_ulp
        procedure :: is_distance_less_than_n_ulp_real64
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

        real(real64) :: lhs_lower_bound, lhs_upper_bound

        lhs_lower_bound = get_lower_bound(lhs, ulp) ! lhsから-ulp[ULP]離れた値
        lhs_upper_bound = get_upper_bound(lhs, ulp) ! lhsから+ulp[ULP]離れた値

        ! lhs - ulp[ULP] <= rhs <= lhs + ulp[ULP]であれば
        ! rhsとlhsの差の絶対値はulp[ULP]以内
        is_distance_less_than_n_ulp_real64 = (lhs_lower_bound <= rhs .and. rhs <= lhs_upper_bound)
    end function is_distance_less_than_n_ulp_real64
end module fassert_common_floatingPointNumber_real64
