module fassert_common_compare_approx_real
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: is_approx_equal

    interface is_approx_equal
        procedure :: is_approx_equal_real32
        procedure :: is_approx_equal_real64
        procedure :: is_approx_equal_real128
    end interface

contains
    !>二つのスカラ値の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function is_approx_equal_real32(val1, val2, tolerance) result(is_equal)
        implicit none
        real(real32), intent(in) :: val1
        real(real32), intent(in) :: val2
        real(real32), intent(in) :: tolerance
        logical :: is_equal

        is_equal = (abs(val1 - val2) <= tolerance)
    end function is_approx_equal_real32

    !>二つのスカラ値の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function is_approx_equal_real64(val1, val2, tolerance) result(is_equal)
        implicit none
        real(real64), intent(in) :: val1
        real(real64), intent(in) :: val2
        real(real64), intent(in) :: tolerance
        logical :: is_equal

        is_equal = (abs(val1 - val2) <= tolerance)
    end function is_approx_equal_real64

    !>二つのスカラ値の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function is_approx_equal_real128(val1, val2, tolerance) result(is_equal)
        implicit none
        real(real128), intent(in) :: val1
        real(real128), intent(in) :: val2
        real(real128), intent(in) :: tolerance
        logical :: is_equal

        is_equal = (abs(val1 - val2) <= tolerance)
    end function is_approx_equal_real128

end module fassert_common_compare_approx_real
