module fassert_common_compare_approx_complex
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: is_approx_equal

    interface is_approx_equal
        procedure :: is_approx_equal_complex32
        procedure :: is_approx_equal_complex64
        procedure :: is_approx_equal_complex128
    end interface

contains
    !>二つのスカラ値の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function is_approx_equal_complex32(val1, val2, tolerance) result(is_equal)
        implicit none
        complex(real32), intent(in) :: val1
        complex(real32), intent(in) :: val2
        real(real32), intent(in) :: tolerance
        logical :: is_equal

        is_equal = (abs(val1 - val2) <= tolerance)
    end function is_approx_equal_complex32

    !>二つのスカラ値の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function is_approx_equal_complex64(val1, val2, tolerance) result(is_equal)
        implicit none
        complex(real64), intent(in) :: val1
        complex(real64), intent(in) :: val2
        real(real64), intent(in) :: tolerance
        logical :: is_equal

        is_equal = (abs(val1 - val2) <= tolerance)
    end function is_approx_equal_complex64

    !>二つのスカラ値の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function is_approx_equal_complex128(val1, val2, tolerance) result(is_equal)
        implicit none
        complex(real128), intent(in) :: val1
        complex(real128), intent(in) :: val2
        real(real128), intent(in) :: tolerance
        logical :: is_equal

        is_equal = (abs(val1 - val2) <= tolerance)
    end function is_approx_equal_complex128

end module fassert_common_compare_approx_complex
