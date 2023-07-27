module fassert_common_compare_equal_complex
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: is_equal

    interface is_equal
        procedure :: is_equal_complex32
        procedure :: is_equal_complex64
        procedure :: is_equal_complex128
    end interface

contains
    !>二つのスカラ値の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure elemental function is_equal_complex32(val1, val2) result(is_equal)
        use :: fassert_common_optval
        use :: fassert_common_floatingPointNumber_real32
        use :: fassert_common_userSpecified
        implicit none
        complex(real32), intent(in) :: val1
        complex(real32), intent(in) :: val2
        logical :: is_equal

        is_equal = (is_distance_less_than_n_ulp(val1%re, val2%re, ULP) .and. &
                    is_distance_less_than_n_ulp(val1%im, val2%im, ULP))
    end function is_equal_complex32

    !>二つのスカラ値の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure elemental function is_equal_complex64(val1, val2) result(is_equal)
        use :: fassert_common_optval
        use :: fassert_common_floatingPointNumber_real64
        use :: fassert_common_userSpecified
        implicit none
        complex(real64), intent(in) :: val1
        complex(real64), intent(in) :: val2
        logical :: is_equal

        is_equal = (is_distance_less_than_n_ulp(val1%re, val2%re, ULP) .and. &
                    is_distance_less_than_n_ulp(val1%im, val2%im, ULP))
    end function is_equal_complex64

    !>二つのスカラ値の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure elemental function is_equal_complex128(val1, val2) result(is_equal)
        use :: fassert_common_optval
        use :: fassert_common_floatingPointNumber_real128
        use :: fassert_common_userSpecified
        implicit none
        complex(real128), intent(in) :: val1
        complex(real128), intent(in) :: val2
        logical :: is_equal

#if defined(NAGFOR)
        is_equal = (are_close(val1%re, val2%re, factor=ULP) .and. &
                    are_close(val1%im, val2%im, factor=ULP))
#else
        is_equal = (is_distance_less_than_n_ulp(val1%re, val2%re, ULP) .and. &
                    is_distance_less_than_n_ulp(val1%im, val2%im, ULP))
#endif
    end function is_equal_complex128

end module fassert_common_compare_equal_complex
