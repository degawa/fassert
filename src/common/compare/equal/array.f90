module fassert_common_compare_equal_array
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: are_equal

    interface are_equal
        procedure :: are_all_values_equal_rank1_int8
        procedure :: are_all_values_equal_rank2_int8
        procedure :: are_all_values_equal_rank3_int8
        procedure :: are_all_values_equal_rank1_int16
        procedure :: are_all_values_equal_rank2_int16
        procedure :: are_all_values_equal_rank3_int16
        procedure :: are_all_values_equal_rank1_int32
        procedure :: are_all_values_equal_rank2_int32
        procedure :: are_all_values_equal_rank3_int32
        procedure :: are_all_values_equal_rank1_int64
        procedure :: are_all_values_equal_rank2_int64
        procedure :: are_all_values_equal_rank3_int64
        procedure :: are_all_values_equal_rank1_real32
        procedure :: are_all_values_equal_rank2_real32
        procedure :: are_all_values_equal_rank3_real32
        procedure :: are_all_values_equal_rank1_real64
        procedure :: are_all_values_equal_rank2_real64
        procedure :: are_all_values_equal_rank3_real64
        procedure :: are_all_values_equal_rank1_real128
        procedure :: are_all_values_equal_rank2_real128
        procedure :: are_all_values_equal_rank3_real128
        procedure :: are_all_values_equal_rank1_complex32
        procedure :: are_all_values_equal_rank2_complex32
        procedure :: are_all_values_equal_rank3_complex32
        procedure :: are_all_values_equal_rank1_complex64
        procedure :: are_all_values_equal_rank2_complex64
        procedure :: are_all_values_equal_rank3_complex64
        procedure :: are_all_values_equal_rank1_complex128
        procedure :: are_all_values_equal_rank2_complex128
        procedure :: are_all_values_equal_rank3_complex128
        procedure :: are_all_values_equal_rank1_logical
        procedure :: are_all_values_equal_rank2_logical
        procedure :: are_all_values_equal_rank3_logical
        procedure :: are_all_values_equal_rank1_char
        procedure :: are_all_values_equal_rank2_char
        procedure :: are_all_values_equal_rank3_char
    end interface

contains
    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_int8(array1, array2) result(are_same)
        implicit none
        integer(int8), intent(in) :: array1(:)
        integer(int8), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank1_int8

    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_int8(array1, array2) result(are_same)
        implicit none
        integer(int8), intent(in) :: array1(:, :)
        integer(int8), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank2_int8

    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_int8(array1, array2) result(are_same)
        implicit none
        integer(int8), intent(in) :: array1(:, :, :)
        integer(int8), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank3_int8

    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_int16(array1, array2) result(are_same)
        implicit none
        integer(int16), intent(in) :: array1(:)
        integer(int16), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank1_int16

    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_int16(array1, array2) result(are_same)
        implicit none
        integer(int16), intent(in) :: array1(:, :)
        integer(int16), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank2_int16

    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_int16(array1, array2) result(are_same)
        implicit none
        integer(int16), intent(in) :: array1(:, :, :)
        integer(int16), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank3_int16

    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_int32(array1, array2) result(are_same)
        implicit none
        integer(int32), intent(in) :: array1(:)
        integer(int32), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank1_int32

    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_int32(array1, array2) result(are_same)
        implicit none
        integer(int32), intent(in) :: array1(:, :)
        integer(int32), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank2_int32

    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_int32(array1, array2) result(are_same)
        implicit none
        integer(int32), intent(in) :: array1(:, :, :)
        integer(int32), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank3_int32

    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_int64(array1, array2) result(are_same)
        implicit none
        integer(int64), intent(in) :: array1(:)
        integer(int64), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank1_int64

    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_int64(array1, array2) result(are_same)
        implicit none
        integer(int64), intent(in) :: array1(:, :)
        integer(int64), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank2_int64

    !>二つの配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_int64(array1, array2) result(are_same)
        implicit none
        integer(int64), intent(in) :: array1(:, :, :)
        integer(int64), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank3_int64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_real32(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real32
        use :: fassert_common_userSpecified
        implicit none
        real(real32), intent(in) :: array1(:)
        real(real32), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1, array2, ULP))
    end function are_all_values_equal_rank1_real32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_real32(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real32
        use :: fassert_common_userSpecified
        implicit none
        real(real32), intent(in) :: array1(:, :)
        real(real32), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1, array2, ULP))
    end function are_all_values_equal_rank2_real32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_real32(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real32
        use :: fassert_common_userSpecified
        implicit none
        real(real32), intent(in) :: array1(:, :, :)
        real(real32), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1, array2, ULP))
    end function are_all_values_equal_rank3_real32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_real64(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real64
        use :: fassert_common_userSpecified
        implicit none
        real(real64), intent(in) :: array1(:)
        real(real64), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1, array2, ULP))
    end function are_all_values_equal_rank1_real64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_real64(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real64
        use :: fassert_common_userSpecified
        implicit none
        real(real64), intent(in) :: array1(:, :)
        real(real64), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1, array2, ULP))
    end function are_all_values_equal_rank2_real64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_real64(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real64
        use :: fassert_common_userSpecified
        implicit none
        real(real64), intent(in) :: array1(:, :, :)
        real(real64), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1, array2, ULP))
    end function are_all_values_equal_rank3_real64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_real128(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real128
        use :: fassert_common_userSpecified
        implicit none
        real(real128), intent(in) :: array1(:)
        real(real128), intent(in) :: array2(:)
        logical :: are_same

#if defined(NAGFOR)
        are_same = all(are_close(array1, array2, factor=ULP))
#else
        are_same = all(is_distance_less_than_n_ulp(array1, array2, ULP))
#endif
    end function are_all_values_equal_rank1_real128

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_real128(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real128
        use :: fassert_common_userSpecified
        implicit none
        real(real128), intent(in) :: array1(:, :)
        real(real128), intent(in) :: array2(:, :)
        logical :: are_same

#if defined(NAGFOR)
        are_same = all(are_close(array1, array2, factor=ULP))
#else
        are_same = all(is_distance_less_than_n_ulp(array1, array2, ULP))
#endif
    end function are_all_values_equal_rank2_real128

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_real128(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real128
        use :: fassert_common_userSpecified
        implicit none
        real(real128), intent(in) :: array1(:, :, :)
        real(real128), intent(in) :: array2(:, :, :)
        logical :: are_same

#if defined(NAGFOR)
        are_same = all(are_close(array1, array2, factor=ULP))
#else
        are_same = all(is_distance_less_than_n_ulp(array1, array2, ULP))
#endif
    end function are_all_values_equal_rank3_real128

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_complex32(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real32
        use :: fassert_common_userSpecified
        implicit none
        complex(real32), intent(in) :: array1(:)
        complex(real32), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1%re, array2%re, ULP) .and. &
                       is_distance_less_than_n_ulp(array1%im, array2%im, ULP))
    end function are_all_values_equal_rank1_complex32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_complex32(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real32
        use :: fassert_common_userSpecified
        implicit none
        complex(real32), intent(in) :: array1(:, :)
        complex(real32), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1%re, array2%re, ULP) .and. &
                       is_distance_less_than_n_ulp(array1%im, array2%im, ULP))
    end function are_all_values_equal_rank2_complex32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_complex32(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real32
        use :: fassert_common_userSpecified
        implicit none
        complex(real32), intent(in) :: array1(:, :, :)
        complex(real32), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1%re, array2%re, ULP) .and. &
                       is_distance_less_than_n_ulp(array1%im, array2%im, ULP))
    end function are_all_values_equal_rank3_complex32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_complex64(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real64
        use :: fassert_common_userSpecified
        implicit none
        complex(real64), intent(in) :: array1(:)
        complex(real64), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1%re, array2%re, ULP) .and. &
                       is_distance_less_than_n_ulp(array1%im, array2%im, ULP))
    end function are_all_values_equal_rank1_complex64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_complex64(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real64
        use :: fassert_common_userSpecified
        implicit none
        complex(real64), intent(in) :: array1(:, :)
        complex(real64), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1%re, array2%re, ULP) .and. &
                       is_distance_less_than_n_ulp(array1%im, array2%im, ULP))
    end function are_all_values_equal_rank2_complex64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_complex64(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real64
        use :: fassert_common_userSpecified
        implicit none
        complex(real64), intent(in) :: array1(:, :, :)
        complex(real64), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(is_distance_less_than_n_ulp(array1%re, array2%re, ULP) .and. &
                       is_distance_less_than_n_ulp(array1%im, array2%im, ULP))
    end function are_all_values_equal_rank3_complex64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_complex128(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real128
        use :: fassert_common_userSpecified
        implicit none
        complex(real128), intent(in) :: array1(:)
        complex(real128), intent(in) :: array2(:)
        logical :: are_same

#if defined(NAGFOR)
        are_same = all(are_close(array1%re, array2%re, factor=ULP) .and. &
                       are_close(array1%im, array2%im, factor=ULP))
#else
        are_same = all(is_distance_less_than_n_ulp(array1%re, array2%re, ULP) .and. &
                       is_distance_less_than_n_ulp(array1%im, array2%im, ULP))
#endif
    end function are_all_values_equal_rank1_complex128

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_complex128(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real128
        use :: fassert_common_userSpecified
        implicit none
        complex(real128), intent(in) :: array1(:, :)
        complex(real128), intent(in) :: array2(:, :)
        logical :: are_same

#if defined(NAGFOR)
        are_same = all(are_close(array1%re, array2%re, factor=ULP) .and. &
                       are_close(array1%im, array2%im, factor=ULP))
#else
        are_same = all(is_distance_less_than_n_ulp(array1%re, array2%re, ULP) .and. &
                       is_distance_less_than_n_ulp(array1%im, array2%im, ULP))
#endif
    end function are_all_values_equal_rank2_complex128

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_complex128(array1, array2) result(are_same)
        use :: fassert_common_floatingPointNumber_real128
        use :: fassert_common_userSpecified
        implicit none
        complex(real128), intent(in) :: array1(:, :, :)
        complex(real128), intent(in) :: array2(:, :, :)
        logical :: are_same

#if defined(NAGFOR)
        are_same = all(are_close(array1%re, array2%re, factor=ULP) .and. &
                       are_close(array1%im, array2%im, factor=ULP))
#else
        are_same = all(is_distance_less_than_n_ulp(array1%re, array2%re, ULP) .and. &
                       is_distance_less_than_n_ulp(array1%im, array2%im, ULP))
#endif
    end function are_all_values_equal_rank3_complex128

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_logical(array1, array2) result(are_same)
        implicit none
        logical, intent(in) :: array1(:)
        logical, intent(in) :: array2(:)
        logical :: are_same

        are_same = all(array1 .eqv. array2)
    end function are_all_values_equal_rank1_logical

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_logical(array1, array2) result(are_same)
        implicit none
        logical, intent(in) :: array1(:, :)
        logical, intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(array1 .eqv. array2)
    end function are_all_values_equal_rank2_logical

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_logical(array1, array2) result(are_same)
        implicit none
        logical, intent(in) :: array1(:, :, :)
        logical, intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(array1 .eqv. array2)
    end function are_all_values_equal_rank3_logical

    !>二つの文字型配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank1_char(array1, array2) result(are_same)
        implicit none
        character(*), intent(in) :: array1(:)
        character(*), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank1_char

    !>二つの文字型配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank2_char(array1, array2) result(are_same)
        implicit none
        character(*), intent(in) :: array1(:, :)
        character(*), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank2_char

    !>二つの文字型配列の全要素が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_equal_rank3_char(array1, array2) result(are_same)
        implicit none
        character(*), intent(in) :: array1(:, :, :)
        character(*), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function are_all_values_equal_rank3_char

end module fassert_common_compare_equal_array
