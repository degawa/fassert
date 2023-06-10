module fassert_common_compare_approx_array
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: are_approx_equal

    interface are_approx_equal
        procedure :: are_all_values_approx_equal_rank1_real32
        procedure :: are_all_values_approx_equal_rank2_real32
        procedure :: are_all_values_approx_equal_rank3_real32
        procedure :: are_all_values_approx_equal_rank1_real64
        procedure :: are_all_values_approx_equal_rank2_real64
        procedure :: are_all_values_approx_equal_rank3_real64
        procedure :: are_all_values_approx_equal_rank1_real128
        procedure :: are_all_values_approx_equal_rank2_real128
        procedure :: are_all_values_approx_equal_rank3_real128
        procedure :: are_all_values_approx_equal_rank1_complex32
        procedure :: are_all_values_approx_equal_rank2_complex32
        procedure :: are_all_values_approx_equal_rank3_complex32
        procedure :: are_all_values_approx_equal_rank1_complex64
        procedure :: are_all_values_approx_equal_rank2_complex64
        procedure :: are_all_values_approx_equal_rank3_complex64
        procedure :: are_all_values_approx_equal_rank1_complex128
        procedure :: are_all_values_approx_equal_rank2_complex128
        procedure :: are_all_values_approx_equal_rank3_complex128
    end interface

contains
    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank1_real32(array1, array2, tolerance) result(are_same)
        implicit none
        real(real32), intent(in) :: array1(:)
        real(real32), intent(in) :: array2(:)
        real(real32), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank1_real32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank2_real32(array1, array2, tolerance) result(are_same)
        implicit none
        real(real32), intent(in) :: array1(:, :)
        real(real32), intent(in) :: array2(:, :)
        real(real32), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank2_real32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank3_real32(array1, array2, tolerance) result(are_same)
        implicit none
        real(real32), intent(in) :: array1(:, :, :)
        real(real32), intent(in) :: array2(:, :, :)
        real(real32), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank3_real32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank1_real64(array1, array2, tolerance) result(are_same)
        implicit none
        real(real64), intent(in) :: array1(:)
        real(real64), intent(in) :: array2(:)
        real(real64), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank1_real64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank2_real64(array1, array2, tolerance) result(are_same)
        implicit none
        real(real64), intent(in) :: array1(:, :)
        real(real64), intent(in) :: array2(:, :)
        real(real64), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank2_real64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank3_real64(array1, array2, tolerance) result(are_same)
        implicit none
        real(real64), intent(in) :: array1(:, :, :)
        real(real64), intent(in) :: array2(:, :, :)
        real(real64), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank3_real64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank1_real128(array1, array2, tolerance) result(are_same)
        implicit none
        real(real128), intent(in) :: array1(:)
        real(real128), intent(in) :: array2(:)
        real(real128), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank1_real128

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank2_real128(array1, array2, tolerance) result(are_same)
        implicit none
        real(real128), intent(in) :: array1(:, :)
        real(real128), intent(in) :: array2(:, :)
        real(real128), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank2_real128

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank3_real128(array1, array2, tolerance) result(are_same)
        implicit none
        real(real128), intent(in) :: array1(:, :, :)
        real(real128), intent(in) :: array2(:, :, :)
        real(real128), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank3_real128

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank1_complex32(array1, array2, tolerance) result(are_same)
        implicit none
        complex(real32), intent(in) :: array1(:)
        complex(real32), intent(in) :: array2(:)
        real(real32), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank1_complex32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank2_complex32(array1, array2, tolerance) result(are_same)
        implicit none
        complex(real32), intent(in) :: array1(:, :)
        complex(real32), intent(in) :: array2(:, :)
        real(real32), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank2_complex32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank3_complex32(array1, array2, tolerance) result(are_same)
        implicit none
        complex(real32), intent(in) :: array1(:, :, :)
        complex(real32), intent(in) :: array2(:, :, :)
        real(real32), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank3_complex32

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank1_complex64(array1, array2, tolerance) result(are_same)
        implicit none
        complex(real64), intent(in) :: array1(:)
        complex(real64), intent(in) :: array2(:)
        real(real64), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank1_complex64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank2_complex64(array1, array2, tolerance) result(are_same)
        implicit none
        complex(real64), intent(in) :: array1(:, :)
        complex(real64), intent(in) :: array2(:, :)
        real(real64), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank2_complex64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank3_complex64(array1, array2, tolerance) result(are_same)
        implicit none
        complex(real64), intent(in) :: array1(:, :, :)
        complex(real64), intent(in) :: array2(:, :, :)
        real(real64), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank3_complex64

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank1_complex128(array1, array2, tolerance) result(are_same)
        implicit none
        complex(real128), intent(in) :: array1(:)
        complex(real128), intent(in) :: array2(:)
        real(real128), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank1_complex128

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank2_complex128(array1, array2, tolerance) result(are_same)
        implicit none
        complex(real128), intent(in) :: array1(:, :)
        complex(real128), intent(in) :: array2(:, :)
        real(real128), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank2_complex128

    !>二つの配列の全要素の差が許容値以下の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_values_approx_equal_rank3_complex128(array1, array2, tolerance) result(are_same)
        implicit none
        complex(real128), intent(in) :: array1(:, :, :)
        complex(real128), intent(in) :: array2(:, :, :)
        real(real128), intent(in) :: tolerance
        logical :: are_same

        are_same = all(abs(array1 - array2) <= tolerance)
    end function are_all_values_approx_equal_rank3_complex128

end module fassert_common_compare_approx_array
