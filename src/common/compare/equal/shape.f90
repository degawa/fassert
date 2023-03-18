module fassert_common_compare_equal_shape
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: are_same_shape

    interface are_same_shape
        procedure :: are_same_shape_rank1_int32
        procedure :: are_same_shape_rank2_int32
        procedure :: are_same_shape_rank3_int32
        procedure :: are_same_shape_rank1_real32
        procedure :: are_same_shape_rank2_real32
        procedure :: are_same_shape_rank3_real32
        procedure :: are_same_shape_rank1_real64
        procedure :: are_same_shape_rank2_real64
        procedure :: are_same_shape_rank3_real64
        procedure :: are_same_shape_rank1_char
    end interface

contains
    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank1_int32(array1, array2) result(are_same)
        implicit none
        integer(int32), intent(in) :: array1(:)
        integer(int32), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank1_int32

    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank2_int32(array1, array2) result(are_same)
        implicit none
        integer(int32), intent(in) :: array1(:, :)
        integer(int32), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank2_int32

    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank3_int32(array1, array2) result(are_same)
        implicit none
        integer(int32), intent(in) :: array1(:, :, :)
        integer(int32), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank3_int32

    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank1_real32(array1, array2) result(are_same)
        implicit none
        real(real32), intent(in) :: array1(:)
        real(real32), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank1_real32

    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank2_real32(array1, array2) result(are_same)
        implicit none
        real(real32), intent(in) :: array1(:, :)
        real(real32), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank2_real32

    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank3_real32(array1, array2) result(are_same)
        implicit none
        real(real32), intent(in) :: array1(:, :, :)
        real(real32), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank3_real32

    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank1_real64(array1, array2) result(are_same)
        implicit none
        real(real64), intent(in) :: array1(:)
        real(real64), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank1_real64

    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank2_real64(array1, array2) result(are_same)
        implicit none
        real(real64), intent(in) :: array1(:, :)
        real(real64), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank2_real64

    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank3_real64(array1, array2) result(are_same)
        implicit none
        real(real64), intent(in) :: array1(:, :, :)
        real(real64), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank3_real64

    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank1_char(array1, array2) result(are_same)
        implicit none
        character, intent(in) :: array1(:)
        character, intent(in) :: array2(:)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank1_char
end module fassert_common_compare_equal_shape
