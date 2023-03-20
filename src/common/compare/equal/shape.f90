module fassert_common_compare_equal_shape
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: are_same_shape

    interface are_same_shape
        procedure :: are_same_shape_rank1
        procedure :: are_same_shape_rank2
        procedure :: are_same_shape_rank3
    end interface

contains
    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank1(array1, array2) result(are_same)
        implicit none
        class(*), intent(in) :: array1(:)
        class(*), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank1

    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank2(array1, array2) result(are_same)
        implicit none
        class(*), intent(in) :: array1(:, :)
        class(*), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank2

    !>二つの配列の形状が等しい場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_same_shape_rank3(array1, array2) result(are_same)
        implicit none
        class(*), intent(in) :: array1(:, :, :)
        class(*), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function are_same_shape_rank3
end module fassert_common_compare_equal_shape
