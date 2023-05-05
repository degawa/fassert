module fassert_common_compare_logical_false
    implicit none
    private
    public :: is_false
    public :: are_false

    interface are_false
        procedure :: are_all_false_rank1
        procedure :: are_all_false_rank2
        procedure :: are_all_false_rank3
    end interface

contains
    !>引数が偽の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function is_false(val)
        implicit none
        logical, intent(in) :: val
        logical :: is_false

        is_false = (val .eqv. .false.)
    end function is_false

    !>配列の全要素が全て偽の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_false_rank1(array) result(are_all_false)
        implicit none
        logical, intent(in) :: array(:)
        logical :: are_all_false

        are_all_false = all(array .eqv. .false.)
    end function are_all_false_rank1

    !>配列の全要素が全て偽の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_false_rank2(array) result(are_all_false)
        implicit none
        logical, intent(in) :: array(:, :)
        logical :: are_all_false

        are_all_false = all(array .eqv. .false.)
    end function are_all_false_rank2

    !>配列の全要素が全て偽の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_false_rank3(array) result(are_all_false)
        implicit none
        logical, intent(in) :: array(:, :, :)
        logical :: are_all_false

        are_all_false = all(array .eqv. .false.)
    end function are_all_false_rank3

end module fassert_common_compare_logical_false
