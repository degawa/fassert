module fassert_common_compare_logical_true
    implicit none
    private
    public :: is_true
    public :: are_true

    interface are_true
        procedure :: are_all_true_rank1
        procedure :: are_all_true_rank2
        procedure :: are_all_true_rank3
    end interface

contains
    !>引数が真の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function is_true(val)
        implicit none
        logical, intent(in) :: val
        logical :: is_true

        is_true = (val .eqv. .true.)
    end function is_true

    !>配列の全要素が全て真の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_true_rank1(array) result(are_all_true)
        implicit none
        logical, intent(in) :: array(:)
        logical :: are_all_true

        are_all_true = all(array .eqv. .true.)
    end function are_all_true_rank1

    !>配列の全要素が全て真の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_true_rank2(array) result(are_all_true)
        implicit none
        logical, intent(in) :: array(:, :)
        logical :: are_all_true

        are_all_true = all(array .eqv. .true.)
    end function are_all_true_rank2

    !>配列の全要素が全て真の場合に`.true.`，そうでない場合`.false.`を返す．
    pure function are_all_true_rank3(array) result(are_all_true)
        implicit none
        logical, intent(in) :: array(:, :, :)
        logical :: are_all_true

        are_all_true = all(array .eqv. .true.)
    end function are_all_true_rank3

end module fassert_common_compare_logical_true
