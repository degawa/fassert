module fassert_common_message_outputOnFailure_toUnit_shape
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_unit, msg_unit => assertion_message_unit
    use :: fassert_common_message, only:default_verbose_format_indent
    use :: fassert_common_message_outputOnFailure_format
    implicit none
    private
    public :: output_on_failure

    interface output_on_failure
        procedure :: output_rank1
        procedure :: output_rank2
        procedure :: output_rank3
    end interface

    character(*), private, parameter :: fmt_shape_rank1 = '('//fmt_indent//',A,"(",'//fmt_index_rank1//',")")'
    character(*), private, parameter :: fmt_shape_rank2 = '('//fmt_indent//',A,"(",'//fmt_index_rank2//',")")'
    character(*), private, parameter :: fmt_shape_rank3 = '('//fmt_indent//',A,"(",'//fmt_index_rank3//',")")'

contains
    !>配列形状の実測値と予測値を装置に出力する．
    subroutine output_rank1(actual, expected)
        implicit none
        class(*), intent(in) :: actual(:)
            !! 実測値
        class(*), intent(in) :: expected(:)
            !! 予測値

        write (msg_unit, fmt_shape_rank1) "Expected: ", shape(expected)
        write (msg_unit, fmt_shape_rank1) "Actual  : ", shape(actual)
    end subroutine output_rank1

    !>配列形状の実測値と予測値を装置に出力する．
    subroutine output_rank2(actual, expected)
        implicit none
        class(*), intent(in) :: actual(:, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :)
            !! 予測値

        write (msg_unit, fmt_shape_rank2) "Expected: ", shape(expected)
        write (msg_unit, fmt_shape_rank2) "Actual  : ", shape(actual)
    end subroutine output_rank2

    !>配列形状の実測値と予測値を装置に出力する．
    subroutine output_rank3(actual, expected)
        implicit none
        class(*), intent(in) :: actual(:, :, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :, :)
            !! 予測値

        write (msg_unit, fmt_shape_rank3) "Expected: ", shape(expected)
        write (msg_unit, fmt_shape_rank3) "Actual  : ", shape(actual)
    end subroutine output_rank3
end module fassert_common_message_outputOnFailure_toUnit_shape
