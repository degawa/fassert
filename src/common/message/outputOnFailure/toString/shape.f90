module fassert_common_message_outputOnFailure_toString_shape
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_message_outputOnFailure_format
    use :: fassert_common_store
    implicit none
    private
    public :: output_on_failure

    interface output_on_failure
        procedure :: output_rank1_to_string
        procedure :: output_rank2_to_string
        procedure :: output_rank3_to_string
    end interface

    character(*), private, parameter :: fmt_shape_rank1 = '('//fmt_indent//',A,"(",'//fmt_index_rank1//',")")'
    character(*), private, parameter :: fmt_shape_rank2 = '('//fmt_indent//',A,"(",'//fmt_index_rank2//',")")'
    character(*), private, parameter :: fmt_shape_rank3 = '('//fmt_indent//',A,"(",'//fmt_index_rank3//',")")'

contains
    !>配列形状の実測値と予測値を装置に出力する．
    pure subroutine output_rank1_to_string(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual(:)
            !! 実測値
        class(*), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_shape_rank1) "Expected Shape: ", shape(expected)
        call append(output_message, trim(buffer))
        write (buffer, fmt_shape_rank1) "Actual Shape  : ", shape(actual)
        call append(output_message, trim(buffer))
    end subroutine output_rank1_to_string

    !>配列形状の実測値と予測値を装置に出力する．
    pure subroutine output_rank2_to_string(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual(:, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_shape_rank2) "Expected Shape: ", shape(expected)
        call append(output_message, trim(buffer))
        write (buffer, fmt_shape_rank2) "Actual Shape  : ", shape(actual)
        call append(output_message, trim(buffer))
    end subroutine output_rank2_to_string

    !>配列形状の実測値と予測値を装置に出力する．
    pure subroutine output_rank3_to_string(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual(:, :, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_shape_rank3) "Expected Shape: ", shape(expected)
        call append(output_message, trim(buffer))
        write (buffer, fmt_shape_rank3) "Actual Shape  : ", shape(actual)
        call append(output_message, trim(buffer))
    end subroutine output_rank3_to_string
end module fassert_common_message_outputOnFailure_toString_shape
