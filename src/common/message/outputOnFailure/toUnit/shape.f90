module fassert_common_message_outputOnFailure_toUnit_shape
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_unit, msg_unit => assertion_message_unit
    use :: fassert_common_message_outputOnFailure_toString_shape
    implicit none
    private
    public :: output_on_failure

    interface output_on_failure
        procedure :: output_rank1
        procedure :: output_rank2
        procedure :: output_rank3
    end interface

contains
    !>配列形状の実測値と予測値を装置に出力する．
    subroutine output_rank1(actual, expected)
        implicit none
        class(*), intent(in) :: actual(:)
            !! 実測値
        class(*), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_rank1

    !>配列形状の実測値と予測値を装置に出力する．
    subroutine output_rank2(actual, expected)
        implicit none
        class(*), intent(in) :: actual(:, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_rank2

    !>配列形状の実測値と予測値を装置に出力する．
    subroutine output_rank3(actual, expected)
        implicit none
        class(*), intent(in) :: actual(:, :, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_rank3
end module fassert_common_message_outputOnFailure_toUnit_shape
