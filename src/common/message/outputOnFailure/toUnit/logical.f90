module fassert_common_message_outputOnFailure_toUnit_logical
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_unit, msg_unit => assertion_message_unit
    use :: fassert_common_message_outputOnFailure_toString_logical
    implicit none
    private
    public :: output_on_expect_true_failure
    public :: output_on_expect_false_failure

    interface output_on_expect_true_failure
        procedure :: output_true
        procedure :: output_true_rank1
        procedure :: output_true_rank2
        procedure :: output_true_rank3
    end interface

    interface output_on_expect_false_failure
        procedure :: output_false
        procedure :: output_false_rank1
        procedure :: output_false_rank2
        procedure :: output_false_rank3
    end interface

contains
    !>実測値と予測値を装置に出力する．
    subroutine output_true(actual)
        use :: fassert_common_message, only:to_string
        implicit none
        logical, intent(in) :: actual
            !! 実測値

        character(:), allocatable :: msg
        call output_on_expect_true_failure(actual, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_true

    !>実測値と予測値を装置に出力する．
    subroutine output_true_rank1(actual)
        implicit none
        logical, intent(in) :: actual(:)
            !! 実測値

        character(:), allocatable :: msg
        call output_on_expect_true_failure(actual, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_true_rank1

    !>実測値と予測値を装置に出力する．
    subroutine output_true_rank2(actual)
        implicit none
        logical, intent(in) :: actual(:, :)
            !! 実測値

        character(:), allocatable :: msg
        call output_on_expect_true_failure(actual, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_true_rank2

    !>実測値と予測値を装置に出力する．
    subroutine output_true_rank3(actual)
        implicit none
        logical, intent(in) :: actual(:, :, :)
            !! 実測値

        character(:), allocatable :: msg
        call output_on_expect_true_failure(actual, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_true_rank3


    !>実測値と予測値を装置に出力する．
    subroutine output_false(actual)
        implicit none
        logical, intent(in) :: actual
            !! 実測値

        character(:), allocatable :: msg
        call output_on_expect_false_failure(actual, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_false

    !>実測値と予測値を装置に出力する．
    subroutine output_false_rank1(actual)
        implicit none
        logical, intent(in) :: actual(:)
            !! 実測値

        character(:), allocatable :: msg
        call output_on_expect_false_failure(actual, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_false_rank1

    !>実測値と予測値を装置に出力する．
    subroutine output_false_rank2(actual)
        implicit none
        logical, intent(in) :: actual(:, :)
            !! 実測値

        character(:), allocatable :: msg
        call output_on_expect_false_failure(actual, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_false_rank2

    !>実測値と予測値を装置に出力する．
    subroutine output_false_rank3(actual)
        implicit none
        logical, intent(in) :: actual(:, :, :)
            !! 実測値

        character(:), allocatable :: msg
        call output_on_expect_false_failure(actual, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_false_rank3

end module fassert_common_message_outputOnFailure_toUnit_logical
