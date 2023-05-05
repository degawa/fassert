module fassert_common_message_outputOnFailure_toString_logical
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_message_outputOnFailure_format
    use :: fassert_common_store
    implicit none
    private
    public :: output_on_expect_true_failure
    public :: output_on_expect_false_failure

    interface output_on_expect_true_failure
        procedure :: output_true_to_string
        procedure :: output_true_rank1_to_string
        procedure :: output_true_rank2_to_string
        procedure :: output_true_rank3_to_string
    end interface

    interface output_on_expect_false_failure
        procedure :: output_false_to_string
        procedure :: output_false_rank1_to_string
        procedure :: output_false_rank2_to_string
        procedure :: output_false_rank3_to_string
    end interface

contains
    !>実測値と予測値を文字列に出力する．
    pure subroutine output_true_to_string(actual, output_message)
        use :: fassert_common_message, only:to_string
        implicit none
        logical, intent(in) :: actual
            !! 実測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_str) "Expected: ", to_string(.true.)
        call append(output_message, trim(buffer))
        write (buffer, fmt_str) "Actual  : ", to_string(actual)
        call append(output_message, trim(buffer))
    end subroutine output_true_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_true_rank1_to_string(actual, output_message)
        use :: fassert_common_message, only:to_string
        use :: fassert_common_hasZero
        implicit none
        logical, intent(in) :: actual(:)
            !! 実測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual .eqv. .true.), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are true."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_str) "Expected: ", to_string(.true.)
            call append(output_message, trim(buffer))
            write (buffer, fmt_str) "Actual  : ", to_string(actual(idx(1)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank1) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_true_rank1_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_true_rank2_to_string(actual, output_message)
        use :: fassert_common_message, only:to_string
        use :: fassert_common_hasZero
        implicit none
        logical, intent(in) :: actual(:, :)
            !! 実測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual .eqv. .true.), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are true."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_str) "Expected: ", to_string(.true.)
            call append(output_message, trim(buffer))
            write (buffer, fmt_str) "Actual  : ", to_string(actual(idx(1), idx(2)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank2) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_true_rank2_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_true_rank3_to_string(actual, output_message)
        use :: fassert_common_message, only:to_string
        use :: fassert_common_hasZero
        implicit none
        logical, intent(in) :: actual(:, :, :)
            !! 実測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual .eqv. .true.), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are true."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_str) "Expected: ", to_string(.true.)
            call append(output_message, trim(buffer))
            write (buffer, fmt_str) "Actual  : ", to_string(actual(idx(1), idx(2), idx(3)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank3) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_true_rank3_to_string


    !>実測値と予測値を文字列に出力する．
    pure subroutine output_false_to_string(actual, output_message)
        use :: fassert_common_message, only:to_string
        implicit none
        logical, intent(in) :: actual
            !! 実測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_str) "Expected: ", to_string(.false.)
        call append(output_message, trim(buffer))
        write (buffer, fmt_str) "Actual  : ", to_string(actual)
        call append(output_message, trim(buffer))
    end subroutine output_false_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_false_rank1_to_string(actual, output_message)
        use :: fassert_common_message, only:to_string
        use :: fassert_common_hasZero
        implicit none
        logical, intent(in) :: actual(:)
            !! 実測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual .eqv. .false.), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are false."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_str) "Expected: ", to_string(.false.)
            call append(output_message, trim(buffer))
            write (buffer, fmt_str) "Actual  : ", to_string(actual(idx(1)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank1) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_false_rank1_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_false_rank2_to_string(actual, output_message)
        use :: fassert_common_message, only:to_string
        use :: fassert_common_hasZero
        implicit none
        logical, intent(in) :: actual(:, :)
            !! 実測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual .eqv. .false.), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are false."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_str) "Expected: ", to_string(.false.)
            call append(output_message, trim(buffer))
            write (buffer, fmt_str) "Actual  : ", to_string(actual(idx(1), idx(2)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank2) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_false_rank2_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_false_rank3_to_string(actual, output_message)
        use :: fassert_common_message, only:to_string
        use :: fassert_common_hasZero
        implicit none
        logical, intent(in) :: actual(:, :, :)
            !! 実測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual .eqv. .false.), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are false."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_str) "Expected: ", to_string(.false.)
            call append(output_message, trim(buffer))
            write (buffer, fmt_str) "Actual  : ", to_string(actual(idx(1), idx(2), idx(3)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank3) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_false_rank3_to_string

end module fassert_common_message_outputOnFailure_toString_logical
