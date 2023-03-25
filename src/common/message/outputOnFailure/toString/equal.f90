module fassert_common_message_outputOnFailure_toString_equal
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_message_outputOnFailure_format
    use :: fassert_common_store
    implicit none
    private
    public :: output_on_failure

    interface output_on_failure
        procedure :: output_int8_to_string
        procedure :: output_int16_to_string
        procedure :: output_int32_to_string
        procedure :: output_int64_to_string
        procedure :: output_real32_to_string
        procedure :: output_real64_to_string
        procedure :: output_int32_rank1_to_string
        procedure :: output_int32_rank2_to_string
        procedure :: output_int32_rank3_to_string
        procedure :: output_real32_rank1_to_string
        procedure :: output_real32_rank2_to_string
        procedure :: output_real32_rank3_to_string
        procedure :: output_real64_rank1_to_string
        procedure :: output_real64_rank2_to_string
        procedure :: output_real64_rank3_to_string
        procedure :: output_logical_to_string
        procedure :: output_string_to_string
        procedure :: output_char_rank1_to_string
    end interface

    character(*), private, parameter :: fmt_int = '('//fmt_indent//',A,I0)'
    character(*), private, parameter :: fmt_real = '('//fmt_indent//',A,g0)'
    character(*), private, parameter :: fmt_str = '('//fmt_indent//',A,A)'
    character(*), private, parameter :: fmt_char = '('//fmt_indent//',A,*(A:," "))'

contains
    !>実測値と予測値を文字列に出力する．
    pure subroutine output_int8_to_string(actual, expected, output_message)
        implicit none
        integer(int8), intent(in) :: actual
            !! 実測値
        integer(int8), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_int) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_int) "Actual  : ", actual
        call append(output_message, trim(buffer))
    end subroutine output_int8_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_int16_to_string(actual, expected, output_message)
        implicit none
        integer(int16), intent(in) :: actual
            !! 実測値
        integer(int16), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_int) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_int) "Actual  : ", actual
        call append(output_message, trim(buffer))
    end subroutine output_int16_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_int32_to_string(actual, expected, output_message)
        implicit none
        integer(int32), intent(in) :: actual
            !! 実測値
        integer(int32), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_int) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_int) "Actual  : ", actual
        call append(output_message, trim(buffer))
    end subroutine output_int32_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_int64_to_string(actual, expected, output_message)
        implicit none
        integer(int64), intent(in) :: actual
            !! 実測値
        integer(int64), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_int) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_int) "Actual  : ", actual
        call append(output_message, trim(buffer))
    end subroutine output_int64_to_string

    !>実測値と予測値，それらの差を文字列に出力する．
    pure subroutine output_real32_to_string(actual, expected, output_message)
        implicit none
        real(real32), intent(in) :: actual
            !! 実測値
        real(real32), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_real) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_real) "Actual  : ", actual
        call append(output_message, trim(buffer))
        write (buffer, fmt_real) "Difference: ", expected - actual
        call append(output_message, trim(buffer))
    end subroutine output_real32_to_string

    !>実測値と予測値，それらの差を文字列に出力する．
    pure subroutine output_real64_to_string(actual, expected, output_message)
        implicit none
        real(real64), intent(in) :: actual
            !! 実測値
        real(real64), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_real) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_real) "Actual  : ", actual
        call append(output_message, trim(buffer))
        write (buffer, fmt_real) "Difference:", expected - actual
        call append(output_message, trim(buffer))
    end subroutine output_real64_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int32_rank1_to_string(actual, expected, output_message)
        implicit none
        integer(int32), intent(in) :: actual(:)
            !! 実測値
        integer(int32), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        character(*), parameter :: fmt = '('//fmt_indent//',A,i0," at (",'//fmt_index_rank1//',")")'

        write (buffer, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int32_rank1_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int32_rank2_to_string(actual, expected, output_message)
        implicit none
        integer(int32), intent(in) :: actual(:, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        character(*), parameter :: fmt = '('//fmt_indent//',A,i0," at (",'//fmt_index_rank2//',")")'

        write (buffer, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int32_rank2_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int32_rank3_to_string(actual, expected, output_message)
        implicit none
        integer(int32), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        character(*), parameter :: fmt = '('//fmt_indent//',A,i0," at (",'//fmt_index_rank3//',")")'

        write (buffer, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int32_rank3_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_real32_rank1_to_string(actual, expected, output_message)
        implicit none
        real(real32), intent(in) :: actual(:)
            !! 実測値
        real(real32), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_index_rank1//',")")'

        write (buffer, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_real32_rank1_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_real32_rank2_to_string(actual, expected, output_message)
        implicit none
        real(real32), intent(in) :: actual(:, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_index_rank2//',")")'

        write (buffer, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_real32_rank2_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_real32_rank3_to_string(actual, expected, output_message)
        implicit none
        real(real32), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_index_rank3//',")")'

        write (buffer, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_real32_rank3_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_real64_rank1_to_string(actual, expected, output_message)
        implicit none
        real(real64), intent(in) :: actual(:)
            !! 実測値
        real(real64), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_index_rank1//',")")'

        write (buffer, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_real64_rank1_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_real64_rank2_to_string(actual, expected, output_message)
        implicit none
        real(real64), intent(in) :: actual(:, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_index_rank2//',")")'

        write (buffer, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_real64_rank2_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_real64_rank3_to_string(actual, expected, output_message)
        implicit none
        real(real64), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_index_rank3//',")")'

        write (buffer, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_real64_rank3_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_logical_to_string(actual, expected, output_message)
        use :: fassert_common_message, only:to_string
        implicit none
        logical, intent(in) :: actual
            !! 実測値
        logical, intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_str) "Expected: ", to_string(expected)
        call append(output_message, trim(buffer))
        write (buffer, fmt_str) "Actual  : ", to_string(actual)
        call append(output_message, trim(buffer))
    end subroutine output_logical_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_string_to_string(actual, expected, output_message)
        implicit none
        character(*), intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_str) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_str) "Actual  : ", actual
        call append(output_message, trim(buffer))
    end subroutine output_string_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_char_rank1_to_string(actual, expected, output_message)
        implicit none
        character, intent(in) :: actual(:)
            !! 実測値
        character, intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(128) :: buffer

        write (buffer, fmt_char) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_char) "Actual  : ", actual
        call append(output_message, trim(buffer))
    end subroutine output_char_rank1_to_string
end module fassert_common_message_outputOnFailure_toString_equal
