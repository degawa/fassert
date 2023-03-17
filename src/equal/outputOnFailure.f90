module fassette_equal_outputOnFailure
    use, intrinsic :: iso_fortran_env
    use :: fassette_common_unit, msg_unit => assertion_message_unit
    use :: fassette_common_message, only:default_verbose_format_indent
    implicit none
    private
    public :: output_on_failure

    interface output_on_failure
        procedure :: output_int8
        procedure :: output_int16
        procedure :: output_int32
        procedure :: output_int64
        procedure :: output_real32
        procedure :: output_real64
        procedure :: output_int32_rank1
        procedure :: output_int32_rank2
        procedure :: output_int32_rank3
        procedure :: output_real32_rank1
        procedure :: output_real32_rank2
        procedure :: output_real32_rank3
        procedure :: output_real64_rank1
        procedure :: output_real64_rank2
        procedure :: output_real64_rank3
        procedure :: output_logical
        procedure :: output_string
        procedure :: output_char_rank1
    end interface

    character(*), private, parameter :: fmt_indent = default_verbose_format_indent
    character(*), private, parameter :: fmt_int = '('//fmt_indent//',A,I0)'
    character(*), private, parameter :: fmt_real = '('//fmt_indent//',A,g0)'
    character(*), private, parameter :: fmt_str = '('//fmt_indent//',A,A)'
    character(*), private, parameter :: fmt_char = '('//fmt_indent//',A,*(A:," "))'

    character(*), private, parameter :: fmt_idx_sep = ',",",'
    character(*), private, parameter :: fmt_idx_d1 = 'i0'
    character(*), private, parameter :: fmt_idx_d2 = fmt_idx_d1//repeat(fmt_idx_sep//fmt_idx_d1, 2 - 1)
    character(*), private, parameter :: fmt_idx_d3 = fmt_idx_d1//repeat(fmt_idx_sep//fmt_idx_d1, 3 - 1)
contains
    !>実測値と予測値を標準出力に出力する．
    subroutine output_int8(actual, expected)
        implicit none
        integer(int8), intent(in) :: actual
            !! 実測値
        integer(int8), intent(in) :: expected
            !! 予測値

        write (msg_unit, fmt_int) "Expected: ", expected
        write (msg_unit, fmt_int) "Actual  : ", actual
    end subroutine output_int8

    !>実測値と予測値を標準出力に出力する．
    subroutine output_int16(actual, expected)
        implicit none
        integer(int16), intent(in) :: actual
            !! 実測値
        integer(int16), intent(in) :: expected
            !! 予測値

        write (msg_unit, fmt_int) "Expected: ", expected
        write (msg_unit, fmt_int) "Actual  : ", actual
    end subroutine output_int16

    !>実測値と予測値を標準出力に出力する．
    subroutine output_int32(actual, expected)
        implicit none
        integer(int32), intent(in) :: actual
            !! 実測値
        integer(int32), intent(in) :: expected
            !! 予測値

        write (msg_unit, fmt_int) "Expected: ", expected
        write (msg_unit, fmt_int) "Actual  : ", actual
    end subroutine output_int32

    !>実測値と予測値を標準出力に出力する．
    subroutine output_int64(actual, expected)
        implicit none
        integer(int64), intent(in) :: actual
            !! 実測値
        integer(int64), intent(in) :: expected
            !! 予測値

        write (msg_unit, fmt_int) "Expected: ", expected
        write (msg_unit, fmt_int) "Actual  : ", actual
    end subroutine output_int64

    !>実測値と予測値，それらの差を標準出力に出力する．
    subroutine output_real32(actual, expected)
        implicit none
        real(real32), intent(in) :: actual
            !! 実測値
        real(real32), intent(in) :: expected
            !! 予測値

        write (msg_unit, fmt_real) "Expected: ", expected
        write (msg_unit, fmt_real) "Actual  : ", actual
        write (msg_unit, fmt_real) "Difference: ", expected - actual
    end subroutine output_real32

    !>実測値と予測値，それらの差を標準出力に出力する．
    subroutine output_real64(actual, expected)
        implicit none
        real(real64), intent(in) :: actual
            !! 実測値
        real(real64), intent(in) :: expected
            !! 予測値

        write (msg_unit, fmt_real) "Expected: ", expected
        write (msg_unit, fmt_real) "Actual  : ", actual
        write (msg_unit, fmt_real) "Difference:", expected - actual
    end subroutine output_real64

    !>実測値と予測値の差の最大・最小値を標準出力に出力する．
    subroutine output_int32_rank1(actual, expected)
        implicit none
        integer(int32), intent(in) :: actual(:)
            !! 実測値
        integer(int32), intent(in) :: expected(:)
            !! 予測値

        character(*), parameter :: fmt = '('//fmt_indent//',A,i0," at (",'//fmt_idx_d1//',")")'

        write (msg_unit, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        write (msg_unit, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
    end subroutine output_int32_rank1

    !>実測値と予測値の差の最大・最小値を標準出力に出力する．
    subroutine output_int32_rank2(actual, expected)
        implicit none
        integer(int32), intent(in) :: actual(:, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :)
            !! 予測値

        character(*), parameter :: fmt = '('//fmt_indent//',A,i0," at (",'//fmt_idx_d2//',")")'

        write (msg_unit, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        write (msg_unit, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
    end subroutine output_int32_rank2

    !>実測値と予測値の差の最大・最小値を標準出力に出力する．
    subroutine output_int32_rank3(actual, expected)
        implicit none
        integer(int32), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :, :)
            !! 予測値

        character(*), parameter :: fmt = '('//fmt_indent//',A,i0," at (",'//fmt_idx_d3//',")")'

        write (msg_unit, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        write (msg_unit, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
    end subroutine output_int32_rank3

    !>実測値と予測値の差の最大・最小値を標準出力に出力する．
    subroutine output_real32_rank1(actual, expected)
        implicit none
        real(real32), intent(in) :: actual(:)
            !! 実測値
        real(real32), intent(in) :: expected(:)
            !! 予測値

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_idx_d1//',")")'

        write (msg_unit, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        write (msg_unit, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
    end subroutine output_real32_rank1

    !>実測値と予測値の差の最大・最小値を標準出力に出力する．
    subroutine output_real32_rank2(actual, expected)
        implicit none
        real(real32), intent(in) :: actual(:, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :)
            !! 予測値

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_idx_d2//',")")'

        write (msg_unit, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        write (msg_unit, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
    end subroutine output_real32_rank2

    !>実測値と予測値の差の最大・最小値を標準出力に出力する．
    subroutine output_real32_rank3(actual, expected)
        implicit none
        real(real32), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :, :)
            !! 予測値

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_idx_d3//',")")'

        write (msg_unit, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        write (msg_unit, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
    end subroutine output_real32_rank3

    !>実測値と予測値の差の最大・最小値を標準出力に出力する．
    subroutine output_real64_rank1(actual, expected)
        implicit none
        real(real64), intent(in) :: actual(:)
            !! 実測値
        real(real64), intent(in) :: expected(:)
            !! 予測値

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_idx_d1//',")")'

        write (msg_unit, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        write (msg_unit, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
    end subroutine output_real64_rank1

    !>実測値と予測値の差の最大・最小値を標準出力に出力する．
    subroutine output_real64_rank2(actual, expected)
        implicit none
        real(real64), intent(in) :: actual(:, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :)
            !! 予測値

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_idx_d2//',")")'

        write (msg_unit, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        write (msg_unit, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
    end subroutine output_real64_rank2

    !>実測値と予測値の差の最大・最小値を標準出力に出力する．
    subroutine output_real64_rank3(actual, expected)
        implicit none
        real(real64), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :, :)
            !! 予測値

        character(*), parameter :: fmt = '('//fmt_indent//',A,g0," at (",'//fmt_idx_d3//',")")'

        write (msg_unit, fmt) "Maximum Absolute Difference: ", &
                                maxval(abs(expected - actual)), &
                                findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        write (msg_unit, fmt) "Minimum Absolute Difference: ", &
                                minval(abs(expected - actual)), &
                                findloc(abs(expected - actual), minval(abs(expected - actual))) !&
    end subroutine output_real64_rank3

    !>実測値と予測値を標準出力に出力する．
    subroutine output_logical(actual, expected)
        use :: fassette_common_message, only:to_string
        implicit none
        logical, intent(in) :: actual
            !! 実測値
        logical, intent(in) :: expected
            !! 予測値

        write (msg_unit, fmt_str) "Expected: ", to_string(expected)
        write (msg_unit, fmt_str) "Actual  : ", to_string(actual)
    end subroutine output_logical

    !>実測値と予測値を標準出力に出力する．
    subroutine output_string(actual, expected)
        implicit none
        character(*), intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: expected
            !! 予測値

        write (msg_unit, fmt_str) "Expected: ", expected
        write (msg_unit, fmt_str) "Actual  : ", actual
    end subroutine output_string

    !>実測値と予測値を標準出力に出力する．
    subroutine output_char_rank1(actual, expected)
        implicit none
        character, intent(in) :: actual(:)
            !! 実測値
        character, intent(in) :: expected(:)
            !! 予測値

        write (msg_unit, fmt_char) "Expected: ", expected
        write (msg_unit, fmt_char) "Actual  : ", actual
    end subroutine output_char_rank1
end module fassette_equal_outputOnFailure
