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
        procedure :: output_real128_to_string
        procedure :: output_complex32_to_string
        procedure :: output_complex64_to_string
        procedure :: output_complex128_to_string
        procedure :: output_logical_to_string
        procedure :: output_char_to_string
        procedure :: output_int8_rank1_to_string
        procedure :: output_int8_rank2_to_string
        procedure :: output_int8_rank3_to_string
        procedure :: output_int16_rank1_to_string
        procedure :: output_int16_rank2_to_string
        procedure :: output_int16_rank3_to_string
        procedure :: output_int32_rank1_to_string
        procedure :: output_int32_rank2_to_string
        procedure :: output_int32_rank3_to_string
        procedure :: output_int64_rank1_to_string
        procedure :: output_int64_rank2_to_string
        procedure :: output_int64_rank3_to_string
        procedure :: output_real32_rank1_to_string
        procedure :: output_real32_rank2_to_string
        procedure :: output_real32_rank3_to_string
        procedure :: output_real64_rank1_to_string
        procedure :: output_real64_rank2_to_string
        procedure :: output_real64_rank3_to_string
        procedure :: output_real128_rank1_to_string
        procedure :: output_real128_rank2_to_string
        procedure :: output_real128_rank3_to_string
        procedure :: output_complex32_rank1_to_string
        procedure :: output_complex32_rank2_to_string
        procedure :: output_complex32_rank3_to_string
        procedure :: output_complex64_rank1_to_string
        procedure :: output_complex64_rank2_to_string
        procedure :: output_complex64_rank3_to_string
        procedure :: output_complex128_rank1_to_string
        procedure :: output_complex128_rank2_to_string
        procedure :: output_complex128_rank3_to_string
        procedure :: output_logical_rank1_to_string
        procedure :: output_logical_rank2_to_string
        procedure :: output_logical_rank3_to_string
        procedure :: output_char_rank1_to_string
        procedure :: output_char_rank2_to_string
        procedure :: output_char_rank3_to_string
    end interface

contains
    !>実測値と予測値を文字列に出力する．
    pure subroutine output_int8_to_string(actual, expected, output_message)
        implicit none
        integer(int8), intent(in) :: actual
            !! 実測値
        integer(int8), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

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

        character(256) :: buffer

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

        character(256) :: buffer

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

        character(256) :: buffer

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

        character(256) :: buffer

        write (buffer, fmt_real32) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_real32) "Actual  : ", actual
        call append(output_message, trim(buffer))
        write (buffer, fmt_real32) "Difference: ", expected - actual
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

        character(256) :: buffer

        write (buffer, fmt_real64) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_real64) "Actual  : ", actual
        call append(output_message, trim(buffer))
        write (buffer, fmt_real64) "Difference: ", expected - actual
        call append(output_message, trim(buffer))
    end subroutine output_real64_to_string

    !>実測値と予測値，それらの差を文字列に出力する．
    pure subroutine output_real128_to_string(actual, expected, output_message)
        implicit none
        real(real128), intent(in) :: actual
            !! 実測値
        real(real128), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_real128) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_real128) "Actual  : ", actual
        call append(output_message, trim(buffer))
        write (buffer, fmt_real128) "Difference: ", expected - actual
        call append(output_message, trim(buffer))
    end subroutine output_real128_to_string

    !>実測値と予測値，それらの差を文字列に出力する．
    pure subroutine output_complex32_to_string(actual, expected, output_message)
        implicit none
        complex(real32), intent(in) :: actual
            !! 実測値
        complex(real32), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex32) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex32) "Actual  : ", actual
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex32) "Difference: ", expected - actual
        call append(output_message, trim(buffer))
    end subroutine output_complex32_to_string

    !>実測値と予測値，それらの差を文字列に出力する．
    pure subroutine output_complex64_to_string(actual, expected, output_message)
        implicit none
        complex(real64), intent(in) :: actual
            !! 実測値
        complex(real64), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex64) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex64) "Actual  : ", actual
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex64) "Difference: ", expected - actual
        call append(output_message, trim(buffer))
    end subroutine output_complex64_to_string

    !>実測値と予測値，それらの差を文字列に出力する．
    pure subroutine output_complex128_to_string(actual, expected, output_message)
        implicit none
        complex(real128), intent(in) :: actual
            !! 実測値
        complex(real128), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex128) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex128) "Actual  : ", actual
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex128) "Difference: ", expected - actual
        call append(output_message, trim(buffer))
    end subroutine output_complex128_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int8_rank1_to_string(actual, expected, output_message)
        implicit none
        integer(int8), intent(in) :: actual(:)
            !! 実測値
        integer(int8), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_int_rank1) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank1) "Minimum Absolute Difference: ", &
                                      minval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int8_rank1_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int8_rank2_to_string(actual, expected, output_message)
        implicit none
        integer(int8), intent(in) :: actual(:, :)
            !! 実測値
        integer(int8), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_int_rank2) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank2) "Minimum Absolute Difference: ", &
                                      minval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int8_rank2_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int8_rank3_to_string(actual, expected, output_message)
        implicit none
        integer(int8), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int8), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_int_rank3) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank3) "Minimum Absolute Difference: ", &
                                      minval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int8_rank3_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int16_rank1_to_string(actual, expected, output_message)
        implicit none
        integer(int16), intent(in) :: actual(:)
            !! 実測値
        integer(int16), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_int_rank1) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank1) "Minimum Absolute Difference: ", &
                                      minval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int16_rank1_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int16_rank2_to_string(actual, expected, output_message)
        implicit none
        integer(int16), intent(in) :: actual(:, :)
            !! 実測値
        integer(int16), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_int_rank2) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank2) "Minimum Absolute Difference: ", &
                                      minval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int16_rank2_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int16_rank3_to_string(actual, expected, output_message)
        implicit none
        integer(int16), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int16), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_int_rank3) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank3) "Minimum Absolute Difference: ", &
                                      minval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int16_rank3_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int32_rank1_to_string(actual, expected, output_message)
        implicit none
        integer(int32), intent(in) :: actual(:)
            !! 実測値
        integer(int32), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_int_rank1) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank1) "Minimum Absolute Difference: ", &
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

        character(256) :: buffer

        write (buffer, fmt_int_rank2) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank2) "Minimum Absolute Difference: ", &
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

        character(256) :: buffer

        write (buffer, fmt_int_rank3) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank3) "Minimum Absolute Difference: ", &
                                      minval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int32_rank3_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int64_rank1_to_string(actual, expected, output_message)
        implicit none
        integer(int64), intent(in) :: actual(:)
            !! 実測値
        integer(int64), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_int_rank1) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank1) "Minimum Absolute Difference: ", &
                                      minval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int64_rank1_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int64_rank2_to_string(actual, expected, output_message)
        implicit none
        integer(int64), intent(in) :: actual(:, :)
            !! 実測値
        integer(int64), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_int_rank2) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank2) "Minimum Absolute Difference: ", &
                                      minval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int64_rank2_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_int64_rank3_to_string(actual, expected, output_message)
        implicit none
        integer(int64), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int64), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_int_rank3) "Maximum Absolute Difference: ", &
                                      maxval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_int_rank3) "Minimum Absolute Difference: ", &
                                      minval(abs(expected - actual)), &
                                      findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_int64_rank3_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_real32_rank1_to_string(actual, expected, output_message)
        implicit none
        real(real32), intent(in) :: actual(:)
            !! 実測値
        real(real32), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_real32_rank1) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_real32_rank1) "Minimum Absolute Difference: ", &
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

        character(256) :: buffer

        write (buffer, fmt_real32_rank2) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_real32_rank2) "Minimum Absolute Difference: ", &
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

        character(256) :: buffer

        write (buffer, fmt_real32_rank3) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_real32_rank3) "Minimum Absolute Difference: ", &
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

        character(256) :: buffer

        write (buffer, fmt_real64_rank1) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_real64_rank1) "Minimum Absolute Difference: ", &
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

        character(256) :: buffer

        write (buffer, fmt_real64_rank2) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_real64_rank2) "Minimum Absolute Difference: ", &
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

        character(256) :: buffer

        write (buffer, fmt_real64_rank3) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_real64_rank3) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_real64_rank3_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_real128_rank1_to_string(actual, expected, output_message)
        implicit none
        real(real128), intent(in) :: actual(:)
            !! 実測値
        real(real128), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_real128_rank1) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_real128_rank1) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_real128_rank1_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_real128_rank2_to_string(actual, expected, output_message)
        implicit none
        real(real128), intent(in) :: actual(:, :)
            !! 実測値
        real(real128), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_real128_rank2) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_real128_rank2) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_real128_rank2_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_real128_rank3_to_string(actual, expected, output_message)
        implicit none
        real(real128), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real128), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_real128_rank3) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_real128_rank3) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_real128_rank3_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_complex32_rank1_to_string(actual, expected, output_message)
        implicit none
        complex(real32), intent(in) :: actual(:)
            !! 実測値
        complex(real32), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex32_rank1) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex32_rank1) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_complex32_rank1_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_complex32_rank2_to_string(actual, expected, output_message)
        implicit none
        complex(real32), intent(in) :: actual(:, :)
            !! 実測値
        complex(real32), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex32_rank2) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex32_rank2) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_complex32_rank2_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_complex32_rank3_to_string(actual, expected, output_message)
        implicit none
        complex(real32), intent(in) :: actual(:, :, :)
            !! 実測値
        complex(real32), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex32_rank3) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex32_rank3) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_complex32_rank3_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_complex64_rank1_to_string(actual, expected, output_message)
        implicit none
        complex(real64), intent(in) :: actual(:)
            !! 実測値
        complex(real64), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex64_rank1) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex64_rank1) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_complex64_rank1_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_complex64_rank2_to_string(actual, expected, output_message)
        implicit none
        complex(real64), intent(in) :: actual(:, :)
            !! 実測値
        complex(real64), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex64_rank2) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex64_rank2) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_complex64_rank2_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_complex64_rank3_to_string(actual, expected, output_message)
        implicit none
        complex(real64), intent(in) :: actual(:, :, :)
            !! 実測値
        complex(real64), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex64_rank3) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex64_rank3) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_complex64_rank3_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_complex128_rank1_to_string(actual, expected, output_message)
        implicit none
        complex(real128), intent(in) :: actual(:)
            !! 実測値
        complex(real128), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex128_rank1) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex128_rank1) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_complex128_rank1_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_complex128_rank2_to_string(actual, expected, output_message)
        implicit none
        complex(real128), intent(in) :: actual(:, :)
            !! 実測値
        complex(real128), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex128_rank2) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex128_rank2) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_complex128_rank2_to_string

    !>実測値と予測値の差の最大・最小値を文字列に出力する．
    pure subroutine output_complex128_rank3_to_string(actual, expected, output_message)
        implicit none
        complex(real128), intent(in) :: actual(:, :, :)
            !! 実測値
        complex(real128), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_complex128_rank3) "Maximum Absolute Difference: ", &
                                         maxval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), maxval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
        write (buffer, fmt_complex128_rank3) "Minimum Absolute Difference: ", &
                                         minval(abs(expected - actual)), &
                                         findloc(abs(expected - actual), minval(abs(expected - actual))) !&
        call append(output_message, trim(buffer))
    end subroutine output_complex128_rank3_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_logical_to_string(actual, expected, output_message)
        use :: fassert_common_message, only:to_string
        implicit none
        logical, intent(in) :: actual
            !! 実測値
        logical, intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_str) "Expected: ", to_string(expected)
        call append(output_message, trim(buffer))
        write (buffer, fmt_str) "Actual  : ", to_string(actual)
        call append(output_message, trim(buffer))
    end subroutine output_logical_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_logical_rank1_to_string(actual, expected, output_message)
        use :: fassert_common_message, only:to_string
        use :: fassert_common_hasZero
        implicit none
        logical, intent(in) :: actual(:)
            !! 実測値
        logical, intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual .eqv. expected), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are equivalent."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_str) "Expected: ", to_string(expected(idx(1)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_str) "Actual  : ", to_string(actual(idx(1)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank1) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_logical_rank1_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_logical_rank2_to_string(actual, expected, output_message)
        use :: fassert_common_message, only:to_string
        use :: fassert_common_hasZero
        implicit none
        logical, intent(in) :: actual(:, :)
            !! 実測値
        logical, intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual .eqv. expected), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are equivalent."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_str) "Expected: ", to_string(expected(idx(1), idx(2)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_str) "Actual  : ", to_string(actual(idx(1), idx(2)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank2) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_logical_rank2_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_logical_rank3_to_string(actual, expected, output_message)
        use :: fassert_common_message, only:to_string
        use :: fassert_common_hasZero
        implicit none
        logical, intent(in) :: actual(:, :, :)
            !! 実測値
        logical, intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual .eqv. expected), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are equivalent."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_str) "Expected: ", to_string(expected(idx(1), idx(2), idx(3)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_str) "Actual  : ", to_string(actual(idx(1), idx(2), idx(3)))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank3) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_logical_rank3_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_char_to_string(actual, expected, output_message)
        implicit none
        character(*), intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: expected
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer

        write (buffer, fmt_str) "Expected: ", expected
        call append(output_message, trim(buffer))
        write (buffer, fmt_str) "Actual  : ", actual
        call append(output_message, trim(buffer))
    end subroutine output_char_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_char_rank1_to_string(actual, expected, output_message)
        use :: fassert_common_hasZero
        implicit none
        character(*), intent(in) :: actual(:)
            !! 実測値
        character(*), intent(in) :: expected(:)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual == expected), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are equivalent."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_char) "Expected: ", expected(idx(1))
            call append(output_message, trim(buffer))
            write (buffer, fmt_char) "Actual  : ", actual(idx(1))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank1) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_char_rank1_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_char_rank2_to_string(actual, expected, output_message)
        use :: fassert_common_hasZero
        implicit none
        character(*), intent(in) :: actual(:, :)
            !! 実測値
        character(*), intent(in) :: expected(:, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual == expected), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are equivalent."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_char) "Expected: ", expected(idx(1), idx(2))
            call append(output_message, trim(buffer))
            write (buffer, fmt_char) "Actual  : ", actual(idx(1), idx(2))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank2) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_char_rank2_to_string

    !>実測値と予測値を文字列に出力する．
    pure subroutine output_char_rank3_to_string(actual, expected, output_message)
        use :: fassert_common_hasZero
        implicit none
        character(*), intent(in) :: actual(:, :, :)
            !! 実測値
        character(*), intent(in) :: expected(:, :, :)
            !! 予測値
        character(:), allocatable, intent(inout) :: output_message

        character(256) :: buffer
        integer(int32) :: idx(rank(actual))

        idx = findloc((actual == expected), .false.)

        if (has_zero(idx)) then
            write (buffer, '('//fmt_indent//',A)') "All elements are equivalent."
            call append(output_message, trim(buffer))
        else
            write (buffer, fmt_char) "Expected: ", expected(idx(1), idx(2), idx(3))
            call append(output_message, trim(buffer))
            write (buffer, fmt_char) "Actual  : ", actual(idx(1), idx(2), idx(3))
            call append(output_message, trim(buffer))
            write (buffer, fmt_position_rank3) "Position: ", idx
            call append(output_message, trim(buffer))
        end if
    end subroutine output_char_rank3_to_string

end module fassert_common_message_outputOnFailure_toString_equal
