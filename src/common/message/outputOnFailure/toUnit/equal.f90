module fassert_common_message_outputOnFailure_toUnit_equal
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_unit, msg_unit => assertion_message_unit
    use :: fassert_common_message_outputOnFailure_toString_equal
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
        procedure :: output_real128
        procedure :: output_complex32
        procedure :: output_complex64
        procedure :: output_complex128
        procedure :: output_logical
        procedure :: output_char
        procedure :: output_int8_rank1
        procedure :: output_int8_rank2
        procedure :: output_int8_rank3
        procedure :: output_int16_rank1
        procedure :: output_int16_rank2
        procedure :: output_int16_rank3
        procedure :: output_int32_rank1
        procedure :: output_int32_rank2
        procedure :: output_int32_rank3
        procedure :: output_int64_rank1
        procedure :: output_int64_rank2
        procedure :: output_int64_rank3
        procedure :: output_real32_rank1
        procedure :: output_real32_rank2
        procedure :: output_real32_rank3
        procedure :: output_real64_rank1
        procedure :: output_real64_rank2
        procedure :: output_real64_rank3
        procedure :: output_real128_rank1
        procedure :: output_real128_rank2
        procedure :: output_real128_rank3
        procedure :: output_complex32_rank1
        procedure :: output_complex32_rank2
        procedure :: output_complex32_rank3
        procedure :: output_complex64_rank1
        procedure :: output_complex64_rank2
        procedure :: output_complex64_rank3
        procedure :: output_complex128_rank1
        procedure :: output_complex128_rank2
        procedure :: output_complex128_rank3
        procedure :: output_logical_rank1
        procedure :: output_logical_rank2
        procedure :: output_logical_rank3
        procedure :: output_char_rank1
        procedure :: output_char_rank2
        procedure :: output_char_rank3
    end interface

contains
    !>実測値と予測値を装置に出力する．
    subroutine output_int8(actual, expected)
        implicit none
        integer(int8), intent(in) :: actual
            !! 実測値
        integer(int8), intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int8

    !>実測値と予測値を装置に出力する．
    subroutine output_int16(actual, expected)
        implicit none
        integer(int16), intent(in) :: actual
            !! 実測値
        integer(int16), intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int16

    !>実測値と予測値を装置に出力する．
    subroutine output_int32(actual, expected)
        implicit none
        integer(int32), intent(in) :: actual
            !! 実測値
        integer(int32), intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int32

    !>実測値と予測値を装置に出力する．
    subroutine output_int64(actual, expected)
        implicit none
        integer(int64), intent(in) :: actual
            !! 実測値
        integer(int64), intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int64

    !>実測値と予測値を装置に出力する．
    subroutine output_real32(actual, expected)
        implicit none
        real(real32), intent(in) :: actual
            !! 実測値
        real(real32), intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real32

    !>実測値と予測値を装置に出力する．
    subroutine output_real64(actual, expected)
        implicit none
        real(real64), intent(in) :: actual
            !! 実測値
        real(real64), intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real64

    !>実測値と予測値を装置に出力する．
    subroutine output_real128(actual, expected)
        implicit none
        real(real128), intent(in) :: actual
            !! 実測値
        real(real128), intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real128

    !>実測値と予測値を装置に出力する．
    subroutine output_complex32(actual, expected)
        implicit none
        complex(real32), intent(in) :: actual
            !! 実測値
        complex(real32), intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex32

    !>実測値と予測値を装置に出力する．
    subroutine output_complex64(actual, expected)
        implicit none
        complex(real64), intent(in) :: actual
            !! 実測値
        complex(real64), intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex64

    !>実測値と予測値を装置に出力する．
    subroutine output_complex128(actual, expected)
        implicit none
        complex(real128), intent(in) :: actual
            !! 実測値
        complex(real128), intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex128

    !>実測値と予測値を装置に出力する．
    subroutine output_logical(actual, expected)
        implicit none
        logical, intent(in) :: actual
            !! 実測値
        logical, intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_logical

    !>実測値と予測値を装置に出力する．
    subroutine output_char(actual, expected)
        implicit none
        character(*), intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: expected
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_char

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int8_rank1(actual, expected)
        implicit none
        integer(int8), intent(in) :: actual(:)
            !! 実測値
        integer(int8), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int8_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int8_rank2(actual, expected)
        implicit none
        integer(int8), intent(in) :: actual(:, :)
            !! 実測値
        integer(int8), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int8_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int8_rank3(actual, expected)
        implicit none
        integer(int8), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int8), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int8_rank3

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int16_rank1(actual, expected)
        implicit none
        integer(int16), intent(in) :: actual(:)
            !! 実測値
        integer(int16), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int16_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int16_rank2(actual, expected)
        implicit none
        integer(int16), intent(in) :: actual(:, :)
            !! 実測値
        integer(int16), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int16_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int16_rank3(actual, expected)
        implicit none
        integer(int16), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int16), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int16_rank3

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int32_rank1(actual, expected)
        implicit none
        integer(int32), intent(in) :: actual(:)
            !! 実測値
        integer(int32), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int32_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int32_rank2(actual, expected)
        implicit none
        integer(int32), intent(in) :: actual(:, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int32_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int32_rank3(actual, expected)
        implicit none
        integer(int32), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int32_rank3

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int64_rank1(actual, expected)
        implicit none
        integer(int64), intent(in) :: actual(:)
            !! 実測値
        integer(int64), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int64_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int64_rank2(actual, expected)
        implicit none
        integer(int64), intent(in) :: actual(:, :)
            !! 実測値
        integer(int64), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int64_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_int64_rank3(actual, expected)
        implicit none
        integer(int64), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int64), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_int64_rank3

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_real32_rank1(actual, expected)
        implicit none
        real(real32), intent(in) :: actual(:)
            !! 実測値
        real(real32), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real32_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_real32_rank2(actual, expected)
        implicit none
        real(real32), intent(in) :: actual(:, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real32_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_real32_rank3(actual, expected)
        implicit none
        real(real32), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real32_rank3

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_real64_rank1(actual, expected)
        implicit none
        real(real64), intent(in) :: actual(:)
            !! 実測値
        real(real64), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real64_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_real64_rank2(actual, expected)
        implicit none
        real(real64), intent(in) :: actual(:, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real64_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_real64_rank3(actual, expected)
        implicit none
        real(real64), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real64_rank3

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_real128_rank1(actual, expected)
        implicit none
        real(real128), intent(in) :: actual(:)
            !! 実測値
        real(real128), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real128_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_real128_rank2(actual, expected)
        implicit none
        real(real128), intent(in) :: actual(:, :)
            !! 実測値
        real(real128), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real128_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_real128_rank3(actual, expected)
        implicit none
        real(real128), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real128), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_real128_rank3

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_complex32_rank1(actual, expected)
        implicit none
        complex(real32), intent(in) :: actual(:)
            !! 実測値
        complex(real32), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex32_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_complex32_rank2(actual, expected)
        implicit none
        complex(real32), intent(in) :: actual(:, :)
            !! 実測値
        complex(real32), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex32_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_complex32_rank3(actual, expected)
        implicit none
        complex(real32), intent(in) :: actual(:, :, :)
            !! 実測値
        complex(real32), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex32_rank3

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_complex64_rank1(actual, expected)
        implicit none
        complex(real64), intent(in) :: actual(:)
            !! 実測値
        complex(real64), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex64_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_complex64_rank2(actual, expected)
        implicit none
        complex(real64), intent(in) :: actual(:, :)
            !! 実測値
        complex(real64), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex64_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_complex64_rank3(actual, expected)
        implicit none
        complex(real64), intent(in) :: actual(:, :, :)
            !! 実測値
        complex(real64), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex64_rank3

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_complex128_rank1(actual, expected)
        implicit none
        complex(real128), intent(in) :: actual(:)
            !! 実測値
        complex(real128), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex128_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_complex128_rank2(actual, expected)
        implicit none
        complex(real128), intent(in) :: actual(:, :)
            !! 実測値
        complex(real128), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex128_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_complex128_rank3(actual, expected)
        implicit none
        complex(real128), intent(in) :: actual(:, :, :)
            !! 実測値
        complex(real128), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_complex128_rank3

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_logical_rank1(actual, expected)
        implicit none
        logical, intent(in) :: actual(:)
            !! 実測値
        logical, intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_logical_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_logical_rank2(actual, expected)
        implicit none
        logical, intent(in) :: actual(:, :)
            !! 実測値
        logical, intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_logical_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_logical_rank3(actual, expected)
        implicit none
        logical, intent(in) :: actual(:, :, :)
            !! 実測値
        logical, intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_logical_rank3

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_char_rank1(actual, expected)
        implicit none
        character(*), intent(in) :: actual(:)
            !! 実測値
        character(*), intent(in) :: expected(:)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_char_rank1

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_char_rank2(actual, expected)
        implicit none
        character(*), intent(in) :: actual(:, :)
            !! 実測値
        character(*), intent(in) :: expected(:, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_char_rank2

    !>実測値と予測値の差の最大・最小値を装置に出力する．
    subroutine output_char_rank3(actual, expected)
        implicit none
        character(*), intent(in) :: actual(:, :, :)
            !! 実測値
        character(*), intent(in) :: expected(:, :, :)
            !! 予測値

        character(:), allocatable :: msg
        call output_on_failure(actual, expected, msg)
        write (msg_unit, '(A)') msg
    end subroutine output_char_rank3

end module fassert_common_message_outputOnFailure_toUnit_equal
