module fassert_interface
    implicit none
    private
    public :: Iis_equal
    public :: Iis_equal_rank1
    public :: Iis_equal_rank2
    public :: Iis_equal_rank3
    public :: Ioutput_on_failure_to_unit
    public :: Ioutput_on_failure_to_unit_rank1
    public :: Ioutput_on_failure_to_unit_rank2
    public :: Ioutput_on_failure_to_unit_rank3
    public :: Ioutput_on_failure_to_string
    public :: Ioutput_on_failure_to_string_rank1
    public :: Ioutput_on_failure_to_string_rank2
    public :: Ioutput_on_failure_to_string_rank3

    interface
        !>任意のスカラ型の比較を行う手続のインタフェース
        logical function Iis_equal(actual, expected)
            implicit none
            class(*), intent(in) :: actual
                !! 予測値
            class(*), intent(in) :: expected
                !! 実測値
        end function Iis_equal

        !>任意の1次元配列の比較を行う手続のインタフェース
        logical function Iis_equal_rank1(actual, expected)
            implicit none
            class(*), intent(in) :: actual(:)
                !! 予測値
            class(*), intent(in) :: expected(:)
                !! 実測値
        end function Iis_equal_rank1

        !>任意の2次元配列の比較を行う手続のインタフェース
        logical function Iis_equal_rank2(actual, expected)
            implicit none
            class(*), intent(in) :: actual(:, :)
                !! 予測値
            class(*), intent(in) :: expected(:, :)
                !! 実測値
        end function Iis_equal_rank2

        !>任意の3次元配列の比較を行う手続のインタフェース
        logical function Iis_equal_rank3(actual, expected)
            implicit none
            class(*), intent(in) :: actual(:, :, :)
                !! 予測値
            class(*), intent(in) :: expected(:, :, :)
                !! 実測値
        end function Iis_equal_rank3
    end interface

    interface
        !>検査失敗時に予測値と実測値を出力する手続のインタフェース
        subroutine Ioutput_on_failure_to_unit(actual, expected)
            implicit none
            class(*), intent(in) :: actual
                !! 予測値
            class(*), intent(in) :: expected
                !! 実測値
        end subroutine Ioutput_on_failure_to_unit

        !>検査失敗時に予測値と実測値を出力する手続のインタフェース
        subroutine Ioutput_on_failure_to_unit_rank1(actual, expected)
            implicit none
            class(*), intent(in) :: actual(:)
                !! 予測値
            class(*), intent(in) :: expected(:)
                !! 実測値
        end subroutine Ioutput_on_failure_to_unit_rank1

        !>検査失敗時に予測値と実測値を出力する手続のインタフェース
        subroutine Ioutput_on_failure_to_unit_rank2(actual, expected)
            implicit none
            class(*), intent(in) :: actual(:, :)
                !! 予測値
            class(*), intent(in) :: expected(:, :)
                !! 実測値
        end subroutine Ioutput_on_failure_to_unit_rank2

        !>検査失敗時に予測値と実測値を出力する手続のインタフェース
        subroutine Ioutput_on_failure_to_unit_rank3(actual, expected)
            implicit none
            class(*), intent(in) :: actual(:, :, :)
                !! 予測値
            class(*), intent(in) :: expected(:, :, :)
                !! 実測値
        end subroutine Ioutput_on_failure_to_unit_rank3
    end interface

    interface
        !>検査失敗時に予測値と実測値を文字列に出力する手続のインタフェース
        subroutine Ioutput_on_failure_to_string(actual, expected, output_message)
            implicit none
            class(*), intent(in) :: actual
                !! 予測値
            class(*), intent(in) :: expected
                !! 実測値
            character(:), allocatable, intent(inout) :: output_message
                !! 出力を格納する文字列
        end subroutine Ioutput_on_failure_to_string

        !>検査失敗時に予測値と実測値を文字列に出力する手続のインタフェース
        subroutine Ioutput_on_failure_to_string_rank1(actual, expected, output_message)
            implicit none
            class(*), intent(in) :: actual(:)
                !! 予測値
            class(*), intent(in) :: expected(:)
                !! 実測値
            character(:), allocatable, intent(inout) :: output_message
                !! 出力を格納する文字列
        end subroutine Ioutput_on_failure_to_string_rank1

        !>検査失敗時に予測値と実測値を文字列に出力する手続のインタフェース
        subroutine Ioutput_on_failure_to_string_rank2(actual, expected, output_message)
            implicit none
            class(*), intent(in) :: actual(:, :)
                !! 予測値
            class(*), intent(in) :: expected(:, :)
                !! 実測値
            character(:), allocatable, intent(inout) :: output_message
                !! 出力を格納する文字列
        end subroutine Ioutput_on_failure_to_string_rank2

        !>検査失敗時に予測値と実測値を文字列に出力する手続のインタフェース
        subroutine Ioutput_on_failure_to_string_rank3(actual, expected, output_message)
            implicit none
            class(*), intent(in) :: actual(:, :, :)
                !! 予測値
            class(*), intent(in) :: expected(:, :, :)
                !! 実測値
            character(:), allocatable, intent(inout) :: output_message
                !! 出力を格納する文字列
        end subroutine Ioutput_on_failure_to_string_rank3
    end interface
end module fassert_interface
