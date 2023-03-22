module fassert_interface
    implicit none
    private
    public :: Iis_equal
    public :: Ioutput_on_failure_to_unit
    public :: Ioutput_on_failure_to_string

    interface
        !>任意のスカラ型の比較を行う手続のインタフェース
        logical function Iis_equal(actual, expected)
            implicit none
            class(*), intent(in) :: actual
                !! 予測値
            class(*), intent(in) :: expected
                !! 実測値
        end function Iis_equal
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
    end interface
end module fassert_interface
