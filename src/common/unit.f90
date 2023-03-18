module fassert_common_unit
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: set_assertion_message_unit

    integer(int32), public, protected :: assertion_message_unit = output_unit
        !! assertion messageの出力装置番号

contains
    !>`unit`を装置番号`assertion_message_unit`に設定する．
    subroutine set_assertion_message_unit(unit)
        implicit none
        integer(int32), intent(in) :: unit
            !! 装置番号

        assertion_message_unit = unit
    end subroutine set_assertion_message_unit
end module fassert_common_unit
