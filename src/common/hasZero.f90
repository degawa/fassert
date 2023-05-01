module fassert_common_hasZero
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: has_zero

    interface has_zero
        procedure :: has_zero_int32_rank1
    end interface

contains
    !>Returns `.true.` if array contains zero as its element
    !>and returns `.false.` elsewhere.
    pure function has_zero_int32_rank1(array)
        implicit none
        integer(int32), intent(in) :: array(:)
            !! array
        logical :: has_zero_int32_rank1

        if (count(array == 0) > 0) then
            has_zero_int32_rank1 = .true.
        else
            has_zero_int32_rank1 = .false.
        end if
    end function has_zero_int32_rank1
end module fassert_common_hasZero
