module fassette_common_optval
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: optval

    interface optval
        procedure :: optval_real32
        procedure :: optval_real64
        procedure :: optval_logical
    end interface
contains
    !>単精度実数型引数`x`が渡されていれば`x`を，
    !>渡されていなければ`deafult`を返す．
    pure elemental function optval_real32(x, default) result(y)
        implicit none
        !&<
        real(real32), intent(in), optional  :: x
        real(real32), intent(in)            :: default
            !! `x`が渡されていない場合の既定値
        !&>
        real(real32) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_real32

    !>倍精度実数型引数`x`が渡されていれば`x`を，
    !>渡されていなければ`deafult`を返す．
    pure elemental function optval_real64(x, default) result(y)
        implicit none
        !&<
        real(real64), intent(in), optional  :: x
        real(real64), intent(in)            :: default
            !! `x`が渡されていない場合の既定値
        !&>
        real(real64) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_real64

    !>論理型引数`x`が渡されていれば`x`を，
    !>渡されていなければ`deafult`を返す．
    pure elemental function optval_logical(x, default) result(y)
        implicit none
        !&<
        logical, intent(in), optional   :: x
        logical, intent(in)             :: default
            !! `x`が渡されていない場合の既定値
        !&>
        logical :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_logical
end module fassette_common_optval
