module fassert_common_store
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: store

    interface store
        procedure :: store_logical
    end interface

contains
    !>`stat`が渡されている場合に，`val`の値を`stat`に書き込む．
    !>`stat`が渡されていなければ，何もしない．
    pure elemental subroutine store_logical(stat, val)
        implicit none
        !&<
        logical, intent(out), optional  :: stat
            !! `val`を書き込む変数
        logical, intent(in)             :: val
            !! 書き込まれる値
        !&>

        if (present(stat)) stat = val
    end subroutine store_logical
end module fassert_common_store
