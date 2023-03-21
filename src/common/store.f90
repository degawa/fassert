module fassert_common_store
    use :: fassert_common_message
    implicit none
    private
    public :: store
    public :: append

    interface store
        procedure :: store_logical
    end interface

    interface append
        procedure :: append_str
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

    !>`str`が渡されている場合に，`val`の値を`str`末尾に改行して追記する．
    subroutine append_str(str, val)
        implicit none
        !&<
        character(:), allocatable   , intent(inout) :: str
            !! `val`が連結される文字列
        character(*)                , intent(in)    :: val
            !! 連結する文字列
        !&>

        if (allocated(str)) then
            str = str//NL//val
        else
            str = val
        end if
    end subroutine append_str
end module fassert_common_store
