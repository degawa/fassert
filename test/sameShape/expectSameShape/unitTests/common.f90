module test_sameShape_expectSameShape_unitTests_expect_common
    use, intrinsic :: iso_fortran_env
    use :: strings_enclose
    use :: testdrive_util, only:to_string
    use :: par_funnel
    use :: fassert_common_unit
    implicit none
    private
    public :: replace_all
    public :: failure_message

    type, public :: string_type
        character(:), allocatable :: val
    end type string_type

contains

    function replace_all(str, it, with) result(replaced)
        implicit none
        character(*), intent(in) :: str
        character(*), intent(in) :: it
        character(*), intent(in) :: with
        character(:), allocatable :: replaced

        integer(int32) :: len_it, len_with, idx_it
        len_it = len(it)
        len_with = len(with)

        replaced = str

        idx_it = index(replaced, it, back=.false.)
        do while (idx_it > 0)
            replaced = replaced(1:idx_it - 1)//with//replaced(idx_it + len_it:)
            idx_it = index(replaced, it, back=.false.)
        end do
    end function replace_all

    function failure_message(case_name, expected, actual, stat) result(msg)
        implicit none
        character(*), intent(in) :: case_name
        character(*), intent(in) :: expected
        character(*), intent(in) :: actual
        logical, intent(in), optional :: stat(2)
        character(:), allocatable :: msg

        character(:), allocatable :: stat_exp, stat_act

        stat_exp = ""
        stat_act = ""
        if (present(stat)) then
            stat_exp = to_string(stat(1))
            stat_act = to_string(stat(2))
        end if
        msg = case_name//new_line(" ")// &
              "    expected : "//expected//" "//stat_exp//new_line(" ")// &
              "    actual   : "//actual//" "//stat_act
    end function failure_message
end module test_sameShape_expectSameShape_unitTests_expect_common
