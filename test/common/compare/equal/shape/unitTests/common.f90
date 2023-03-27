module test_common_compare_equal_shape_unitTests_common
    use, intrinsic :: iso_fortran_env
    use :: testdrive_util, only:to_string
    implicit none
    private
    public :: failure_message

contains
    function failure_message(expected, actual, test_name) result(msg)
        implicit none
        logical, intent(in) :: expected
        logical, intent(in) :: actual
        character(*), intent(in) :: test_name
        character(:), allocatable :: msg

        msg = test_name//new_line(" ")// &
              "    expected : "//to_string(expected)//new_line(" ")// &
              "    actual   : "//to_string(actual)
    end function failure_message
end module test_common_compare_equal_shape_unitTests_common
