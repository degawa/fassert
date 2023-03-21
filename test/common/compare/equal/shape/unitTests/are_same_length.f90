module test_common_compare_equal_shape_unitTests_areSameLength
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: strings_enclose
    use :: fassert_common_compare_equal_length
    use :: fassert_common_message
    implicit none
    private
    ! STR: character(:), allocatable
    ! T: true, F: false
    public :: STR_should_return_T_when_input_same_length_string
    public :: STR_should_return_F_when_input_different_length_string
    public :: STR_should_return_T_when_input_diff_len_due_to_tailing_space

contains
    subroutine STR_should_return_T_when_input_same_length_string(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: a
        character(:), allocatable :: b

        a = "abc"
        b = "xyz"

        call check(error, are_same_length(a, b), &
                   "expected "//enclose(string_true, '"')// &
                   ", but got "//enclose(to_string(are_same_length(a, b)), '"'))
    end subroutine STR_should_return_T_when_input_same_length_string

    subroutine STR_should_return_F_when_input_different_length_string(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: a
        character(:), allocatable :: b

        a = "abc"
        b = "abca"

        call check(error,.not. are_same_length(a, b), &
                   "expected "//enclose(string_false, '"')// &
                   ", but got "//enclose(to_string(are_same_length(a, b)), '"'))
    end subroutine STR_should_return_F_when_input_different_length_string

    subroutine STR_should_return_T_when_input_diff_len_due_to_tailing_space(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: a
        character(:), allocatable :: b

        a = "abc"
        b = "xyz "

        call check(error, are_same_length(a, b), &
                   "expected "//enclose(string_true, '"')// &
                   ", but got "//enclose(to_string(are_same_length(a, b)), '"'))
    end subroutine STR_should_return_T_when_input_diff_len_due_to_tailing_space
end module test_common_compare_equal_shape_unitTests_areSameLength
