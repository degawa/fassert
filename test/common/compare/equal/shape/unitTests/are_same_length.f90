module test_common_compare_equal_shape_unitTests_areSameLength
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_compare_equal_length
    use :: fassert_common_message
    implicit none
    private
    public :: str_should_return_true_when_input_same_length_string
    public :: str_should_return_false_when_input_different_length_string
    public :: str_should_return_true_when_input_diff_len_due_to_tailing_space

contains
    subroutine str_should_return_true_when_input_same_length_string(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: a
        character(:), allocatable :: b

        a = "abc"
        b = "xyz"

        call check(error, are_same_length(a, b), &
                   "expected '"//string_true// &
                   "', but got '"//to_string(are_same_length(a, b))//"'")
    end subroutine str_should_return_true_when_input_same_length_string

    subroutine str_should_return_false_when_input_different_length_string(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: a
        character(:), allocatable :: b

        a = "abc"
        b = "abca"

        call check(error,.not. are_same_length(a, b), &
                   "expected '"//string_false// &
                   "', but got '"//to_string(are_same_length(a, b))//"'")
    end subroutine str_should_return_false_when_input_different_length_string

    subroutine str_should_return_true_when_input_diff_len_due_to_tailing_space(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: a
        character(:), allocatable :: b

        a = "abc"
        b = "xyz "

        call check(error, are_same_length(a, b), &
                   "expected '"//string_true// &
                   "', but got '"//to_string(are_same_length(a, b))//"'")
    end subroutine str_should_return_true_when_input_diff_len_due_to_tailing_space
end module test_common_compare_equal_shape_unitTests_areSameLength
