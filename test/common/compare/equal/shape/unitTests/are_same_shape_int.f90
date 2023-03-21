module test_common_compare_equal_shape_unitTests_areSameShape_int
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: strings_enclose
    use :: fassert_common_compare_equal_shape
    use :: fassert_common_message
    implicit none
    private
    ! D: dimension, I: integer
    ! T: true, F: false
    public :: D1I32_should_return_T_when_input_same_shape
    public :: D1I32_should_return_F_when_input_different_shape
    public :: D2I32_should_return_T_when_input_same_shape
    public :: D2I32_should_return_F_when_input_different_shape
    public :: D3I32_should_return_T_when_input_same_shape
    public :: D3I32_should_return_F_when_input_different_shape

contains
    subroutine D1I32_should_return_T_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3)
        integer(int32) :: b(3)

        call check(error, are_same_shape(a, b), &
                   "expected "//enclose(string_true, '"')// &
                   ", but got "//enclose(to_string(are_same_shape(a, b)), '"'))
    end subroutine D1I32_should_return_T_when_input_same_shape

    subroutine D1I32_should_return_F_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3)
        integer(int32) :: b(2)

        call check(error,.not. are_same_shape(a, b), &
                   "expected "//enclose(string_false, '"')// &
                   ", but got "//enclose(to_string(are_same_shape(a, b)), '"'))
    end subroutine D1I32_should_return_F_when_input_different_shape

    subroutine D2I32_should_return_T_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3, 2)
        integer(int32) :: b(3, 2)

        call check(error, are_same_shape(a, b), &
                   "expected "//enclose(string_true, '"')// &
                   ", but got "//enclose(to_string(are_same_shape(a, b)), '"'))
    end subroutine D2I32_should_return_T_when_input_same_shape

    subroutine D2I32_should_return_F_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3, 2)
        integer(int32) :: b(2, 3)

        call check(error,.not. are_same_shape(a, b), &
                   "expected "//enclose(string_false, '"')// &
                   ", but got "//enclose(to_string(are_same_shape(a, b)), '"'))
    end subroutine D2I32_should_return_F_when_input_different_shape

    subroutine D3I32_should_return_T_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3, 2, 4)
        integer(int32) :: b(3, 2, 4)

        call check(error, are_same_shape(a, b), &
                   "expected "//enclose(string_true, '"')// &
                   ", but got "//enclose(to_string(are_same_shape(a, b)), '"'))
    end subroutine D3I32_should_return_T_when_input_same_shape

    subroutine D3I32_should_return_F_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3, 2, 4)
        integer(int32) :: b(2, 3, 1)

        call check(error,.not. are_same_shape(a, b), &
                   "expected "//enclose(string_false, '"')// &
                   ", but got "//enclose(to_string(are_same_shape(a, b)), '"'))
    end subroutine D3I32_should_return_F_when_input_different_shape
end module test_common_compare_equal_shape_unitTests_areSameShape_int
