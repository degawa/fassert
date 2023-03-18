module test_sameShape_compareArrayShape_unitTests_areSameShape_int
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string
    use :: fassert_common_compare_equal_shape
    implicit none
    private
    public :: d1_i32_should_return_true_when_input_same_shape
    public :: d1_i32_should_return_false_when_input_different_shape
    public :: d2_i32_should_return_true_when_input_same_shape
    public :: d2_i32_should_return_false_when_input_different_shape
    public :: d3_i32_should_return_true_when_input_same_shape
    public :: d3_i32_should_return_false_when_input_different_shape

contains
    subroutine d1_i32_should_return_true_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3)
        integer(int32) :: b(3)

        call check(error, are_same_shape(a, b), &
                   "expected '"//to_string(.true.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d1_i32_should_return_true_when_input_same_shape

    subroutine d1_i32_should_return_false_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3)
        integer(int32) :: b(2)

        call check(error,.not. are_same_shape(a, b), &
                   "expected '"//to_string(.false.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d1_i32_should_return_false_when_input_different_shape

    subroutine d2_i32_should_return_true_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3, 2)
        integer(int32) :: b(3, 2)

        call check(error, are_same_shape(a, b), &
                   "expected '"//to_string(.true.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d2_i32_should_return_true_when_input_same_shape

    subroutine d2_i32_should_return_false_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3, 2)
        integer(int32) :: b(2, 3)

        call check(error,.not. are_same_shape(a, b), &
                   "expected '"//to_string(.false.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d2_i32_should_return_false_when_input_different_shape

    subroutine d3_i32_should_return_true_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3, 2, 4)
        integer(int32) :: b(3, 2, 4)

        call check(error, are_same_shape(a, b), &
                   "expected '"//to_string(.true.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d3_i32_should_return_true_when_input_same_shape

    subroutine d3_i32_should_return_false_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: a(3, 2, 4)
        integer(int32) :: b(2, 3, 1)

        call check(error,.not. are_same_shape(a, b), &
                   "expected '"//to_string(.false.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d3_i32_should_return_false_when_input_different_shape
end module test_sameShape_compareArrayShape_unitTests_areSameShape_int
