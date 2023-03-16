module test_sameShape_compareArrayShape_unitTests_areSameShape_real
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string
    use :: assert_sameShape_compareArrayShape
    implicit none
    private
    public :: d1_r32_should_return_true_when_input_same_shape
    public :: d1_r32_should_return_false_when_input_different_shape
    public :: d2_r32_should_return_true_when_input_same_shape
    public :: d2_r32_should_return_false_when_input_different_shape
    public :: d3_r32_should_return_true_when_input_same_shape
    public :: d3_r32_should_return_false_when_input_different_shape
    public :: d1_r64_should_return_true_when_input_same_shape
    public :: d1_r64_should_return_false_when_input_different_shape
    public :: d2_r64_should_return_true_when_input_same_shape
    public :: d2_r64_should_return_false_when_input_different_shape
    public :: d3_r64_should_return_true_when_input_same_shape
    public :: d3_r64_should_return_false_when_input_different_shape

contains
    subroutine d1_r32_should_return_true_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: a(3)
        real(real32) :: b(3)

        call check(error, are_same_shape(a, b), &
                   "expected '"//to_string(.true.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d1_r32_should_return_true_when_input_same_shape

    subroutine d1_r32_should_return_false_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: a(3)
        real(real32) :: b(2)

        call check(error,.not. are_same_shape(a, b), &
                   "expected '"//to_string(.false.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d1_r32_should_return_false_when_input_different_shape

    subroutine d2_r32_should_return_true_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: a(3, 2)
        real(real32) :: b(3, 2)

        call check(error, are_same_shape(a, b), &
                   "expected '"//to_string(.true.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d2_r32_should_return_true_when_input_same_shape

    subroutine d2_r32_should_return_false_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: a(3, 2)
        real(real32) :: b(2, 3)

        call check(error,.not. are_same_shape(a, b), &
                   "expected '"//to_string(.false.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d2_r32_should_return_false_when_input_different_shape

    subroutine d3_r32_should_return_true_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: a(3, 2, 4)
        real(real32) :: b(3, 2, 4)

        call check(error, are_same_shape(a, b), &
                   "expected '"//to_string(.true.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d3_r32_should_return_true_when_input_same_shape

    subroutine d3_r32_should_return_false_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: a(3, 2, 4)
        real(real32) :: b(2, 3, 1)

        call check(error,.not. are_same_shape(a, b), &
                   "expected '"//to_string(.false.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d3_r32_should_return_false_when_input_different_shape

    subroutine d1_r64_should_return_true_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real64) :: a(3)
        real(real64) :: b(3)

        call check(error, are_same_shape(a, b), &
                   "expected '"//to_string(.true.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d1_r64_should_return_true_when_input_same_shape

    subroutine d1_r64_should_return_false_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real64) :: a(3)
        real(real64) :: b(2)

        call check(error,.not. are_same_shape(a, b), &
                   "expected '"//to_string(.false.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d1_r64_should_return_false_when_input_different_shape

    subroutine d2_r64_should_return_true_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real64) :: a(3, 2)
        real(real64) :: b(3, 2)

        call check(error, are_same_shape(a, b), &
                   "expected '"//to_string(.true.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d2_r64_should_return_true_when_input_same_shape

    subroutine d2_r64_should_return_false_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real64) :: a(3, 2)
        real(real64) :: b(2, 3)

        call check(error,.not. are_same_shape(a, b), &
                   "expected '"//to_string(.false.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d2_r64_should_return_false_when_input_different_shape

    subroutine d3_r64_should_return_true_when_input_same_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real64) :: a(3, 2, 4)
        real(real64) :: b(3, 2, 4)

        call check(error, are_same_shape(a, b), &
                   "expected '"//to_string(.true.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d3_r64_should_return_true_when_input_same_shape

    subroutine d3_r64_should_return_false_when_input_different_shape(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real64) :: a(3, 2, 4)
        real(real64) :: b(2, 3, 1)

        call check(error,.not. are_same_shape(a, b), &
                   "expected '"//to_string(.false.)// &
                   "', but got '"//to_string(are_same_shape(a, b))//"'")
    end subroutine d3_r64_should_return_false_when_input_different_shape
end module test_sameShape_compareArrayShape_unitTests_areSameShape_real