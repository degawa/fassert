module test_common_message_unitTests_toString
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: fassette_common_message
    implicit none
    private
    public :: toString_should_return_T_when_input_true
    public :: toString_should_return_F_when_input_false

contains
    subroutine toString_should_return_T_when_input_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, to_string(.true.) == "T", &
                   "expected "//"T" &
                   //", but got "//to_string(.true.))
        if (occurred(error)) return
    end subroutine toString_should_return_T_when_input_true

    subroutine toString_should_return_F_when_input_false(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, to_string(.false.) == "F", &
                   "expected "//"F" &
                   //", but got "//to_string(.false.))
        if (occurred(error)) return
    end subroutine toString_should_return_F_when_input_false
end module test_common_message_unitTests_toString
