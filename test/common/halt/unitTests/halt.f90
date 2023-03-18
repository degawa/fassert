module test_common_halt_unitTests
    use :: testdrive, only:error_type
    use :: fassert_common_status
    use :: fassert_common_halt
    implicit none
    private
    public :: halt_should_not_stop_when_input_passed_status

contains
    subroutine halt_should_not_stop_when_input_passed_status(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call halt_on_failure(passed)
    end subroutine halt_should_not_stop_when_input_passed_status
end module test_common_halt_unitTests
