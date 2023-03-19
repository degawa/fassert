module test_common_halt_unitTests
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: fassert_common_status
    use :: fassert_common_halt
    implicit none
    private
    public :: halt_should_not_stop_when_input_passed_status
    public :: halt_should_error_stop_when_input_failed_status

    integer(int32), private, parameter :: exit_success = 0

contains
    subroutine halt_should_not_stop_when_input_passed_status(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int32) :: actual_exit_status
        character(:), allocatable :: command

        command = construct_command("halt_on_failure_w_passed_status")

        call execute_command_line(command=command, wait=.true., exitstat=actual_exit_status)

        call check(error, actual_exit_status == exit_success, &
                   "expected "//to_string(exit_success)// &
                   ", but got "//to_string(actual_exit_status))
    end subroutine halt_should_not_stop_when_input_passed_status

    subroutine halt_should_error_stop_when_input_failed_status(error)
        use, intrinsic :: iso_fortran_env
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        integer(int32) :: actual_exit_status
        character(:), allocatable :: command

        command = construct_command("halt_on_failure_w_failed_status")

        call execute_command_line(command=command, wait=.true., exitstat=actual_exit_status)

        call check(error, actual_exit_status /= exit_success, &
                   "expected not "//to_string(exit_success)// &
                   ", but got "//to_string(actual_exit_status))
    end subroutine halt_should_error_stop_when_input_failed_status

    function construct_command(exe) result(command)
        implicit none
        character(*), intent(in) :: exe
        character(:), allocatable :: command

        character(:), allocatable :: descriptor, compiler

        descriptor = get_descriptor()
        compiler = get_compiler_name()
        command = 'fpm run'// &
                  ' --compiler '//compiler// &
                  ' --example '//exe// &
                  ' > '//descriptor
        ! the compiler flag for enabling preprocess is not necessary
    end function construct_command

    function get_descriptor() result(descriptor)
        implicit none
        character(:), allocatable :: descriptor

        descriptor = "NUL 2>&1" ! windows OS
#if defined(__unix__) || (__linux__)
        descriptor = "/dev/null 2>&1" ! unix/linux OS
#endif
    end function get_descriptor

    function get_compiler_name() result(compiler)
        implicit none
        character(:), allocatable :: compiler

#if defined(__GFORTRAN__)
        compiler = "gfortran"
#elif defined(__INTEL_COMPILER)
        compiler = "ifort"
#elif defined(NAGFOR)
        compiler = "nagfor"
#endif
    end function get_compiler_name
end module test_common_halt_unitTests
