module test_common_message_unitTests_output
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: fassert_common_message
    use :: fassert_common_status
    implicit none
    private
    public :: doesOutputMsg_should_return_true_when_input_false
    public :: doesOutputMsg_should_return_false_when_input_true
    public :: doesOutputMsg_should_return_true_when_no_input
    public :: doesNotOutputMsg_should_return_true_when_input_true
    public :: doesNotOutputMsg_should_return_false_when_input_false
    public :: doesNotOutputMsg_should_return_false_when_no_input
    public :: isVerboseOutput_should_return_true_when_input_parameters
    public :: isVerboseOutput_should_return_false_when_input_parameters

contains
    subroutine doesOutputMsg_should_return_true_when_input_false(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, does_output_message(.false.) .eqv. .true., &
                   "expected "//to_string(.true.) &
                   //", but got "//to_string(does_output_message(.false.)))
        if (occurred(error)) return
    end subroutine doesOutputMsg_should_return_true_when_input_false

    subroutine doesOutputMsg_should_return_false_when_input_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, does_output_message(.true.) .eqv. .false., &
                   "expected "//to_string(.false.) &
                   //", but got "//to_string(does_output_message(.true.)))
        if (occurred(error)) return
    end subroutine doesOutputMsg_should_return_false_when_input_true

    subroutine doesOutputMsg_should_return_true_when_no_input(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, does_output_message() .eqv. .true., &
                   "expected "//to_string(.true.) &
                   //", but got "//to_string(does_output_message()))
        if (occurred(error)) return
    end subroutine doesOutputMsg_should_return_true_when_no_input

    subroutine doesNotOutputMsg_should_return_true_when_input_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, does_not_output_message(.true.) .eqv. .true., &
                   "expected "//to_string(.true.) &
                   //", but got "//to_string(does_not_output_message(.true.)))
        if (occurred(error)) return
    end subroutine doesNotOutputMsg_should_return_true_when_input_true

    subroutine doesNotOutputMsg_should_return_false_when_input_false(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, does_not_output_message(.false.) .eqv. .false., &
                   "expected "//to_string(.false.) &
                   //", but got "//to_string(does_not_output_message(.false.)))
        if (occurred(error)) return
    end subroutine doesNotOutputMsg_should_return_false_when_input_false

    subroutine doesNotOutputMsg_should_return_false_when_no_input(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, does_not_output_message() .eqv. .false., &
                   "expected "//to_string(.false.) &
                   //", but got "//to_string(does_not_output_message()))
        if (occurred(error)) return
    end subroutine doesNotOutputMsg_should_return_false_when_no_input

    subroutine isVerboseOutput_should_return_true_when_input_parameters(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        logical :: stat
        logical, parameter :: expected = .true.
        character(1), parameter :: T = "T"

        ! f - -
        stat = is_verbose_output(failed)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(failed, -, -), "// &
                   "expected "//T//", but got "//to_string(stat))
        if (occurred(error)) return

        ! f F -
        stat = is_verbose_output(failed, verbose=.true.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(failed, .false., -), "// &
                   "expected "//T//", but got "//to_string(stat))
        if (occurred(error)) return

        ! f T -
        stat = is_verbose_output(failed, verbose=.true.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(failed, .true., -), "// &
                   "expected "//T//", but got "//to_string(stat))
        if (occurred(error)) return

        ! p T -
        stat = is_verbose_output(passed, verbose=.true.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(passed, .true., -), "// &
                   "expected "//T//", but got "//to_string(stat))
        if (occurred(error)) return

        ! f - T
        stat = is_verbose_output(failed, quiet=.true.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(failed, -, .true.), "// &
                   "expected "//T//", but got "//to_string(stat))
        if (occurred(error)) return

        ! f - F
        stat = is_verbose_output(failed, quiet=.false.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(failed, -, .false.), "// &
                   "expected "//T//", but got "//to_string(stat))
        if (occurred(error)) return

        ! f T F
        stat = is_verbose_output(failed, verbose=.true., quiet=.false.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(failed, .true., .false.), "// &
                   "expected "//T//", but got "//to_string(stat))
        if (occurred(error)) return

        ! f F F
        stat = is_verbose_output(failed, verbose=.false., quiet=.false.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(failed, .false., .false.), "// &
                   "expected "//T//", but got "//to_string(stat))
        if (occurred(error)) return

        ! f T T
        stat = is_verbose_output(failed, verbose=.true., quiet=.true.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(failed, .false., .false.), "// &
                   "expected "//T//", but got "//to_string(stat))
        if (occurred(error)) return

        ! p T F
        stat = is_verbose_output(passed, verbose=.true., quiet=.false.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(passed, .true., .false.), "// &
                   "expected "//T//", but got "//to_string(stat))
        if (occurred(error)) return
    end subroutine isVerboseOutput_should_return_true_when_input_parameters

    subroutine isVerboseOutput_should_return_false_when_input_parameters(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler
        logical :: stat
        logical, parameter :: expected = .false.
        character(1), parameter :: F = "F"

        ! p - -
        stat = is_verbose_output(passed)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(passed, -, -), "// &
                   "expected "//F//", but got "//to_string(stat))
        if (occurred(error)) return

        ! p F -
        stat = is_verbose_output(passed, verbose=.false.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(passed, .false., -), "// &
                   "expected "//F//", but got "//to_string(stat))
        if (occurred(error)) return

        ! p F T
        stat = is_verbose_output(passed, verbose=.false., quiet=.true.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(passed, .false., .true.), "// &
                   "expected "//F//", but got "//to_string(stat))
        if (occurred(error)) return

        ! p - T
        stat = is_verbose_output(passed, quiet=.true.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(passed, -, .true.), "// &
                   "expected "//F//", but got "//to_string(stat))
        if (occurred(error)) return

        ! p - F
        stat = is_verbose_output(passed, quiet=.false.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(passed, -, .false.), "// &
                   "expected "//F//", but got "//to_string(stat))
        if (occurred(error)) return

        ! p F T
        stat = is_verbose_output(passed, verbose=.false., quiet=.true.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(passed, .false., .true.), "// &
                   "expected "//F//", but got "//to_string(stat))
        if (occurred(error)) return

        ! p F F
        stat = is_verbose_output(passed, verbose=.false., quiet=.false.)
        call check(error, stat .eqv. expected, &
                   "is_verbose_output(passed, .false., .false.), "// &
                   "expected "//F//", but got "//to_string(stat))
        if (occurred(error)) return
    end subroutine isVerboseOutput_should_return_false_when_input_parameters
end module test_common_message_unitTests_output
