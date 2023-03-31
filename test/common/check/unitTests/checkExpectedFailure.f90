module test_common_check_unitTests_expectedFailure
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_actual_value
    use :: strings_enclose
    use :: par_funnel
    use :: fassert_common_unit
    use :: test_common_check_unitTests_common
    use :: fassert_common_check
    implicit none
    private
    public :: checkExpecFail_parameterized_test

contains
    subroutine checkExpecFail_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
               new_test_parameter(arguments="test_name='a unit test' condition=false", &
                                  expected="message='PASSED: a unit test [expected failure]'") &
               , new_test_parameter(arguments="test_name='a unit test' condition=true", &
                                    expected="message='FAILED: a unit test [unexpected pass]'") &
               , new_test_parameter(arguments="test_name='a unit test' condition=false stat=false", &
                                    expected="message='PASSED: a unit test [expected failure]' stat_exp=true") &
               , new_test_parameter(arguments="test_name='a unit test' condition=true stat=true", &
                                    expected="message='FAILED: a unit test [unexpected pass]' stat_exp=false") &
               , new_test_parameter(arguments="test_name='a unit test' condition=false quiet=false", &
                                    expected="message='PASSED: a unit test [expected failure]'") &
               , new_test_parameter(arguments="test_name='a unit test' condition=true quiet=false", &
                                    expected="message='FAILED: a unit test [unexpected pass]'") &
               , new_test_parameter(arguments="test_name='a unit test' condition=false quiet=true", &
                                    expected="message=''") &
               , new_test_parameter(arguments="test_name='a unit test' condition=true quiet=true", &
                                    expected="message=''") &
               , new_test_parameter(arguments="test_name='a unit test' condition=false stat=false quiet=false", &
                                    expected="message='PASSED: a unit test [expected failure]' stat_exp=true") &
               , new_test_parameter(arguments="test_name='a unit test' condition=true stat=true quiet=false", &
                                    expected="message='FAILED: a unit test [unexpected pass]' stat_exp=false") &
               , new_test_parameter(arguments="test_name='a unit test' condition=false stat=false quiet=true", &
                                    expected="message='' stat_exp=true") &
               , new_test_parameter(arguments="test_name='a unit test' condition=true stat=true quiet=true", &
                                    expected="message='' stat_exp=false") &
               ], &
               optional_args=[argument("stat"), argument("quiet")] &
               )

        call runner(error, spec, run_test_cases)
    end subroutine checkExpecFail_parameterized_test

    subroutine run_test_cases(spec, results)
        type(parameterization_spec_type), intent(in) :: spec
        type(test_results_type), intent(inout) :: results

        ! arguments
        character(256) :: test_name
        logical :: condition, stat, quiet

        ! expected
        character(256) :: message
        logical :: stat_exp

        block
            character(:), allocatable :: case_name, buffer
            integer(int32) :: case, scratch_unit_number
            type(arguments_presence_type) :: arg_pres
            type(test_parameter_type) :: param
            type(error_type), allocatable :: error
            logical :: cond

            do case = 1, results%get_number_of_test_cases()
                call setup_case(spec, case, param, scratch_unit_number, case_name, arg_pres, &
                                test_name, condition, stat, quiet, &
                                message, stat_exp)

                write (output_unit, '(12X, "- ",A)') case_name

                if (arg_pres.has. [.false., .false.]) &
                    call check_expected_failure(condition, trim(test_name))
                if (arg_pres.has. [.true., .false.]) &
                    call check_expected_failure(condition, trim(test_name), stat=stat)
                if (arg_pres.has. [.false., .true.]) &
                    call check_expected_failure(condition, trim(test_name), quiet=quiet)
                if (arg_pres.has. [.true., .true.]) &
                    call check_expected_failure(condition, trim(test_name), stat=stat, quiet=quiet)

                call get_actual_value(error, scratch_unit_number, buffer)
                if (occurred(error)) then
                    call results%check_test(case, .false., error%message)
                    cycle ! not return, continue testing the remaining cases
                end if

                cond = all([len_trim(buffer) == len_trim(message), &
                            trim(buffer) == trim(message)])

                if (param%presented("stat")) then
                    cond = cond .and. (stat .eqv. stat_exp)
                    call results%check_test(case, cond, &
                                            failure_message(case_name, trim(message), trim(buffer), [stat_exp, stat]))
                else
                    call results%check_test(case, cond, &
                                            failure_message(case_name, trim(message), trim(buffer)))
                end if

                call teardown_case(scratch_unit_number, buffer)
            end do
        end block
    end subroutine run_test_cases
end module test_common_check_unitTests_expectedFailure
