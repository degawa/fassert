module test_logical_expectTrue_unitTests_expectTrue
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: strings_enclose
    use :: par_funnel
    use :: fassert_common_unit
    use :: test_logical_expectTrue_unitTests_expectTrue_common
    use :: expectLogical
    implicit none
    private
    public :: expectTrue_parameterized_test

contains
    subroutine expectTrue_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
               !v success cases
               !-v expect_true(actual,name,stat)
               new_test_parameter(arguments="actual=true test_name='a unit test'", &
                                  expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_true(actual,name,stat,verbose=F)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_true(actual,name,stat,verbose=F,quiet=F)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_true(actual,name,stat,verbose=F,quiet=T)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=true") &
               !-v expect_true(actual,name,stat,verbose=T)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true", &
                                    expected="message='PASSED: a unit test"//new_line_char// &
                                    "    Expected: T"//new_line_char// &
                                    "    Actual  : T' stat_exp=true") &
               !-v expect_true(actual,name,stat,verbose=T,quiet=F)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='PASSED: a unit test"//new_line_char// &
                                    "    Expected: T"//new_line_char// &
                                    "    Actual  : T' stat_exp=true") &
               !-v expect_true(actual,name,stat,verbose=T,quiet=T)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='    Expected: T"//new_line_char// &
                                    "    Actual  : T' stat_exp=true") &
               !v failed cases
               !-v expect_true(actual,name,stat)
               , new_test_parameter(arguments="actual=false test_name='a unit test'", &
                                    expected="message='FAILED: a unit test"//new_line_char// &
                                    "    Expected: T"//new_line_char// &
                                    "    Actual  : F' stat_exp=false") &
               !-v expect_true(actual,name,stat,verbose=F)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false", &
                                    expected="message='FAILED: a unit test"//new_line_char// &
                                    "    Expected: T"//new_line_char// &
                                    "    Actual  : F' stat_exp=false") &
               !-v expect_true(actual,name,stat,verbose=F,quiet=F)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='FAILED: a unit test"//new_line_char// &
                                    "    Expected: T"//new_line_char// &
                                    "    Actual  : F' stat_exp=false") &
               !-v expect_true(actual,name,stat,verbose=F,quiet=T)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=false") &
               !-v expect_true(actual,name,stat,verbose=T)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true", &
                                    expected="message='FAILED: a unit test"//new_line_char// &
                                    "    Expected: T"//new_line_char// &
                                    "    Actual  : F' stat_exp=false") &
               !-v expect_true(actual,name,stat,verbose=T,quiet=F)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='FAILED: a unit test"//new_line_char// &
                                    "    Expected: T"//new_line_char// &
                                    "    Actual  : F' stat_exp=false") &
               !-v expect_true(actual,name,stat,verbose=T,quiet=T)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='    Expected: T"//new_line_char// &
                                    "    Actual  : F' stat_exp=false") &
               ], &
               optional_args=[argument("verbose"), argument("quiet")], &
               replace_new_line=.true.)

        call runner(error, spec, run_test_cases)
    contains
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            ! arguments
            logical :: actual, stat, verbose, expected_failure, quiet
            character(256) :: test_name

            ! expected
            character(256) :: message
            logical :: stat_exp

            block
                character(:), allocatable :: case_name, buffer
                integer(int32) :: case, scratch_unit_number
                type(test_parameter_type) :: param
                type(arguments_presence_type) :: arg_pres
                character(:), allocatable :: msg_exp

                logical :: cond

                do case = 1, results%get_number_of_test_cases()
                    call setup_case(spec, case, param, scratch_unit_number, case_name, arg_pres, &
                                    actual, test_name, stat, verbose, expected_failure, quiet, &
                                    message, stat_exp)

                    write (output_unit, '(12X, "- ",A)') case_name

                    if (arg_pres.has. [.false., .false.]) &
                        call expect_true(actual, trim(test_name), stat)
                    if (arg_pres.has. [.true., .false.]) &
                        call expect_true(actual, trim(test_name), stat, verbose=verbose)
                    if (arg_pres.has. [.false., .true.]) &
                        call expect_true(actual, trim(test_name), stat, quiet=quiet)
                    if (arg_pres.has. [.true., .true.]) &
                        call expect_true(actual, trim(test_name), stat, verbose=verbose, quiet=quiet)

                    msg_exp = replace_new_line_mark(trim(message))
                    call get_all_actual_value(error, scratch_unit_number, buffer)
                    if (occurred(error)) then
                        call results%check_test(case, .false., error%message)
                        cycle
                    end if

                    cond = all([stat .eqv. stat_exp, &
                                len(buffer) == len_trim(msg_exp), &
                                buffer == trim(msg_exp)])

                    call results%check_test(case, cond, &
                                            failure_message(case_name, trim(msg_exp), buffer, [stat_exp, stat]))

                    call teardown_case(scratch_unit_number, buffer)
                end do
            end block
        end subroutine run_test_cases
    end subroutine expectTrue_parameterized_test
end module test_logical_expectTrue_unitTests_expectTrue
