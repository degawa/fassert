module test_logical_expectFalse_unitTests_expectFalseMsg
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: strings_enclose
    use :: par_funnel
    use :: test_logical_expectFalse_unitTests_expectFalse_common
    use :: expectLogical
    implicit none
    private
    public :: expectFalseMsg_parameterized_test

contains
    subroutine expectFalseMsg_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
               !v success cases
               !-v expect_false(actual,name,stat)
               new_test_parameter(arguments="actual=false test_name='a unit test'", &
                                  expected="message='PASSED: a unit test' stat_exp=true alloced=true") &
               !-v expect_false(actual,name,stat,verbose=F)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true alloced=true") &
               !-v expect_false(actual,name,stat,verbose=F,quiet=F)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true alloced=true") &
               !-v expect_false(actual,name,stat,verbose=F,quiet=T)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=true alloced=false") &
               !-v expect_false(actual,name,stat,verbose=T)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true", &
                                    expected="message='"// &
                                    "PASSED: a unit test"//new_line_char// &
                                    "    Expected: F"//new_line_char// &
                                    "    Actual  : F' stat_exp=true alloced=true") &
               !-v expect_false(actual,name,stat,verbose=T,quiet=F)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='"// &
                                    "PASSED: a unit test"//new_line_char// &
                                    "    Expected: F"//new_line_char// &
                                    "    Actual  : F' stat_exp=true alloced=true") &
               !-v expect_false(actual,name,stat,verbose=T,quiet=T)
               , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='"// &
                                    "    Expected: F"//new_line_char// &
                                    "    Actual  : F' stat_exp=true alloced=true") &
               !v failed cases
               !-v expect_false(actual,name,stat)
               , new_test_parameter(arguments="actual=true test_name='a unit test'", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: F"//new_line_char// &
                                    "    Actual  : T' stat_exp=false alloced=true") &
               !-v expect_false(actual,name,stat,verbose=F)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: F"//new_line_char// &
                                    "    Actual  : T' stat_exp=false alloced=true") &
               !-v expect_false(actual,name,stat,verbose=F,quiet=F)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: F"//new_line_char// &
                                    "    Actual  : T' stat_exp=false alloced=true") &
               !-v expect_false(actual,name,stat,verbose=F,quiet=T)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=false alloced=false") &
               !-v expect_false(actual,name,stat,verbose=T)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: F"//new_line_char// &
                                    "    Actual  : T' stat_exp=false alloced=true") &
               !-v expect_false(actual,name,stat,verbose=T,quiet=F)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: F"//new_line_char// &
                                    "    Actual  : T' stat_exp=false alloced=true") &
               !-v expect_false(actual,name,stat,verbose=T,quiet=T)
               , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='"// &
                                    "    Expected: F"//new_line_char// &
                                    "    Actual  : T' stat_exp=false alloced=true") &
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
            logical :: stat_exp, alloced

            block
                character(:), allocatable :: case_name
                integer(int32) :: case
                type(test_parameter_type) :: param
                type(arguments_presence_type) :: arg_pres
                character(:), allocatable :: msg_exp, message_act

                logical :: alloced_act, cond

                do case = 1, results%get_number_of_test_cases()
                    call setup_case(spec, case, param, case_name, arg_pres, &
                                    actual, test_name, stat, verbose, expected_failure, quiet, &
                                    message, stat_exp, alloced)

                    write (output_unit, '(12X, "- ",A)') case_name

                    if (arg_pres.has. [.false., .false.]) &
                        call expect_false(actual, trim(test_name), stat, &
                                          output_message=message_act)
                    if (arg_pres.has. [.true., .false.]) &
                        call expect_false(actual, trim(test_name), stat, verbose=verbose, &
                                          output_message=message_act)
                    if (arg_pres.has. [.false., .true.]) &
                        call expect_false(actual, trim(test_name), stat, quiet=quiet, &
                                          output_message=message_act)
                    if (arg_pres.has. [.true., .true.]) &
                        call expect_false(actual, trim(test_name), stat, verbose=verbose, quiet=quiet, &
                                          output_message=message_act)

                    alloced_act = allocated(message_act)
                    cond = alloced_act .eqv. alloced
                    if (.not. alloced_act) message_act = ""

                    msg_exp = replace_new_line_mark(trim(message))
                    cond = all([cond, &
                                stat .eqv. stat_exp, &
                                len(message_act) == len_trim(msg_exp), &
                                message_act == trim(msg_exp)])

                    call results%check_test(case, cond, &
                                            failure_message(case_name, trim(msg_exp), message_act, [stat_exp, stat], &
                                                            [alloced, alloced_act]))

                    call teardown_case(message_act)
                end do
            end block
        end subroutine run_test_cases
    end subroutine expectFalseMsg_parameterized_test
end module test_logical_expectFalse_unitTests_expectFalseMsg
