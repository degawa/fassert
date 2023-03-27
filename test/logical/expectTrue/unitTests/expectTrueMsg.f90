module test_logical_expectTrue_unitTests_expectTrueMsg
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: strings_enclose
    use :: test_logical_expectTrue_unitTests_expectTrue_common
    use :: expectLogical
    implicit none
    private
    public :: expectTrueMsg_parameterized_test

contains
    subroutine expectTrueMsg_parameterized_test(error)
        use :: par_funnel
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                 !v success cases
                 !-v expect_true(actual,name,stat)
                 new_test_parameter(arguments="actual=true test_name='a unit test'", &
                                    expected="message='PASSED: a unit test' stat_exp=true alloced=true") &
                 !-v expect_true(actual,name,stat,verbose=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false", &
                                      expected="message='PASSED: a unit test' stat_exp=true alloced=true") &
                 !-v expect_true(actual,name,stat,verbose=F,quiet=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=false", &
                                      expected="message='PASSED: a unit test' stat_exp=true alloced=true") &
                 !-v expect_true(actual,name,stat,verbose=F,quiet=T)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=true", &
                                      expected="message='' stat_exp=true alloced=false") &
                 !-v expect_true(actual,name,stat,verbose=T)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true", &
                                      expected="message='PASSED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : T' stat_exp=true alloced=true") &
                 !-v expect_true(actual,name,stat,verbose=T,quiet=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=false", &
                                      expected="message='PASSED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : T' stat_exp=true alloced=true") &
                 !-v expect_true(actual,name,stat,verbose=T,quiet=T)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=true", &
                                      expected="message='    Expected: T\n"// &
                                      "    Actual  : T' stat_exp=true alloced=true") &
                 !v failed cases
                 !-v expect_true(actual,name,stat)
                 , new_test_parameter(arguments="actual=false test_name='a unit test'", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false alloced=true") &
                 !-v expect_true(actual,name,stat,verbose=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false alloced=true") &
                 !-v expect_true(actual,name,stat,verbose=F,quiet=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=false", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false alloced=true") &
                 !-v expect_true(actual,name,stat,verbose=F,quiet=T)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=true", &
                                      expected="message='' stat_exp=false alloced=false") &
                 !-v expect_true(actual,name,stat,verbose=T)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false alloced=true") &
                 !-v expect_true(actual,name,stat,verbose=T,quiet=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=false", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false alloced=true") &
                 !-v expect_true(actual,name,stat,verbose=T,quiet=T)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=true", &
                                      expected="message='    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false alloced=true") &
                 ]

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            use :: strings_enclose
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            ! arguments
            logical :: actual, stat, verbose, expected_failure, quiet
            character(256) :: test_name

            ! expected
            character(256) :: message
            logical :: stat_exp, alloced

            call results%construct(params)

            block
                character(:), allocatable :: case_name
                integer(int32) :: case
                type(arguments_presence_type) :: arg_pres
                character(:), allocatable :: msg_exp, message_act

                logical :: alloced_act, cond

                do case = 1, results%get_number_of_test_cases()
                    call setup_case(params(case), case_name, arg_pres, &
                                    actual, test_name, stat, verbose, expected_failure, quiet, &
                                    message, stat_exp, alloced)

                    write (output_unit, '(12X, "- ",A)') case_name

                    if (arg_pres.has. [.false., .false.]) &
                        call expect_true(actual, trim(test_name), stat, &
                                         output_message=message_act)
                    if (arg_pres.has. [.true., .false.]) &
                        call expect_true(actual, trim(test_name), stat, verbose=verbose, &
                                         output_message=message_act)
                    if (arg_pres.has. [.false., .true.]) &
                        call expect_true(actual, trim(test_name), stat, quiet=quiet, &
                                         output_message=message_act)
                    if (arg_pres.has. [.true., .true.]) &
                        call expect_true(actual, trim(test_name), stat, verbose=verbose, quiet=quiet, &
                                         output_message=message_act)

                    alloced_act = allocated(message_act)
                    cond = alloced_act .eqv. alloced
                    if (.not. alloced_act) message_act = ""

                    msg_exp = replace_all(trim(message), "\n", new_line(" "))
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
    end subroutine expectTrueMsg_parameterized_test
end module test_logical_expectTrue_unitTests_expectTrueMsg
