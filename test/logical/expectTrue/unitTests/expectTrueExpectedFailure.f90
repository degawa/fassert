module test_logical_expectTrue_unitTests_expectTrueExpectedFailure
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: strings_enclose
    use :: fassert_common_unit
    use :: test_logical_expectTrue_unitTests_expectTrue_common
    use :: expectLogical
    implicit none
    private
    !T: true, F: false
    public :: expectTrue_expected_failure_F_parameterized_test
    public :: expectTrue_expected_failure_T_parameterized_test

contains
    subroutine expectTrue_expected_failure_F_parameterized_test(error)
        use :: par_funnel
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                 !v success cases
                 !-v expect_true(actual,name,stat,expected_failure=F)
                 new_test_parameter(arguments="actual=true test_name='a unit test' "// &
                                    "expected_failure=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false "// &
                                      "expected_failure=false", &
                                      expected="message='PASSED: a unit test' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F,quiet=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=false "// &
                                      "expected_failure=false", &
                                      expected="message='PASSED: a unit test' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F,quiet=T)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=true "// &
                                      "expected_failure=false", &
                                      expected="message='' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=T,expected_failure=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true "// &
                                      "expected_failure=false", &
                                      expected="message='PASSED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : T' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=T,expected_failure=F,quiet=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=false "// &
                                      "expected_failure=false", &
                                      expected="message='PASSED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : T' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=T,expected_failure=F,quiet=T)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=true "// &
                                      "expected_failure=false", &
                                      expected="message='    Expected: T\n"// &
                                      "    Actual  : T' stat_exp=true") &
                 !v failed cases
                 !-v expect_true(actual,name,stat,expected_failure=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' "// &
                                      "expected_failure=false", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false "// &
                                      "expected_failure=false", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F,quiet=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=false "// &
                                      "expected_failure=false", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F,quiet=T)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=true "// &
                                      "expected_failure=false", &
                                      expected="message='' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=T)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true "// &
                                      "expected_failure=false", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=T,expected_failure=F,quiet=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=false "// &
                                      "expected_failure=false", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=T,expected_failure=F,quiet=T)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=true "// &
                                      "expected_failure=false", &
                                      expected="message='    Expected: T\n"// &
                                      "    Actual  : F' stat_exp=false") &
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
            logical :: stat_exp

            call results%construct(params)

            block
                character(:), allocatable :: case_name, buffer
                integer(int32) :: case, scratch_unit_number
                type(arguments_presence_type) :: arg_pres
                character(:), allocatable :: msg_exp

                logical :: cond

                do case = 1, results%get_number_of_test_cases()
                    call setup_case(params(case), scratch_unit_number, case_name, arg_pres, &
                                    actual, test_name, stat, verbose, expected_failure, quiet, &
                                    message, stat_exp)

                    write (output_unit, '(12X, "- ",A)') case_name

                    if (arg_pres.has. [.false., .false.]) &
                        call expect_true(actual, trim(test_name), stat, expected_failure=.false.)
                    if (arg_pres.has. [.true., .false.]) &
                        call expect_true(actual, trim(test_name), stat, verbose=verbose, expected_failure=.false.)
                    if (arg_pres.has. [.false., .true.]) &
                        call expect_true(actual, trim(test_name), stat, expected_failure=.false., quiet=quiet)
                    if (arg_pres.has. [.true., .true.]) &
                        call expect_true(actual, trim(test_name), stat, verbose=verbose, expected_failure=.false., quiet=quiet)

                    msg_exp = replace_all(trim(message), "\n", new_line(" "))
                    call get_all_actual_value(error, scratch_unit_number, buffer)
                    cond = all([stat .eqv. stat_exp, &
                                len(buffer) == len_trim(msg_exp), &
                                buffer == trim(msg_exp)])

                    call results%check_test(case, cond, &
                                            failure_message(case_name, trim(msg_exp), buffer, [stat_exp, stat]))

                    call teardown_case(scratch_unit_number, buffer)
                end do
            end block
        end subroutine run_test_cases
    end subroutine expectTrue_expected_failure_F_parameterized_test

    subroutine expectTrue_expected_failure_T_parameterized_test(error)
        use :: par_funnel
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                 !v expected failure cases
                 !-v expect_true(actual,name,stat,expected_failure=F)
                 new_test_parameter(arguments="actual=false test_name='a unit test' "// &
                                    "expected_failure=true", &
                                    expected="message='PASSED: a unit test [expected failure]' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false "// &
                                      "expected_failure=true", &
                                      expected="message='PASSED: a unit test [expected failure]' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F,quiet=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=false "// &
                                      "expected_failure=true", &
                                      expected="message='PASSED: a unit test [expected failure]' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F,quiet=T)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=true "// &
                                      "expected_failure=true", &
                                      expected="message='' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=T,expected_failure=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true "// &
                                      "expected_failure=true", &
                                      expected="message='PASSED: a unit test [expected failure]\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : F' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=T,expected_failure=F,quiet=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=false "// &
                                      "expected_failure=true", &
                                      expected="message='PASSED: a unit test [expected failure]\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : F' stat_exp=true") &
                 !-v expect_true(actual,name,stat,verbose=T,expected_failure=F,quiet=T)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=true "// &
                                      "expected_failure=true", &
                                      expected="message='    Expected: F\n"// &
                                      "    Actual  : F' stat_exp=true") &
                 !v failed cases
                 !-v expect_true(actual,name,stat,expected_failure=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' "// &
                                      "expected_failure=true", &
                                      expected="message='FAILED: a unit test [unexpected pass]\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : T' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false "// &
                                      "expected_failure=true", &
                                      expected="message='FAILED: a unit test [unexpected pass]\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : T' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F,quiet=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=false "// &
                                      "expected_failure=true", &
                                      expected="message='FAILED: a unit test [unexpected pass]\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : T' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=F,expected_failure=F,quiet=T)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=true "// &
                                      "expected_failure=true", &
                                      expected="message='' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=T)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true "// &
                                      "expected_failure=true", &
                                      expected="message='FAILED: a unit test [unexpected pass]\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : T' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=T,expected_failure=F,quiet=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=false "// &
                                      "expected_failure=true", &
                                      expected="message='FAILED: a unit test [unexpected pass]\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : T' stat_exp=false") &
                 !-v expect_true(actual,name,stat,verbose=T,expected_failure=F,quiet=T)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=true "// &
                                      "expected_failure=true", &
                                      expected="message='    Expected: F\n"// &
                                      "    Actual  : T' stat_exp=false") &
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
            logical :: stat_exp

            call results%construct(params)

            block
                character(:), allocatable :: case_name, buffer
                integer(int32) :: case, scratch_unit_number
                type(arguments_presence_type) :: arg_pres
                character(:), allocatable :: msg_exp

                logical :: cond

                do case = 1, results%get_number_of_test_cases()
                    call setup_case(params(case), scratch_unit_number, case_name, arg_pres, &
                                    actual, test_name, stat, verbose, expected_failure, quiet, &
                                    message, stat_exp)

                    write (output_unit, '(12X, "- ",A)') case_name

                    if (arg_pres.has. [.false., .false.]) &
                        call expect_true(actual, trim(test_name), stat, expected_failure=.true.)
                    if (arg_pres.has. [.true., .false.]) &
                        call expect_true(actual, trim(test_name), stat, verbose=verbose, expected_failure=.true.)
                    if (arg_pres.has. [.false., .true.]) &
                        call expect_true(actual, trim(test_name), stat, expected_failure=.true., quiet=quiet)
                    if (arg_pres.has. [.true., .true.]) &
                        call expect_true(actual, trim(test_name), stat, verbose=verbose, expected_failure=.true., quiet=quiet)

                    msg_exp = replace_all(trim(message), "\n", new_line(" "))
                    call get_all_actual_value(error, scratch_unit_number, buffer)
                    cond = all([stat .eqv. stat_exp, &
                                len(buffer) == len_trim(msg_exp), &
                                buffer == trim(msg_exp)])

                    call results%check_test(case, cond, &
                                            failure_message(case_name, trim(msg_exp), buffer, [stat_exp, stat]))

                    call teardown_case(scratch_unit_number, buffer)
                end do
            end block
        end subroutine run_test_cases
    end subroutine expectTrue_expected_failure_T_parameterized_test
end module test_logical_expectTrue_unitTests_expectTrueExpectedFailure
