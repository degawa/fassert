module test_logical_expectFalse_unitTests_expectFalse
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: strings_enclose
    use :: fassert_common_unit
    use :: test_logical_expectFalse_unitTests_expectFalse_common
    use :: expectLogical
    implicit none
    private
    public :: expectFalse_parameterized_test

contains
    subroutine expectFalse_parameterized_test(error)
        use :: par_funnel
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                 !v success cases
                 !-v expect_false(actual,name,stat)
                 new_test_parameter(arguments="actual=false test_name='a unit test'", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
                 !-v expect_false(actual,name,stat,verbose=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false", &
                                      expected="message='PASSED: a unit test' stat_exp=true") &
                 !-v expect_false(actual,name,stat,verbose=F,quiet=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=false", &
                                      expected="message='PASSED: a unit test' stat_exp=true") &
                 !-v expect_false(actual,name,stat,verbose=F,quiet=T)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=false quiet=true", &
                                      expected="message='' stat_exp=true") &
                 !-v expect_false(actual,name,stat,verbose=T)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true", &
                                      expected="message='PASSED: a unit test\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : F' stat_exp=true") &
                 !-v expect_false(actual,name,stat,verbose=T,quiet=F)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=false", &
                                      expected="message='PASSED: a unit test\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : F' stat_exp=true") &
                 !-v expect_false(actual,name,stat,verbose=T,quiet=T)
                 , new_test_parameter(arguments="actual=false test_name='a unit test' verbose=true quiet=true", &
                                      expected="message='    Expected: F\n"// &
                                      "    Actual  : F' stat_exp=true") &
                 !v failed cases
                 !-v expect_false(actual,name,stat)
                 , new_test_parameter(arguments="actual=true test_name='a unit test'", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : T' stat_exp=false") &
                 !-v expect_false(actual,name,stat,verbose=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : T' stat_exp=false") &
                 !-v expect_false(actual,name,stat,verbose=F,quiet=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=false", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : T' stat_exp=false") &
                 !-v expect_false(actual,name,stat,verbose=F,quiet=T)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=false quiet=true", &
                                      expected="message='' stat_exp=false") &
                 !-v expect_false(actual,name,stat,verbose=T)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : T' stat_exp=false") &
                 !-v expect_false(actual,name,stat,verbose=T,quiet=F)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=false", &
                                      expected="message='FAILED: a unit test\n"// &
                                      "    Expected: F\n"// &
                                      "    Actual  : T' stat_exp=false") &
                 !-v expect_false(actual,name,stat,verbose=T,quiet=T)
                 , new_test_parameter(arguments="actual=true test_name='a unit test' verbose=true quiet=true", &
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
                        call expect_false(actual, trim(test_name), stat)
                    if (arg_pres.has. [.true., .false.]) &
                        call expect_false(actual, trim(test_name), stat, verbose=verbose)
                    if (arg_pres.has. [.false., .true.]) &
                        call expect_false(actual, trim(test_name), stat, quiet=quiet)
                    if (arg_pres.has. [.true., .true.]) &
                        call expect_false(actual, trim(test_name), stat, verbose=verbose, quiet=quiet)

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
        ! subroutine setup_case(param, unit_number, case_name, arg_pres, &
        !                       actual, test_name, stat, verbose, expected_failure, quiet, &
        !                       message, stat_exp)
        !     use :: newunit
        !     type(test_parameter_type), intent(in) :: param
        !     integer(int32), intent(inout) :: unit_number
        !     character(:), allocatable, intent(out) :: case_name
        !     type(arguments_presence_type), intent(inout) :: arg_pres

        !     ! arguments
        !     logical, intent(out) :: actual, stat, verbose, expected_failure, quiet
        !     character(256), intent(out) :: test_name

        !     ! expected
        !     character(256), intent(out) :: message
        !     logical, intent(out) :: stat_exp

        !     namelist /arguments/ actual, test_name, stat, verbose, expected_failure, quiet
        !     namelist /expected/ message, stat_exp

        !     read (unit=param%arguments_namelist, nml=arguments)
        !     read (unit=param%expected_namelist, nml=expected)

        !     case_name = "it should get "//enclose(param%expected(), "{")// &
        !                 " when input "//enclose(param%arguments(), "{")

        !     arg_pres = arguments_presence([param%presented("verbose"), &
        !                                    param%presented("quiet")])

        !     unit_number = get_newunit_number()
        !     call set_assertion_message_unit(unit_number)
        !     open (unit=unit_number, status="scratch")
        ! end subroutine setup_case

        ! subroutine teardown_case(unit_number, buffer)
        !     integer(int32), intent(in) :: unit_number
        !     character(:), allocatable, intent(inout) :: buffer
        !     close (unit_number)
        !     call set_assertion_message_unit(output_unit)
        !     deallocate (buffer)
        ! end subroutine teardown_case

        ! function replace_all(str, it, with) result(replaced)
        !     implicit none
        !     character(*), intent(in) :: str
        !     character(*), intent(in) :: it
        !     character(*), intent(in) :: with
        !     character(:), allocatable :: replaced

        !     integer(int32) :: len_it, len_with, idx_it
        !     len_it = len(it)
        !     len_with = len(with)

        !     replaced = str

        !     idx_it = index(replaced, it, back=.false.)
        !     do while (idx_it > 0)
        !         replaced = replaced(1:idx_it - 1)//with//replaced(idx_it + len_it:)
        !         idx_it = index(replaced, it, back=.false.)
        !     end do
        ! end function replace_all
        ! function failure_message(case_name, expected, actual, stat) result(msg)
        !     implicit none
        !     character(*), intent(in) :: case_name
        !     character(*), intent(in) :: expected
        !     character(*), intent(in) :: actual
        !     logical, intent(in), optional :: stat(2)
        !     character(:), allocatable :: msg

        !     character(:), allocatable :: stat_exp, stat_act

        !     stat_exp = ""
        !     stat_act = ""
        !     if (present(stat)) then
        !         stat_exp = to_string(stat(1))
        !         stat_act = to_string(stat(2))
        !     end if
        !     msg = case_name//new_line(" ")// &
        !           "    expected : "//expected//" "//stat_exp//new_line(" ")// &
        !           "    actual   : "//actual//" "//stat_act
        ! end function failure_message
    end subroutine expectFalse_parameterized_test
end module test_logical_expectFalse_unitTests_expectFalse
