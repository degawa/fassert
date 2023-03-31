module test_common_check_unitTests_trueMsg
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_actual_value
    use :: strings_enclose
    use :: par_funnel
    use :: test_common_check_unitTests_common
    use :: fassert_common_check
    implicit none
    private
    public :: checkTrueMsg_parameterized_test

contains
    subroutine checkTrueMsg_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
               new_test_parameter(arguments="test_name='a unit test' condition=true", &
                                  expected="message='PASSED: a unit test' alloced=true") &
               , new_test_parameter(arguments="test_name='a unit test' condition=false", &
                                    expected="message='FAILED: a unit test' alloced=true") &
               , new_test_parameter(arguments="test_name='a unit test' condition=true stat=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true alloced=true") &
               , new_test_parameter(arguments="test_name='a unit test' condition=false stat=true", &
                                    expected="message='FAILED: a unit test' stat_exp=false alloced=true") &
               , new_test_parameter(arguments="test_name='a unit test' condition=true quiet=false", &
                                    expected="message='PASSED: a unit test' alloced=true") &
               , new_test_parameter(arguments="test_name='a unit test' condition=false quiet=false", &
                                    expected="message='FAILED: a unit test' alloced=true") &
               , new_test_parameter(arguments="test_name='a unit test' condition=true quiet=true", &
                                    expected="message='' alloced=false") &
               , new_test_parameter(arguments="test_name='a unit test' condition=false quiet=true", &
                                    expected="message='' alloced=false") &
               , new_test_parameter(arguments="test_name='a unit test' condition=true stat=false quiet=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true alloced=true") &
               , new_test_parameter(arguments="test_name='a unit test' condition=false stat=true quiet=false", &
                                    expected="message='FAILED: a unit test' stat_exp=false alloced=true") &
               , new_test_parameter(arguments="test_name='a unit test' condition=true stat=false quiet=true", &
                                    expected="message='' stat_exp=true alloced=false") &
               , new_test_parameter(arguments="test_name='a unit test' condition=false stat=true quiet=true", &
                                    expected="message='' stat_exp=false alloced=false") &
               ], &
               optional_args=[argument("stat"), argument("quiet")] &
               )

        call runner(error, spec, run_test_cases)
    end subroutine checkTrueMsg_parameterized_test

    subroutine run_test_cases(spec, results)
        type(parameterization_spec_type), intent(in) :: spec
        type(test_results_type), intent(inout) :: results

        ! expected
        character(256) :: test_name
        logical :: condition, stat, quiet

        ! actual
        character(256) :: message
        logical :: stat_exp, alloced

        block
            character(:), allocatable :: case_name, message_act
            integer(int32) :: case, scratch_unit_number
            type(arguments_presence_type) :: arg_pres
            type(test_parameter_type) :: param
            logical :: cond, alloced_act

            do case = 1, results%get_number_of_test_cases()
                call setup_case(spec, case, param, case_name, arg_pres, &
                                test_name, condition, stat, quiet, &
                                message, stat_exp, alloced)

                write (output_unit, '(12X, "- ",A)') case_name

                if (arg_pres.has. [.false., .false.]) &
                    call check_true(condition, trim(test_name), output_message=message_act)
                if (arg_pres.has. [.true., .false.]) &
                    call check_true(condition, trim(test_name), stat=stat, output_message=message_act)
                if (arg_pres.has. [.false., .true.]) &
                    call check_true(condition, trim(test_name), quiet=quiet, output_message=message_act)
                if (arg_pres.has. [.true., .true.]) &
                    call check_true(condition, trim(test_name), stat=stat, quiet=quiet, output_message=message_act)

                alloced_act = allocated(message_act)
                cond = alloced_act .eqv. alloced
                if (.not. alloced_act) message_act = ""
                cond = all([cond, &
                            len_trim(message_act) == len_trim(message), &
                            trim(message_act) == trim(message)])

                if (param%presented("stat")) then
                    cond = cond .and. (stat .eqv. stat_exp)
                    call results%check_test(case, cond, &
                                            failure_message_msg(case_name, trim(message), trim(message_act), &
                                                                [alloced, alloced_act], [stat_exp, stat]))
                else
                    call results%check_test(case, cond, &
                                            failure_message_msg(case_name, trim(message), trim(message_act), &
                                                                [alloced, alloced_act]))
                end if

                call teardown_case(message_act)
            end do
        end block
    end subroutine run_test_cases
end module test_common_check_unitTests_trueMsg
