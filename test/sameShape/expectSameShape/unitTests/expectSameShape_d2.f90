module test_sameShape_expectSameShape_unitTests_expect_D2
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: strings_enclose
    use :: fassert_common_unit
    use :: par_funnel
    use :: test_sameShape_expectSameShape_unitTests_expect_common
    use :: expectSameShape
    implicit none
    private
    ! D: dimension
    public :: expectSameShepeD2_parameterized_test

contains
    subroutine expectSameShepeD2_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        integer(int32) :: i
        type(argument_type), allocatable :: vtype_list(:)
        vtype_list = [argument("int32") &
                      , argument("real32") &
                      , argument("real64") &
                      ]

        spec = new_parameterization_spec( &
               [( &
                !v success case
                !-v expect_same_shape(actual,expected,name,stat)
                new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=3,2 "// &
                                   "test_name='a unit test'", &
                                   expected="message='PASSED: a unit test' stat_exp=true") &
                !-v expect_same_shape(actual,expected,name,stat,verbose=F)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=3,2 "// &
                                     "test_name='a unit test' verbose=false", &
                                     expected="message='PASSED: a unit test' stat_exp=true") &
                !-v expect_same_shape(actual,expected,name,stat,verbose=F, quiet=F)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=3,2 "// &
                                     "test_name='a unit test' verbose=false quiet=false", &
                                     expected="message='PASSED: a unit test' stat_exp=true") &
                !-v expect_same_shape(actual,expected,name,stat,verbose=F, quiet=T)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=3,2 "// &
                                     "test_name='a unit test' verbose=false quiet=true", &
                                     expected="message='' stat_exp=true") &
                !-v expect_same_shape(actual,expected,name,stat,verbose=T)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=3,2 "// &
                                     "test_name='a unit test' verbose=true", &
                                     expected="message='"// &
                                     "PASSED: a unit test"//new_line_char// &
                                     "    Expected Shape: (3,2)"//new_line_char// &
                                     "    Actual Shape  : (3,2)' stat_exp=true") &
                !-v expect_same_shape(actual,expected,name,stat,verbose=T, quiet=F)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=3,2 "// &
                                     "test_name='a unit test' verbose=true quiet=false", &
                                     expected="message='"// &
                                     "PASSED: a unit test"//new_line_char// &
                                     "    Expected Shape: (3,2)"//new_line_char// &
                                     "    Actual Shape  : (3,2)' stat_exp=true") &
                !-v expect_same_shape(actual,expected,name,stat,verbose=T, quiet=T)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=3,2 "// &
                                     "test_name='a unit test' verbose=true quiet=true", &
                                     expected="message='"// &
                                     "    Expected Shape: (3,2)"//new_line_char// &
                                     "    Actual Shape  : (3,2)' stat_exp=true") &
                !v failed case
                !-v expect_same_shape(actual,expected,name,stat)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=2,3 "// &
                                     "test_name='a unit test'", &
                                     expected="message='"// &
                                     "FAILED: a unit test"//new_line_char// &
                                     "    Expected Shape: (2,3)"//new_line_char// &
                                     "    Actual Shape  : (3,2)' stat_exp=false") &
                !-v expect_same_shape(actual,expected,name,stat,verbose=F)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=2,3 "// &
                                     "test_name='a unit test' verbose=false", &
                                     expected="message='"// &
                                     "FAILED: a unit test"//new_line_char// &
                                     "    Expected Shape: (2,3)"//new_line_char// &
                                     "    Actual Shape  : (3,2)' stat_exp=false") &
                !-v expect_same_shape(actual,expected,name,stat,verbose=F, quiet=F)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=2,3 "// &
                                     "test_name='a unit test' verbose=false quiet=false", &
                                     expected="message='"// &
                                     "FAILED: a unit test"//new_line_char// &
                                     "    Expected Shape: (2,3)"//new_line_char// &
                                     "    Actual Shape  : (3,2)' stat_exp=false") &
                !-v expect_same_shape(actual,expected,name,stat,verbose=F, quiet=T)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=2,3 "// &
                                     "test_name='a unit test' verbose=false quiet=true", &
                                     expected="message='' stat_exp=false") &
                !-v expect_same_shape(actual,expected,name,stat,verbose=T, quiet=F)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=2,3 "// &
                                     "test_name='a unit test' verbose=true quiet=false", &
                                     expected="message='"// &
                                     "FAILED: a unit test"//new_line_char// &
                                     "    Expected Shape: (2,3)"//new_line_char// &
                                     "    Actual Shape  : (3,2)' stat_exp=false") &
                !-v expect_same_shape(actual,expected,name,stat,verbose=T, quiet=T)
                , new_test_parameter(arguments="vtype='"//vtype_list(i)%name//"' dim_act=3,2 dim_exp=2,3 "// &
                                     "test_name='a unit test' verbose=true quiet=true", &
                                     expected="message='"// &
                                     "    Expected Shape: (2,3)"//new_line_char// &
                                     "    Actual Shape  : (3,2)' stat_exp=false") &
                , i=1, size(vtype_list)) &
                ], &
               optional_args=[argument("verbose"), argument("quiet")], &
               replace_new_line=.true.)

        call runner(error, spec, run_test_cases)
    contains
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            ! arguments
            character(64) :: vtype
            integer(int32) :: dim_act(2), dim_exp(2)
            character(256) :: test_name
            logical :: stat, verbose, quiet

            ! expected
            character(256) :: message
            logical :: stat_exp

            block
                character(:), allocatable :: case_name, buffer
                integer(int32) :: case, scratch_unit_number
                type(test_parameter_type) :: param
                type(arguments_presence_type) :: arg_pres
                character(:), allocatable :: msg_exp

                class(*), allocatable :: act(:, :), exp(:, :)
                logical :: cond

                do case = 1, results%get_number_of_test_cases()
                    call setup_case(spec, case, param, 2, scratch_unit_number, case_name, arg_pres, &
                                    vtype, dim_act, dim_exp, test_name, stat, verbose, quiet, &
                                    message, stat_exp)

                    write (output_unit, '(12X, "- ",A)') case_name

                    select case (trim(adjustl(vtype)))
                    case ("int8")
                    case ("int16")
                    case ("int32")
                        block
                            allocate (integer(int32) :: act(dim_act(1), dim_act(2)))
                            allocate (integer(int32) :: exp(dim_exp(1), dim_exp(2)))

                            if (arg_pres.has. [.false., .false.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat)
                            if (arg_pres.has. [.true., .false.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat, verbose=verbose)
                            if (arg_pres.has. [.false., .true.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat, quiet=quiet)
                            if (arg_pres.has. [.true., .true.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat, verbose=verbose, quiet=quiet)
                        end block
                    case ("int64")
                    case ("real32")
                        block
                            allocate (real(real32) :: act(dim_act(1), dim_act(2)))
                            allocate (real(real32) :: exp(dim_exp(1), dim_exp(2)))

                            if (arg_pres.has. [.false., .false.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat)
                            if (arg_pres.has. [.true., .false.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat, verbose=verbose)
                            if (arg_pres.has. [.false., .true.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat, quiet=quiet)
                            if (arg_pres.has. [.true., .true.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat, verbose=verbose, quiet=quiet)
                        end block
                    case ("real64")
                        block
                            allocate (real(real64) :: act(dim_act(1), dim_act(2)))
                            allocate (real(real64) :: exp(dim_exp(1), dim_exp(2)))

                            if (arg_pres.has. [.false., .false.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat)
                            if (arg_pres.has. [.true., .false.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat, verbose=verbose)
                            if (arg_pres.has. [.false., .true.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat, quiet=quiet)
                            if (arg_pres.has. [.true., .true.]) &
                                call expect_same_shape(act, exp, trim(test_name), stat, verbose=verbose, quiet=quiet)
                        end block
                    case default
                        cycle
                    end select

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

                    call teardown_case(scratch_unit_number, buffer, act, exp)
                end do
            end block
        end subroutine run_test_cases
    end subroutine expectSameShepeD2_parameterized_test
end module test_sameShape_expectSameShape_unitTests_expect_D2
