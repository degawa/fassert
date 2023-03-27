module test_sameShape_expectSameShape_unitTests_expect_D1
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: strings_enclose
    use :: fassert_common_unit
    use :: test_sameShape_expectSameShape_unitTests_expect_common
    use :: expectSameShape
    implicit none
    private
    ! D: dimension
    public :: expectSameShepeD1_parameterized_test

contains
    subroutine expectSameShepeD1_parameterized_test(error)
        use :: par_funnel
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        integer(int32) :: i
        type(string_type), allocatable :: vtype_list(:)
        vtype_list = [string_type("int32") &
                      , string_type("real32") &
                      , string_type("real64") &
                      ]

        params = [( &
                  !v success case
                  !-v expect_same_shape(actual,expected,name,stat)
                  new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=3 "// &
                                     "test_name='a unit test'", &
                                     expected="message='PASSED: a unit test' stat_exp=true") &
                  !-v expect_same_shape(actual,expected,name,stat,verbose=F)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=3 "// &
                                       "test_name='a unit test' verbose=false", &
                                       expected="message='PASSED: a unit test' stat_exp=true") &
                  !-v expect_same_shape(actual,expected,name,stat,verbose=F, quiet=F)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=3 "// &
                                       "test_name='a unit test' verbose=false quiet=false", &
                                       expected="message='PASSED: a unit test' stat_exp=true") &
                  !-v expect_same_shape(actual,expected,name,stat,verbose=F, quiet=T)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=3 "// &
                                       "test_name='a unit test' verbose=false quiet=true", &
                                       expected="message='' stat_exp=true") &
                  !-v expect_same_shape(actual,expected,name,stat,verbose=T)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=3 "// &
                                       "test_name='a unit test' verbose=true", &
                                       expected="message='PASSED: a unit test\n"// &
                                       "    Expected Shape: (3)\n"// &
                                       "    Actual Shape  : (3)' stat_exp=true") &
                  !-v expect_same_shape(actual,expected,name,stat,verbose=T, quiet=F)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=3 "// &
                                       "test_name='a unit test' verbose=true quiet=false", &
                                       expected="message='PASSED: a unit test\n"// &
                                       "    Expected Shape: (3)\n"// &
                                       "    Actual Shape  : (3)' stat_exp=true") &
                  !-v expect_same_shape(actual,expected,name,stat,verbose=T, quiet=T)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=3 "// &
                                       "test_name='a unit test' verbose=true quiet=true", &
                                       expected="message='    Expected Shape: (3)\n"// &
                                       "    Actual Shape  : (3)' stat_exp=true") &
                  !v failed case
                  !-v expect_same_shape(actual,expected,name,stat)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=2 "// &
                                       "test_name='a unit test'", &
                                       expected="message='FAILED: a unit test\n"// &
                                       "    Expected Shape: (2)\n"// &
                                       "    Actual Shape  : (3)' stat_exp=false") &
                  !-v expect_same_shape(actual,expected,name,stat,verbose=F)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=2 "// &
                                       "test_name='a unit test' verbose=false", &
                                       expected="message='FAILED: a unit test\n"// &
                                       "    Expected Shape: (2)\n"// &
                                       "    Actual Shape  : (3)' stat_exp=false") &
                  !-v expect_same_shape(actual,expected,name,stat,verbose=F, quiet=F)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=2 "// &
                                       "test_name='a unit test' verbose=false quiet=false", &
                                       expected="message='FAILED: a unit test\n"// &
                                       "    Expected Shape: (2)\n"// &
                                       "    Actual Shape  : (3)' stat_exp=false") &
                  !-v expect_same_shape(actual,expected,name,stat,verbose=F, quiet=T)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=2 "// &
                                       "test_name='a unit test' verbose=false quiet=true", &
                                       expected="message='' stat_exp=false") &
                  !-v expect_same_shape(actual,expected,name,stat,verbose=T, quiet=F)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=2 "// &
                                       "test_name='a unit test' verbose=true quiet=false", &
                                       expected="message='FAILED: a unit test\n"// &
                                       "    Expected Shape: (2)\n"// &
                                       "    Actual Shape  : (3)' stat_exp=false") &
                  !-v expect_same_shape(actual,expected,name,stat,verbose=T, quiet=T)
                  , new_test_parameter(arguments="vtype='"//vtype_list(i)%val//"' dim_act=3 dim_exp=2 "// &
                                       "test_name='a unit test' verbose=true quiet=true", &
                                       expected="message='    Expected Shape: (2)\n"// &
                                       "    Actual Shape  : (3)' stat_exp=false") &
                  , i=1, size(vtype_list)) &
                  ]

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            use :: strings_enclose
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            ! arguments
            character(64) :: vtype
            integer(int32) :: dim_act(1), dim_exp(1)
            character(256) :: test_name
            logical :: stat, verbose, quiet

            ! expected
            character(256) :: message
            logical :: stat_exp

            call results%construct(params)

            block
                character(:), allocatable :: case_name, buffer
                integer(int32) :: case, scratch_unit_number
                type(arguments_presence_type) :: arg_pres
                character(:), allocatable :: msg_exp

                class(*), allocatable :: act(:), exp(:)
                logical :: cond

                do case = 1, results%get_number_of_test_cases()
                    call setup_case(params(case), scratch_unit_number, case_name, arg_pres, &
                                    vtype, dim_act, dim_exp, test_name, stat, verbose, quiet, &
                                    message, stat_exp)

                    write (output_unit, '(12X, "- ",A)') case_name

                    select case (trim(adjustl(vtype)))
                    case ("int8")
                    case ("int16")
                    case ("int32")
                        block
                            allocate (integer(int32) :: act(dim_act(1)))
                            allocate (integer(int32) :: exp(dim_exp(1)))

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
                            allocate (real(real32) :: act(dim_act(1)))
                            allocate (real(real32) :: exp(dim_exp(1)))

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
                            allocate (real(real64) :: act(dim_act(1)))
                            allocate (real(real64) :: exp(dim_exp(1)))

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

                    msg_exp = replace_all(trim(message), "\n", new_line(" "))
                    call get_all_actual_value(error, scratch_unit_number, buffer)
                    cond = all([stat .eqv. stat_exp, &
                                len(buffer) == len_trim(msg_exp), &
                                buffer == trim(msg_exp)])

                    call results%check_test(case, cond, &
                                            failure_message(case_name, trim(msg_exp), buffer, [stat_exp, stat]))

                    call teardown_case(scratch_unit_number, buffer, act, exp)
                end do
            end block
        end subroutine run_test_cases
        subroutine setup_case(param, unit_number, case_name, arg_pres, &
                              vtype, dim_act, dim_exp, test_name, stat, verbose, quiet, &
                              message, stat_exp)
            use :: newunit
            type(test_parameter_type), intent(in) :: param
            integer(int32), intent(inout) :: unit_number
            character(:), allocatable, intent(out) :: case_name
            type(arguments_presence_type), intent(inout) :: arg_pres

            ! arguments
            character(64), intent(out) :: vtype
            integer(int32), intent(out) :: dim_act(1), dim_exp(1)
            character(256), intent(out) :: test_name
            logical, intent(out) :: stat, verbose, quiet

            ! expected
            character(256), intent(out) :: message
            logical, intent(out) :: stat_exp

            namelist /arguments/ vtype, dim_act, dim_exp, test_name, stat, verbose, quiet
            namelist /expected/ message, stat_exp

            read (unit=param%arguments_namelist, nml=arguments)
            read (unit=param%expected_namelist, nml=expected)

            case_name = "it should get "//enclose(param%expected(), "{")// &
                        " when input "//enclose(param%arguments(), "{")

            arg_pres = arguments_presence([param%presented("verbose"), &
                                           param%presented("quiet")])

            unit_number = get_newunit_number()
            call set_assertion_message_unit(unit_number)
            open (unit=unit_number, status="scratch")
        end subroutine setup_case

        subroutine teardown_case(unit_number, buffer, act, exp)
            integer(int32), intent(in) :: unit_number
            character(:), allocatable, intent(inout) :: buffer
            class(*), allocatable, intent(inout) :: act(:), exp(:)
            close (unit_number)
            call set_assertion_message_unit(output_unit)
            deallocate (buffer)

            deallocate (act)
            deallocate (exp)
        end subroutine teardown_case
    end subroutine expectSameShepeD1_parameterized_test
end module test_sameShape_expectSameShape_unitTests_expect_D1
