module test_equal_expectEqual_unitTests_expect_real
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: strings_enclose
    use :: fassert_common_unit
    use :: fassert_common_message_outputOnFailure_format, only:real32_specifier, real64_specifier
    use :: test_equal_expectEqual_unitTests_common
    use :: expectEqual
    implicit none
    private
    public :: expectEqual_real_parameterized_test

contains
    subroutine expectEqual_real_parameterized_test(error)
        use :: par_funnel
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
               !v success cases
               !-v expect_equal(actual,expected,name,stat)
               new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='10.0' "// &
                                  "test_name='a unit test'", &
                                  expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=F)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=T)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=true", &
                                    expected="message='"// &
                                    "PASSED: a unit test"//new_line_char// &
                                    "    Expected:  0.100000E+02"//new_line_char// &
                                    "    Actual  :  0.100000E+02"//new_line_char// &
                                    "    Difference:  0.000000E+00' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=F)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='"// &
                                    "PASSED: a unit test"//new_line_char// &
                                    "    Expected:  0.100000E+02"//new_line_char// &
                                    "    Actual  :  0.100000E+02"//new_line_char// &
                                    "    Difference:  0.000000E+00' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=T)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='"// &
                                    "    Expected:  0.100000E+02"//new_line_char// &
                                    "    Actual  :  0.100000E+02"//new_line_char// &
                                    "    Difference:  0.000000E+00' stat_exp=true") &
               !v failed case
               !-v expect_equal(actual,expected,name,stat)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test'", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: -0.100000E+02"//new_line_char// &
                                    "    Actual  :  0.100000E+02"//new_line_char// &
                                    "    Difference: -0.200000E+02' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test' verbose=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: -0.100000E+02"//new_line_char// &
                                    "    Actual  :  0.100000E+02"//new_line_char// &
                                    "    Difference: -0.200000E+02' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=F)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: -0.100000E+02"//new_line_char// &
                                    "    Actual  :  0.100000E+02"//new_line_char// &
                                    "    Difference: -0.200000E+02' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=T)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=F)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: -0.100000E+02"//new_line_char// &
                                    "    Actual  :  0.100000E+02"//new_line_char// &
                                    "    Difference: -0.200000E+02' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=T)
               , new_test_parameter(arguments="vtype='real32' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='"// &
                                    "    Expected: -0.100000E+02"//new_line_char// &
                                    "    Actual  :  0.100000E+02"//new_line_char// &
                                    "    Difference: -0.200000E+02' stat_exp=false") &
               !v success cases
               !-v expect_equal(actual,expected,name,stat)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test'", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=F)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=T)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=true", &
                                    expected="message='"// &
                                    "PASSED: a unit test"//new_line_char// &
                                    "    Expected:  0.100000000000000E+002"//new_line_char// &
                                    "    Actual  :  0.100000000000000E+002"//new_line_char// &
                                    "    Difference:  0.000000000000000E+000' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=F)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='"// &
                                    "PASSED: a unit test"//new_line_char// &
                                    "    Expected:  0.100000000000000E+002"//new_line_char// &
                                    "    Actual  :  0.100000000000000E+002"//new_line_char// &
                                    "    Difference:  0.000000000000000E+000' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=T)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='10.0' "// &
                                    "test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='"// &
                                    "    Expected:  0.100000000000000E+002"//new_line_char// &
                                    "    Actual  :  0.100000000000000E+002"//new_line_char// &
                                    "    Difference:  0.000000000000000E+000' stat_exp=true") &
               !v failed case
               !-v expect_equal(actual,expected,name,stat)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test'", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: -0.100000000000000E+002"//new_line_char// &
                                    "    Actual  :  0.100000000000000E+002"//new_line_char// &
                                    "    Difference: -0.200000000000000E+002' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test' verbose=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: -0.100000000000000E+002"//new_line_char// &
                                    "    Actual  :  0.100000000000000E+002"//new_line_char// &
                                    "    Difference: -0.200000000000000E+002' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=F)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: -0.100000000000000E+002"//new_line_char// &
                                    "    Actual  :  0.100000000000000E+002"//new_line_char// &
                                    "    Difference: -0.200000000000000E+002' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=T)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=F)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Expected: -0.100000000000000E+002"//new_line_char// &
                                    "    Actual  :  0.100000000000000E+002"//new_line_char// &
                                    "    Difference: -0.200000000000000E+002' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=T)
               , new_test_parameter(arguments="vtype='real64' val_act='10.0' val_exp='-10.0' "// &
                                    "test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='"// &
                                    "    Expected: -0.100000000000000E+002"//new_line_char// &
                                    "    Actual  :  0.100000000000000E+002"//new_line_char// &
                                    "    Difference: -0.200000000000000E+002' stat_exp=false") &
               ], &
               optional_args=[argument("verbose"), argument("quiet")], &
               replace_new_line=.true.)

        call runner(error, spec, run_test_cases)
    contains
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            ! arguments
            character(64) :: vtype, val_act, val_exp
            character(256) :: test_name
            logical :: stat, verbose, quiet

            ! expected
            character(256) :: message
            logical :: stat_exp

            block
                character(:), allocatable :: case_name, buffer
                integer(int32) :: case, scratch_unit_number
                type(arguments_presence_type) :: arg_pres
                character(:), allocatable :: msg_exp, trim_val_act, trim_val_exp

                logical :: cond

                do case = 1, results%get_number_of_test_cases()
                    call setup_case(spec, case, scratch_unit_number, case_name, arg_pres, &
                                    vtype, val_act, val_exp, test_name, stat, verbose, quiet, &
                                    message, stat_exp)

                    write (output_unit, '(12X, "- ",A)') case_name

                    select case (trim(adjustl(vtype)))
                    case ("real32")
                        block
                            real(real32) :: act, exp
                            trim_val_act = trim(adjustl(val_act))
                            trim_val_exp = trim(adjustl(val_exp))
                            read (trim_val_act, '('//real32_specifier//')') act
                            read (trim_val_exp, '('//real32_specifier//')') exp

                            if (arg_pres.has. [.false., .false.]) &
                                call expect_equal(act, exp, trim(test_name), stat)
                            if (arg_pres.has. [.true., .false.]) &
                                call expect_equal(act, exp, trim(test_name), stat, verbose=verbose)
                            if (arg_pres.has. [.false., .true.]) &
                                call expect_equal(act, exp, trim(test_name), stat, quiet=quiet)
                            if (arg_pres.has. [.true., .true.]) &
                                call expect_equal(act, exp, trim(test_name), stat, verbose=verbose, quiet=quiet)
                        end block
                    case ("real64")
                        block
                            real(real64) :: act, exp
                            read (val_act, '('//real64_specifier//')') act
                            read (val_exp, '('//real64_specifier//')') exp

                            if (arg_pres.has. [.false., .false.]) &
                                call expect_equal(act, exp, trim(test_name), stat)
                            if (arg_pres.has. [.true., .false.]) &
                                call expect_equal(act, exp, trim(test_name), stat, verbose=verbose)
                            if (arg_pres.has. [.false., .true.]) &
                                call expect_equal(act, exp, trim(test_name), stat, quiet=quiet)
                            if (arg_pres.has. [.true., .true.]) &
                                call expect_equal(act, exp, trim(test_name), stat, verbose=verbose, quiet=quiet)
                        end block
                    case default
                        call results%check_test(case, .false., "unsupported type")
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

                    call teardown_case(scratch_unit_number, buffer)
                end do
            end block
        end subroutine run_test_cases
    end subroutine expectEqual_real_parameterized_test
end module test_equal_expectEqual_unitTests_expect_real
