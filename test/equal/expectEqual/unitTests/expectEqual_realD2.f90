module test_equal_expectEqual_unitTests_expect_realD2
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: strings_enclose
    use :: par_funnel
    use :: fassert_common_unit
    use :: test_equal_expectEqual_unitTests_common
    use :: expectEqual
    implicit none
    private
    public :: expectEqual_realD2_parameterized_test

contains
    subroutine expectEqual_realD2_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
               !v success cases
               !-v expect_equal(actual,expected,name,stat)
               new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                  "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                  "test_name='a unit test'", &
                                  expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=F)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=T)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=true", &
                                    expected="message='"// &
                                    "PASSED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.000000E+00 at (1,1)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000E+00 at (1,1)' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=F)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='"// &
                                    "PASSED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.000000E+00 at (1,1)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000E+00 at (1,1)' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=T)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='"// &
                                    "    Maximum Absolute Difference:  0.000000E+00 at (1,1)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000E+00 at (1,1)' stat_exp=true") &
               !v failed case
               !-v expect_equal(actual,expected,name,stat)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,-4.0,5.0,6.0' "// &
                                    "test_name='a unit test'", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.800000E+01 at (2,2)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000E+00 at (1,1)' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,-4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.800000E+01 at (2,2)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000E+00 at (1,1)' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=F)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,-4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.800000E+01 at (2,2)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000E+00 at (1,1)' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=T)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,-4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=F)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,-4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.800000E+01 at (2,2)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000E+00 at (1,1)' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=T)
               , new_test_parameter(arguments="vtype='real32' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,-4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='"// &
                                    "    Maximum Absolute Difference:  0.800000E+01 at (2,2)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000E+00 at (1,1)' stat_exp=false") &
               !v success cases
               !-v expect_equal(actual,expected,name,stat)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test'", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=F)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='PASSED: a unit test' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=T)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=true", &
                                    expected="message='"// &
                                    "PASSED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.000000000000000E+000 at (1,1)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000000000000E+000 at (1,1)' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=F)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='"// &
                                    "PASSED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.000000000000000E+000 at (1,1)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000000000000E+000 at (1,1)' stat_exp=true") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=T)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='"// &
                                    "    Maximum Absolute Difference:  0.000000000000000E+000 at (1,1)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000000000000E+000 at (1,1)' stat_exp=true") &
               !v failed case
               !-v expect_equal(actual,expected,name,stat)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='-1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test'", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.200000000000000E+001 at (1,1)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000000000000E+000 at (2,1)' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='-1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.200000000000000E+001 at (1,1)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000000000000E+000 at (2,1)' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=F)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='-1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false quiet=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.200000000000000E+001 at (1,1)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000000000000E+000 at (2,1)' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=F, quiet=T)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='-1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=false quiet=true", &
                                    expected="message='' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=F)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='-1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=true quiet=false", &
                                    expected="message='"// &
                                    "FAILED: a unit test"//new_line_char// &
                                    "    Maximum Absolute Difference:  0.200000000000000E+001 at (1,1)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000000000000E+000 at (2,1)' stat_exp=false") &
               !-v expect_equal(actual,expected,name,stat,verbose=T, quiet=T)
               , new_test_parameter(arguments="vtype='real64' size=2,3 "// &
                                    "val_act='1.0,2.0,3.0,4.0,5.0,6.0' val_exp='-1.0,2.0,3.0,4.0,5.0,6.0' "// &
                                    "test_name='a unit test' verbose=true quiet=true", &
                                    expected="message='"// &
                                    "    Maximum Absolute Difference:  0.200000000000000E+001 at (1,1)"//new_line_char// &
                                    "    Minimum Absolute Difference:  0.000000000000000E+000 at (2,1)' stat_exp=false") &
               ], &
               optional_args=[argument("verbose"), argument("quiet")], &
               replace_new_line=.true.)

        call runner(error, spec, run_test_cases)
    contains
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            integer(int32), parameter :: dim = 2

            ! arguments
            character(64) :: vtype
            character(256) :: test_name, val_act, val_exp
            integer(int32) :: size(dim)
            logical :: stat, verbose, quiet

            ! expected
            character(256) :: message
            logical :: stat_exp

            block
                character(:), allocatable :: case_name, buffer
                integer(int32) :: case, scratch_unit_number
                type(arguments_presence_type) :: arg_pres
                character(:), allocatable :: msg_exp

                logical :: cond

                do case = 1, results%get_number_of_test_cases()
                    call setup_case(spec, case, dim, scratch_unit_number, case_name, arg_pres, &
                                    vtype, size, val_act, val_exp, test_name, stat, verbose, quiet, &
                                    message, stat_exp)
                    write (output_unit, '(12X, "- ",A)') case_name

                    select case (trim(adjustl(vtype)))
                    case ("real32")
                        block
                            real(real32), allocatable :: act(:, :), exp(:, :)
                            allocate (act(size(1), size(2)))
                            allocate (exp(size(1), size(2)))
                            read (val_act, *) act
                            read (val_exp, *) exp

                            if (arg_pres.has. [.false., .false.]) &
                                call expect_equal(act, exp, trim(test_name), stat)
                            if (arg_pres.has. [.true., .false.]) &
                                call expect_equal(act, exp, trim(test_name), stat, verbose=verbose)
                            if (arg_pres.has. [.false., .true.]) &
                                call expect_equal(act, exp, trim(test_name), stat, quiet=quiet)
                            if (arg_pres.has. [.true., .true.]) &
                                call expect_equal(act, exp, trim(test_name), stat, verbose=verbose, quiet=quiet)

                            deallocate (act)
                            deallocate (exp)
                        end block
                    case ("real64")
                        block
                            real(real64), allocatable :: act(:, :), exp(:, :)
                            allocate (act(size(1), size(2)))
                            allocate (exp(size(1), size(2)))
                            read (val_act, *) act
                            read (val_exp, *) exp

                            if (arg_pres.has. [.false., .false.]) &
                                call expect_equal(act, exp, trim(test_name), stat)
                            if (arg_pres.has. [.true., .false.]) &
                                call expect_equal(act, exp, trim(test_name), stat, verbose=verbose)
                            if (arg_pres.has. [.false., .true.]) &
                                call expect_equal(act, exp, trim(test_name), stat, quiet=quiet)
                            if (arg_pres.has. [.true., .true.]) &
                                call expect_equal(act, exp, trim(test_name), stat, verbose=verbose, quiet=quiet)

                            deallocate (act)
                            deallocate (exp)
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
    end subroutine expectEqual_realD2_parameterized_test
end module test_equal_expectEqual_unitTests_expect_realD2
