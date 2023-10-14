module test_common_compare_equal_char_unitTests_isEqual
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: par_funnel
    use :: test_common_compare_equal_char_unitTests_common
    use :: fassert_common_compare_equal_char
    implicit none
    private
    public :: isEqual_parameterized_test

contains
    subroutine isEqual_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        ! | means the end of the string
        spec = new_parameterization_spec( &
            [ &
            !- success case
            !-- case sensitive (, no check length)
              new_test_parameter(arguments="val1='abc|'   val2='abc|' ignore_case=false", expected="equals=true") &
            , new_test_parameter(arguments="val1='xyz  |' val2='xyz|' ignore_case=false", expected="equals=true") &
            !-- case sensitive, no check length
            , new_test_parameter(arguments="val1='abc|'   val2='abc|' ignore_case=false check_len=false", expected="equals=true") &
            , new_test_parameter(arguments="val1='lmn  |' val2='lmn|' ignore_case=false check_len=false", expected="equals=true") &
            !-- case sensitive, check length
            , new_test_parameter(arguments="val1='abc|' val2='abc|' ignore_case=false check_len=true", expected="equals=true") &
            , new_test_parameter(arguments="val1='lmn|' val2='lmn|' ignore_case=false check_len=true", expected="equals=true") &
            !-- case insensitive (, no check length)
            , new_test_parameter(arguments="val1='abc|'   val2='ABC|' ignore_case=true", expected="equals=true") &
            , new_test_parameter(arguments="val1='XYZ  |' val2='xyz|' ignore_case=true", expected="equals=true") &
            !-- case insensitive, no check length
            , new_test_parameter(arguments="val1='ABC|'   val2='abc|' ignore_case=true check_len=false", expected="equals=true") &
            , new_test_parameter(arguments="val1='lmn  |' val2='LMN|' ignore_case=true check_len=false", expected="equals=true") &
            !-- case insensitive, check length
            , new_test_parameter(arguments="val1='AbC|' val2='aBc|' ignore_case=true check_len=true", expected="equals=true") &
            , new_test_parameter(arguments="val1='lMn|' val2='LMN|' ignore_case=true check_len=true", expected="equals=true") &
            !- failure case
            !-- case sensitive (, no check length)
            , new_test_parameter(arguments="val1='abc|'   val2='ABC|' ignore_case=false", expected="equals=false") &
            , new_test_parameter(arguments="val1='XYZ  |' val2='xyz|' ignore_case=false", expected="equals=false") &
            !-- case sensitive, no check length
            , new_test_parameter(arguments="val1='ABC|'   val2='abc|' ignore_case=false check_len=false", expected="equals=false") &
            , new_test_parameter(arguments="val1='LMN  |' val2='lmn|' ignore_case=false check_len=false", expected="equals=false") &
            !-- case sensitive, check length
            , new_test_parameter(arguments="val1='abc|' val2='abc |' ignore_case=false check_len=true", expected="equals=false") &
            , new_test_parameter(arguments="val1='lmn |' val2='lmn|' ignore_case=false check_len=true", expected="equals=false") &
            !-- case insensitive (, no check length)
            , new_test_parameter(arguments="val1='abc|'   val2='XYZ|' ignore_case=true", expected="equals=false") &
            , new_test_parameter(arguments="val1='XYZ  |' val2='abc|' ignore_case=true", expected="equals=false") &
            !-- case insensitive, no check length
            , new_test_parameter(arguments="val1='xyz|'   val2='abc|' ignore_case=true check_len=false", expected="equals=false") &
            , new_test_parameter(arguments="val1='ABC  |' val2='LMN|' ignore_case=true check_len=false", expected="equals=false") &
            !-- case insensitive, check length
            , new_test_parameter(arguments="val1='AbC |' val2='aBc|' ignore_case=true check_len=true", expected="equals=false") &
            , new_test_parameter(arguments="val1='lMn|' val2='LMN |' ignore_case=true check_len=true", expected="equals=false") &
            ], &
            optional_args=[argument("check_len")]) !&

        call runner(error, spec, run_test_cases)
    end subroutine isEqual_parameterized_test

    subroutine run_test_cases(spec, results)
        type(parameterization_spec_type), intent(in) :: spec
        type(test_results_type), intent(inout) :: results

        character(16) :: val1, val2
        logical :: ignore_case, check_len, equals
        type(arguments_presence_type) :: arg_pres

        namelist /arguments/ val1, val2, ignore_case, check_len
        namelist /expected/ equals

        block
            character(:), allocatable :: test_name
            type(test_parameter_type) :: param
            integer(int32) :: case
            character(:), allocatable :: lhs, rhs
            logical :: cond, equals_act

            do case = 1, results%get_number_of_test_cases()
                ! setup
                param = spec%get_test_parameter_in(case)
                read (unit=param%arguments_namelist, nml=arguments)
                read (unit=param%expected_namelist, nml=expected)
                arg_pres = spec%get_optional_arguments_presence_in(case)

                test_name = "it should return "//param%expected()// &
                            " when input "//param%arguments()
                write (output_unit, '(12X, "- ",A)') test_name

                lhs = extract(val1)
                rhs = extract(val2)

                ! evaluate
                !&<
                if (arg_pres .has. [.false.]) &
                    equals_act = is_equal(lhs, rhs, ignore_case)
                if (arg_pres .has. [.true.]) &
                    equals_act = is_equal(lhs, rhs, ignore_case, check_len)
                !&>

                cond = (equals .eqv. equals_act)
                call results%check_test(case, cond, &
                                        failure_message(test_name, equals, equals_act))
            end do
        end block
    end subroutine run_test_cases
end module test_common_compare_equal_char_unitTests_isEqual
