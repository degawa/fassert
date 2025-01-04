module test_common_compare_equal_char_unitTests_areEqual_D2
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: par_funnel
    use :: test_common_compare_equal_char_unitTests_common
    use :: fassert_common_compare_equal_char
    implicit none
    private
    public :: areEqual_D2_parameterized_test

contains
    subroutine areEqual_D2_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        ! | means the end of the string
        spec = new_parameterization_spec( &
            [ &
            !- success case
            !-- case sensitive (, no check length)
              new_test_parameter(arguments="val1='abc  |','xyz  |' val2='abc|','xyz|' ignore_case=false", &
                                 expected="equals=true") &
            !-- case sensitive, no check length
            , new_test_parameter(arguments="val1='abc  |','lmn  |' val2='abc|','lmn|' ignore_case=false check_len=false", &
                                 expected="equals=true") &
            !-- case sensitive, check length
            , new_test_parameter(arguments="val1='abc|','lmn|' val2='abc|','lmn|' ignore_case=false check_len=true", &
                                 expected="equals=true") &
            !-- case insensitive (, no check length)
            , new_test_parameter(arguments="val1='abc  |','XYZ  |' val2='ABC|','xyz|' ignore_case=true", &
                                 expected="equals=true") &
            !-- case insensitive, no check length
            , new_test_parameter(arguments="val1='ABC  |','lmn  |' val2='abc|','LMN|' ignore_case=true check_len=false", &
                                 expected="equals=true") &
            !-- case insensitive, check length
            , new_test_parameter(arguments="val1='AbC|','lMn|' val2='aBc|','LMN|' ignore_case=true check_len=true", &
                                 expected="equals=true") &
            !- failure case
            !-- case sensitive (, no check length)
            , new_test_parameter(arguments="val1='abc|','XYZ|' val2='ABC  |','xyz  |' ignore_case=false", &
                                 expected="equals=false") &
            !-- case sensitive, no check length
            , new_test_parameter(arguments="val1='ABC  |','LMN  |' val2='abc|','lmn|' ignore_case=false check_len=false", &
                                 expected="equals=false") &
            !-- case sensitive, check length
            , new_test_parameter(arguments="val1='abc |','lmn |'  val2='abc|','lmn|' ignore_case=false check_len=true", &
                                 expected="equals=false") &
            !-- case insensitive (, no check length)
            , new_test_parameter(arguments="val1='abc |','XYZ |'   val2='XYZ|','abc|' ignore_case=true", &
                                 expected="equals=false") &
            !-- case insensitive, no check length
            , new_test_parameter(arguments="val1='xyz|','ABC  |' val2='abc|','LMN|' ignore_case=true check_len=false", &
                                 expected="equals=false") &
            !-- case insensitive, check length
            , new_test_parameter(arguments="val1='AbC |','lMn |' val2='aBc|','LMN|' ignore_case=true check_len=true", &
                                 expected="equals=false") &
            ], &
            optional_args=[argument("check_len")]) !&

        call runner(error, spec, run_test_cases)
    end subroutine areEqual_D2_parameterized_test

    subroutine run_test_cases(spec, results)
        type(parameterization_spec_type), intent(in) :: spec
        type(test_results_type), intent(inout) :: results

        character(16) :: val1(2, 1), val2(2, 1)
        logical :: ignore_case, check_len, equals
        type(arguments_presence_type) :: arg_pres

        namelist /arguments/ val1, val2, ignore_case, check_len
        namelist /expected/ equals

        block
            character(:), allocatable :: test_name
            type(test_parameter_type) :: param
            integer(int32) :: case
            character(:), allocatable :: lhs(:, :), rhs(:, :)
            integer(int32) :: i
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

                allocate (character(len(extract(val1(1, 1)))) :: lhs(size(val1, dim=1), &
                                                                     size(val1, dim=2)))
                allocate (character(len(extract(val2(1, 1)))) :: rhs(size(val2, dim=1), &
                                                                     size(val2, dim=2)))
                do i = 1, size(val1, dim=1)
                    lhs(i, 1) = extract(val1(i, 1))
                    rhs(i, 1) = extract(val2(i, 1))
                end do

                ! evaluate
                !&<
                if (arg_pres .has. [.false.]) &
                    equals_act = are_equal(lhs, rhs, ignore_case)
                if (arg_pres .has. [.true.]) &
                    equals_act = are_equal(lhs, rhs, ignore_case, check_len)
                !&>

                cond = (equals .eqv. equals_act)
                call results%check_test(case, cond, &
                                        failure_message(test_name, equals, equals_act))

                ! teardown
                deallocate (lhs)
                deallocate (rhs)
            end do
        end block
    end subroutine run_test_cases
end module test_common_compare_equal_char_unitTests_areEqual_D2
