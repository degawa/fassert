module test_common_optval_unitTests_l
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string ! use to_string declared in testdrive_util.
    use :: par_funnel
    use :: test_common_optval_unitTests_common
    use :: fassert_common_optval
    implicit none
    private
    public :: optvalLogical_parameterized_test

contains
    subroutine optvalLogical_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
               new_test_parameter(arguments="x=true default=true", expected="retval=true") &
               , new_test_parameter(arguments="x=true default=false", expected="retval=true") &
               , new_test_parameter(arguments="x=false default=true", expected="retval=false") &
               , new_test_parameter(arguments="x=false default=false", expected="retval=false") &
               , new_test_parameter(arguments="default=true", expected="retval=true") &
               , new_test_parameter(arguments="default=false", expected="retval=false") &
               ], &
               optional_args=[argument("x")])

        call runner(error, spec, run_test_cases)
    contains
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            logical :: x, default, retval

            namelist /arguments/ x, default
            namelist /expected/ retval

            block
                character(:), allocatable :: test_name
                integer(int32) :: case
                logical :: actual
                type(test_parameter_type) :: param
                type(arguments_presence_type) :: arg_pres

                do case = 1, results%get_number_of_test_cases()
                    param = spec%get_test_parameter_in(case)
                    read (unit=param%arguments_namelist, nml=arguments)
                    read (unit=param%expected_namelist, nml=expected)

                    test_name = "it should return "//param%expected()// &
                                " when input "//param%arguments()
                    write (output_unit, '(12X, "- ",A)') test_name

                    arg_pres = spec%get_optional_arguments_presence_in(case)
                    if (arg_pres.has. [.true.]) &
                        actual = optval(x, default)
                    if (arg_pres.has. [.false.]) &
                        actual = optval(default=default)

                    call results%check_test(case, (actual .eqv. retval), &
                                            failure_message(retval, actual, test_name))
                end do
            end block
        end subroutine run_test_cases
        function failure_message(expected, actual, test_name) result(msg)
            implicit none
            logical, intent(in) :: expected
            logical, intent(in) :: actual
            character(*), intent(in) :: test_name
            character(:), allocatable :: msg

            msg = test_name//new_line(" ")// &
                  "    expected : "//to_string(expected)//new_line(" ")// &
                  "    actual   : "//to_string(actual)
        end function failure_message
    end subroutine optvalLogical_parameterized_test
end module test_common_optval_unitTests_l
