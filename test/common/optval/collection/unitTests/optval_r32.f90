module test_common_optval_unitTests_r32
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: par_funnel
    use :: test_common_optval_unitTests_common
    use :: fassert_common_optval
    implicit none
    private
    public :: optvalReal32_parameterized_test

contains
    subroutine optvalReal32_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
               new_test_parameter(arguments="x=0.1378 default=15689", expected="retval=0.1378") &
               , new_test_parameter(arguments="x=563.5 default=48.648", expected="retval=563.5") &
               , new_test_parameter(arguments="default=978.187", expected="retval=978.187") &
               , new_test_parameter(arguments="default=0.1489", expected="retval=0.1489") &
               ], &
               optional_args=[argument("x")])

        call runner(error, spec, run_test_cases)
    contains
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            real(real32) :: x, default, retval

            namelist /arguments/ x, default
            namelist /expected/ retval

            block
                character(:), allocatable :: test_name
                integer(int32) :: case
                real(real32) :: actual
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

                    call results%check_test(case, (actual == retval), &
                                            failure_message(retval, actual, test_name))
                end do
            end block
        end subroutine run_test_cases
        function failure_message(expected, actual, test_name) result(msg)
            implicit none
            real(real32), intent(in) :: expected
            real(real32), intent(in) :: actual
            character(*), intent(in) :: test_name
            character(:), allocatable :: msg

            msg = test_name//new_line(" ")// &
                  "    expected : "//to_string(expected)//new_line(" ")// &
                  "    actual   : "//to_string(actual)
        end function failure_message
    end subroutine optvalReal32_parameterized_test
end module test_common_optval_unitTests_r32
