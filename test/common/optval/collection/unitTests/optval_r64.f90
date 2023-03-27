module test_common_optval_unitTests_r64
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert_common_optval
    implicit none
    private
    public :: optvalReal64_parameterized_test

contains
    subroutine optvalReal64_parameterized_test(error)
        use :: par_funnel
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                   new_test_parameter(arguments="x=15689 default=0.1378", expected="retval=15689") &
                 , new_test_parameter(arguments="x=48.648 default=563.5", expected="retval=48.648") &
                 , new_test_parameter(arguments="default=231d3"       , expected="retval=231d3") &
                 , new_test_parameter(arguments="default=81d-3"        , expected="retval=81d-3") &
                 ] !&

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            real(real64) :: x, default, retval

            namelist /arguments/ x, default
            namelist /expected/ retval

            call results%construct(params)

            block
                character(:), allocatable :: test_name
                integer(int32) :: case
                real(real64) :: actual
                type(arguments_presence_type) :: arg_pres

                do case = 1, results%get_number_of_test_cases()
                    read (unit=params(case)%arguments_namelist, nml=arguments)
                    read (unit=params(case)%expected_namelist, nml=expected)

                    test_name = "it should return "//params(case)%expected()// &
                                " when input "//params(case)%arguments()
                    write (output_unit, '(12X, "- ",A)') test_name

                    arg_pres = arguments_presence([params(case)%presented("x")])
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
            real(real64), intent(in) :: expected
            real(real64), intent(in) :: actual
            character(*), intent(in) :: test_name
            character(:), allocatable :: msg

            msg = test_name//new_line(" ")// &
                  "    expected : "//to_string(expected)//new_line(" ")// &
                  "    actual   : "//to_string(actual)
        end function failure_message
    end subroutine optvalReal64_parameterized_test
end module test_common_optval_unitTests_r64
