module test_common_optval_unitTests_l
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string ! use to_string declared in testdrive_util.
    use :: fassert_common_optval
    implicit none
    private
    public :: optvalLogical_parameterized_test

contains
    subroutine optvalLogical_parameterized_test(error)
        use :: par_funnel
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                   new_test_parameter(arguments="x=true default=true"   , expected="retval=true") &
                 , new_test_parameter(arguments="x=true default=false"  , expected="retval=true") &
                 , new_test_parameter(arguments="x=false default=true"  , expected="retval=false") &
                 , new_test_parameter(arguments="x=false default=false" , expected="retval=false") &
                 , new_test_parameter(arguments="default=true"          , expected="retval=true") &
                 , new_test_parameter(arguments="default=false"         , expected="retval=false") &
                 ] !&

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            logical :: x, default, retval

            namelist /arguments/ x, default
            namelist /expected/ retval

            call results%construct(params)

            block
                character(:), allocatable :: test_name
                integer(int32) :: case
                logical :: actual
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
