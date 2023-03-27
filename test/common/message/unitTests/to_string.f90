module test_common_message_unitTests_toString
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: fassert_common_message
    implicit none
    private
    public :: toString_parameterized_test

contains
    subroutine toString_parameterized_test(error)
        use :: par_funnel
        use :: fassert_common_unit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                   new_test_parameter(arguments="val=false", expected="retval='F'") &
                 , new_test_parameter(arguments="val=true" , expected="retval='T'") &
                 ] !&

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            use, intrinsic :: iso_fortran_env
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            logical :: val
            character(1) :: retval

            namelist /arguments/ val
            namelist /expected/ retval

            call results%construct(params)

            block
                character(:), allocatable :: test_name
                integer(int32) :: case
                character(1) :: actual

                do case = 1, results%get_number_of_test_cases()
                    read (unit=params(case)%arguments_namelist, nml=arguments)
                    read (unit=params(case)%expected_namelist, nml=expected)

                    test_name = "it should return "//params(case)%expected()// &
                                " when input "//params(case)%arguments()
                    write (output_unit, '(12X, "- ",A)') test_name

                    actual = to_string(val)
                    call results%check_test(case, actual == retval, &
                                            failure_message(retval, actual, test_name))
                end do
            end block
        end subroutine run_test_cases
        function failure_message(expected, actual, test_name) result(msg)
            implicit none
            character(1), intent(in) :: expected
            character(1), intent(in) :: actual
            character(*), intent(in) :: test_name
            character(:), allocatable :: msg

            msg = test_name//new_line(" ")// &
                  "    expected : "//expected//new_line(" ")// &
                  "    actual   : "//actual
        end function failure_message
    end subroutine toString_parameterized_test
end module test_common_message_unitTests_toString
