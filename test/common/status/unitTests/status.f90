module test_common_status_unitTests
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: fassert_common_status
    implicit none
    private
    public :: isTestPassed_parameterized_test
    public :: isTestFailed_parameterized_test
    public :: isTestExpecFail_parameterized_test

contains
    subroutine isTestPassed_parameterized_test(error)
        use :: par_funnel
        use :: fassert_common_unit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                   new_test_parameter(arguments="test_status='passed'", expected="retval=true") &
                 , new_test_parameter(arguments="test_status='failed'", expected="retval=false") &
                 ] !&

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            character(6) :: test_status
            logical :: retval

            namelist /arguments/ test_status
            namelist /expected/ retval

            call results%construct(params)

            block
                character(:), allocatable :: test_name
                integer(int32) :: case
                logical :: actual

                do case = 1, results%get_number_of_test_cases()
                    read (unit=params(case)%arguments_namelist, nml=arguments)
                    read (unit=params(case)%expected_namelist, nml=expected)

                    test_name = "it should return "//params(case)%expected()// &
                                " when input "//params(case)%arguments()
                    write (output_unit, '(12X, "- ",A)') test_name

                    select case (test_status)
                    case ('passed')
                        actual = is_test_passed(passed)
                    case ('failed')
                        actual = is_test_passed(failed)
                    case default
                        cycle
                    end select
                    call results%check_test(case, (actual .eqv. retval), &
                                            failure_message(retval, actual, test_name))
                end do
            end block
        end subroutine run_test_cases
    end subroutine isTestPassed_parameterized_test

    subroutine isTestFailed_parameterized_test(error)
        use :: par_funnel
        use :: fassert_common_unit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                   new_test_parameter(arguments="test_status='passed'", expected="retval=false") &
                 , new_test_parameter(arguments="test_status='failed'", expected="retval=true") &
                 ] !&

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            character(6) :: test_status
            logical :: retval

            namelist /arguments/ test_status
            namelist /expected/ retval

            call results%construct(params)

            block
                character(:), allocatable :: test_name
                integer(int32) :: case
                logical :: actual

                do case = 1, results%get_number_of_test_cases()
                    read (unit=params(case)%arguments_namelist, nml=arguments)
                    read (unit=params(case)%expected_namelist, nml=expected)

                    test_name = "it should return "//params(case)%expected()// &
                                " when input "//params(case)%arguments()
                    write (output_unit, '(12X, "- ",A)') test_name

                    select case (test_status)
                    case ('passed')
                        actual = is_test_failed(passed)
                    case ('failed')
                        actual = is_test_failed(failed)
                    case default
                        cycle
                    end select
                    call results%check_test(case, (actual .eqv. retval), &
                                            failure_message(retval, actual, test_name))
                end do
            end block
        end subroutine run_test_cases
    end subroutine isTestFailed_parameterized_test

    subroutine isTestExpecFail_parameterized_test(error)
        use :: par_funnel
        use :: fassert_common_unit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                   new_test_parameter(arguments="expected_failure=true", expected="retval=true") &
                 , new_test_parameter(arguments="expected_failure=false", expected="retval=false") &
                 , new_test_parameter(arguments="", expected="retval=false") &
                 ] !&

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            logical :: expected_failure, retval

            namelist /arguments/ expected_failure
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

                    arg_pres = arguments_presence([params(case)%presented("expected_failure")])
                    if (arg_pres.has. [.true.]) &
                        actual = is_test_of_expected_failure(expected_failure)
                    if (arg_pres.has. [.false.]) &
                        actual = is_test_of_expected_failure()

                    call results%check_test(case, (actual .eqv. retval), &
                                            failure_message(retval, actual, test_name))
                end do
            end block
        end subroutine run_test_cases
    end subroutine isTestExpecFail_parameterized_test

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
end module test_common_status_unitTests
