module test_common_message_unitTests_output
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: fassert_common_message
    use :: fassert_common_unit
    implicit none
    private
    public :: doesOutputMsg_parameterized_test
    public :: doesNotOutputMsg_parameterized_test
    public :: isVerboseOutput_parameterized_test

contains
    subroutine doesOutputMsg_parameterized_test(error)
        use :: par_funnel
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                   new_test_parameter(arguments="quiet=false", expected="retval=true") &
                 , new_test_parameter(arguments="quiet=true" , expected="retval=false") &
                 , new_test_parameter(arguments=""           , expected="retval=true") &
                 ] !&

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            logical :: quiet, retval

            namelist /arguments/ quiet
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

                    arg_pres = arguments_presence([params(case)%presented("quiet")])
                    if (arg_pres.has. [.true.]) &
                        actual = does_output_message(quiet)
                    if (arg_pres.has. [.false.]) &
                        actual = does_output_message()

                    call results%check_test(case, (actual .eqv. retval), &
                                            failure_message(retval, actual, test_name))
                end do
            end block
        end subroutine run_test_cases
    end subroutine doesOutputMsg_parameterized_test

    subroutine doesNotOutputMsg_parameterized_test(error)
        use :: par_funnel
        use :: fassert_common_unit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                   new_test_parameter(arguments="quiet=false", expected="retval=false") &
                 , new_test_parameter(arguments="quiet=true" , expected="retval=true") &
                 , new_test_parameter(arguments=""           , expected="retval=false") &
                 ] !&

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            logical :: quiet, retval

            namelist /arguments/ quiet
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

                    arg_pres = arguments_presence([params(case)%presented("quiet")])
                    if (arg_pres.has. [.true.]) &
                        actual = does_not_output_message(quiet)
                    if (arg_pres.has. [.false.]) &
                        actual = does_not_output_message()

                    call results%check_test(case, (actual .eqv. retval), &
                                            failure_message(retval, actual, test_name))
                end do
            end block
        end subroutine run_test_cases
    end subroutine doesNotOutputMsg_parameterized_test

    subroutine isVerboseOutput_parameterized_test(error)
        use :: par_funnel
        use :: fassert_common_unit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                   new_test_parameter(arguments="stat=false"                            , expected="retval=true") &
                 , new_test_parameter(arguments="stat=true"                             , expected="retval=false") &
                 , new_test_parameter(arguments="stat=false verbose=true"               , expected="retval=true") &
                 , new_test_parameter(arguments="stat=false verbose=false"              , expected="retval=true") &
                 , new_test_parameter(arguments="stat=true verbose=true"                , expected="retval=true") &
                 , new_test_parameter(arguments="stat=true verbose=false"               , expected="retval=false") &
                 , new_test_parameter(arguments="stat=false quiet=true"                 , expected="retval=true") &
                 , new_test_parameter(arguments="stat=false quiet=false"                , expected="retval=true") &
                 , new_test_parameter(arguments="stat=true quiet=true"                  , expected="retval=false") &
                 , new_test_parameter(arguments="stat=true quiet=false"                 , expected="retval=false") &
                 , new_test_parameter(arguments="stat=false verbose=true quiet=true"    , expected="retval=true") &
                 , new_test_parameter(arguments="stat=false verbose=true quiet=false"   , expected="retval=true") &
                 , new_test_parameter(arguments="stat=false verbose=false quiet=true"   , expected="retval=false") &
                 , new_test_parameter(arguments="stat=false verbose=false quiet=false"  , expected="retval=true") &
                 , new_test_parameter(arguments="stat=true verbose=true quiet=true"     , expected="retval=true") &
                 , new_test_parameter(arguments="stat=true verbose=true quiet=false"    , expected="retval=true") &
                 , new_test_parameter(arguments="stat=true verbose=false quiet=true"    , expected="retval=false") &
                 , new_test_parameter(arguments="stat=true verbose=false quiet=false"   , expected="retval=false") &
                 ] !&

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            logical :: stat, verbose, quiet, retval

            namelist /arguments/ stat, verbose, quiet
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

                    arg_pres = arguments_presence([params(case)%presented("verbose"), &
                                                   params(case)%presented("quiet")])
                    if (arg_pres.has. [.false., .false.]) &
                        actual = is_verbose_output(stat)
                    if (arg_pres.has. [.true., .false.]) &
                        actual = is_verbose_output(stat, verbose=verbose)
                    if (arg_pres.has. [.false., .true.]) &
                        actual = is_verbose_output(stat, quiet=quiet)
                    if (arg_pres.has. [.true., .true.]) &
                        actual = is_verbose_output(stat, verbose=verbose, quiet=quiet)

                    call results%check_test(case, (actual .eqv. retval), &
                                            failure_message(retval, actual, test_name))
                end do
            end block
        end subroutine run_test_cases
    end subroutine isVerboseOutput_parameterized_test

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
end module test_common_message_unitTests_output
