module test_common_message_unitTests_output
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: test_common_message_unitTests_common
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

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
                 new_test_parameter(arguments="quiet=false", expected="retval=true") &
               , new_test_parameter(arguments="quiet=true" , expected="retval=false") &
               , new_test_parameter(arguments=""           , expected="retval=true") &
               ], &
               optional_args=[argument("quiet")]) !&

        call runner(error, spec, run_test_cases)
    contains
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            logical :: quiet, retval

            namelist /arguments/ quiet
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

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
                 new_test_parameter(arguments="quiet=false", expected="retval=false") &
               , new_test_parameter(arguments="quiet=true" , expected="retval=true") &
               , new_test_parameter(arguments=""           , expected="retval=false") &
               ], &
               optional_args=[argument("quiet")]) !&

        call runner(error, spec, run_test_cases)
    contains
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            logical :: quiet, retval

            namelist /arguments/ quiet
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

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
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
               ], &
               optional_args=[argument("verbose"), argument("quiet")]) !&

        call runner(error, spec, run_test_cases)
    contains
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            logical :: stat, verbose, quiet, retval

            namelist /arguments/ stat, verbose, quiet
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
