module test_equal_expectEqual_unitTests_common
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:to_string
    use :: testdrive_util, only:to_string
    use :: strings_enclose
    use :: par_funnel
    use :: fassert_common_unit
    implicit none
    private
    public :: runner
    public :: setup_case
    public :: teardown_case
    public :: failure_message

    interface setup_case
        procedure :: setup_case_un
        procedure :: setup_case_un_D
    end interface

    interface teardown_case
        procedure :: teardown_case_un
    end interface
contains
    subroutine runner(error, spec, run_test_cases)
        use :: testdrive, only:error_type, check
        implicit none
        interface
            subroutine test_case_runner(spec, results)
                import parameterization_spec_type, test_results_type
                import error_type
                type(parameterization_spec_type), intent(in) :: spec
                type(test_results_type), intent(inout) :: results
            end subroutine test_case_runner
        end interface

        type(error_type), allocatable, intent(out) :: error
        type(parameterization_spec_type), intent(in) :: spec
        procedure(test_case_runner) :: run_test_cases

        type(test_results_type) :: results

        results = new_test_results_for(spec)
        call run_test_cases(spec, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    end subroutine runner

    subroutine setup_case_un(spec, case, unit_number, case_name, arg_pres, &
                             vtype, val_act, val_exp, test_name, stat, verbose, quiet, &
                             message, stat_exp)
        use :: newunit
        type(parameterization_spec_type), intent(in) :: spec
        integer(int32), intent(in) :: case
        integer(int32), intent(inout) :: unit_number
        character(:), allocatable, intent(out) :: case_name
        type(arguments_presence_type), intent(inout) :: arg_pres

        ! arguments
        character(64), intent(out) :: vtype, val_act, val_exp
        character(256), intent(out) :: test_name
        logical, intent(out) :: stat, verbose, quiet

        ! expected
        character(256), intent(out) :: message
        logical, intent(out) :: stat_exp

        namelist /arguments/ vtype, val_act, val_exp, test_name, stat, verbose, quiet
        namelist /expected/ message, stat_exp

        type(test_parameter_type) :: param

        param = spec%get_test_parameter_in(case)
        read (unit=param%arguments_namelist, nml=arguments)
        read (unit=param%expected_namelist, nml=expected)

        case_name = "it should get "//enclose(param%expected(), "{")// &
                    " when input "//enclose(param%arguments(), "{")

        arg_pres = spec%get_optional_arguments_presence_in(case)

        unit_number = get_newunit_number()
        call set_assertion_message_unit(unit_number)
        open (unit=unit_number, status="scratch")
    end subroutine setup_case_un

    subroutine teardown_case_un(unit_number, buffer)
        integer(int32), intent(in) :: unit_number
        character(:), allocatable, intent(inout) :: buffer
        close (unit_number)
        call set_assertion_message_unit(output_unit)
        deallocate (buffer)
    end subroutine teardown_case_un

    function failure_message(case_name, expected, actual, stat) result(msg)
        implicit none
        character(*), intent(in) :: case_name
        character(*), intent(in) :: expected
        character(*), intent(in) :: actual
        logical, intent(in), optional :: stat(2)
        character(:), allocatable :: msg

        character(:), allocatable :: stat_exp, stat_act

        stat_exp = ""
        stat_act = ""
        if (present(stat)) then
            stat_exp = to_string(stat(1))
            stat_act = to_string(stat(2))
        end if
        msg = case_name//new_line(" ")// &
              "    expected : "//expected//" "//stat_exp//new_line(" ")// &
              "    actual   : "//actual//" "//stat_act
    end function failure_message

    subroutine setup_case_un_D(spec, case, dim, unit_number, case_name, arg_pres, &
                               vtype, size, val_act, val_exp, test_name, stat, verbose, quiet, &
                               message, stat_exp)
        use :: newunit
        type(parameterization_spec_type), intent(in) :: spec
        integer(int32), intent(in) :: case, dim
        integer(int32), intent(inout) :: unit_number
        character(:), allocatable, intent(out) :: case_name
        type(arguments_presence_type), intent(inout) :: arg_pres

        ! arguments
        character(64), intent(out) :: vtype
        character(256), intent(out) :: test_name, val_act, val_exp
        integer(int32), intent(out) :: size(dim)
        logical, intent(out) :: stat, verbose, quiet

        ! expected
        character(256), intent(out) :: message
        logical, intent(out) :: stat_exp

        namelist /arguments/ vtype, size, val_act, val_exp, test_name, stat, verbose, quiet
        namelist /expected/ message, stat_exp

        type(test_parameter_type) :: param

        param = spec%get_test_parameter_in(case)
        read (unit=param%arguments_namelist, nml=arguments)
        read (unit=param%expected_namelist, nml=expected)

        case_name = "it should get "//enclose(param%expected(), "{")// &
                    " when input "//enclose(param%arguments(), "{")

        arg_pres = spec%get_optional_arguments_presence_in(case)

        unit_number = get_newunit_number()
        call set_assertion_message_unit(unit_number)
        open (unit=unit_number, status="scratch")
    end subroutine setup_case_un_D
end module test_equal_expectEqual_unitTests_common
