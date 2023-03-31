module test_logical_expectTrue_unitTests_expectTrue_common
    use, intrinsic :: iso_fortran_env
    use :: strings_enclose
    use :: testdrive_util, only:to_string
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
        procedure :: setup_case_wo_un
    end interface
    interface teardown_case
        procedure :: teardown_case_un
        procedure :: teardown_case_wo_un
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

        call results%construct(spec)
        call run_test_cases(spec, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
        call results%destruct()
    end subroutine runner

    subroutine setup_case_un(spec, case, param, unit_number, case_name, arg_pres, &
                             actual, test_name, stat, verbose, expected_failure, quiet, &
                             message, stat_exp)
        use :: newunit
        type(parameterization_spec_type), intent(in) :: spec
        integer(int32), intent(in) :: case
        type(test_parameter_type), intent(out) :: param
        integer(int32), intent(inout) :: unit_number
        character(:), allocatable, intent(out) :: case_name
        type(arguments_presence_type), intent(inout) :: arg_pres

        ! arguments
        logical, intent(out) :: actual, stat, verbose, expected_failure, quiet
        character(256), intent(out) :: test_name

        ! expected
        character(256), intent(out) :: message
        logical, intent(out) :: stat_exp

        namelist /arguments/ actual, test_name, stat, verbose, expected_failure, quiet
        namelist /expected/ message, stat_exp

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

    subroutine setup_case_wo_un(spec, case, param, case_name, arg_pres, &
                                actual, test_name, stat, verbose, expected_failure, quiet, &
                                message, stat_exp, alloced)
        use :: newunit
        type(parameterization_spec_type), intent(in) :: spec
        integer(int32), intent(in) :: case
        type(test_parameter_type), intent(out) :: param
        character(:), allocatable, intent(out) :: case_name
        type(arguments_presence_type), intent(inout) :: arg_pres

        ! arguments
        logical, intent(out) :: actual, stat, verbose, expected_failure, quiet
        character(256), intent(out) :: test_name

        ! expected
        character(256), intent(out) :: message
        logical, intent(out) :: stat_exp, alloced

        namelist /arguments/ actual, test_name, stat, verbose, expected_failure, quiet
        namelist /expected/ message, stat_exp, alloced

        param = spec%get_test_parameter_in(case)
        read (unit=param%arguments_namelist, nml=arguments)
        read (unit=param%expected_namelist, nml=expected)

        case_name = "it should get "//enclose(param%expected(), "{")// &
                    " when input "//enclose(param%arguments(), "{")

        arg_pres = spec%get_optional_arguments_presence_in(case)
    end subroutine setup_case_wo_un

    subroutine teardown_case_wo_un(buffer)
        character(:), allocatable, intent(inout) :: buffer
        deallocate (buffer)
    end subroutine teardown_case_wo_un

    function replace_all(str, it, with) result(replaced)
        implicit none
        character(*), intent(in) :: str
        character(*), intent(in) :: it
        character(*), intent(in) :: with
        character(:), allocatable :: replaced

        integer(int32) :: len_it, len_with, idx_it
        len_it = len(it)
        len_with = len(with)

        replaced = str

        idx_it = index(replaced, it, back=.false.)
        do while (idx_it > 0)
            replaced = replaced(1:idx_it - 1)//with//replaced(idx_it + len_it:)
            idx_it = index(replaced, it, back=.false.)
        end do
    end function replace_all

    function failure_message(case_name, expected, actual, stat, alloc) result(msg)
        implicit none
        character(*), intent(in) :: case_name
        character(*), intent(in) :: expected
        character(*), intent(in) :: actual
        logical, intent(in) :: stat(2)
        logical, intent(in), optional :: alloc(2)
        character(:), allocatable :: msg

        character(:), allocatable :: alloc_exp, alloc_act, stat_exp, stat_act

        stat_exp = to_string(stat(1))
        stat_act = to_string(stat(2))
        alloc_exp = ""
        alloc_act = ""
        if (present(alloc)) then
            alloc_exp = to_string(alloc(1))
            alloc_act = to_string(alloc(2))
        end if
        msg = case_name//new_line(" ")// &
              "    expected : "//expected//" "//stat_exp//" "//alloc_exp//new_line(" ")// &
              "    actual   : "//actual//" "//stat_act//" "//alloc_act
    end function failure_message
end module test_logical_expectTrue_unitTests_expectTrue_common
