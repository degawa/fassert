module test_common_check_unitTests_common
    use, intrinsic :: iso_fortran_env
    use :: strings_enclose
    use :: testdrive_util, only:to_string
    use :: par_funnel
    use :: fassert_common_unit
    implicit none
    private
    public :: setup_case
    public :: teardown_case
    public :: failure_message
    public :: failure_message_msg

    interface setup_case
        procedure :: setup_case_un
        procedure :: setup_case_wo_un
    end interface
    interface teardown_case
        procedure :: teardown_case_un
        procedure :: teardown_case_wo_un
    end interface

contains
    subroutine setup_case_un(spec, case, param, unit_number, case_name, arg_pres, &
                             test_name, condition, stat, quiet, &
                             message, stat_exp)
        use :: newunit
        type(parameterization_spec_type), intent(in) :: spec
        integer(int32), intent(in) :: case
        type(test_parameter_type), intent(inout) :: param
        integer(int32), intent(inout) :: unit_number
        character(:), allocatable, intent(out) :: case_name
        type(arguments_presence_type), intent(inout) :: arg_pres

        ! arguments
        character(256), intent(out) :: test_name
        logical, intent(out) :: condition, stat, quiet
        ! expected
        character(256), intent(out) :: message
        logical, intent(out) :: stat_exp

        namelist /arguments/ test_name, condition, stat, quiet
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

    subroutine setup_case_wo_un(spec, case, param, case_name, arg_pres, &
                                test_name, condition, stat, quiet, &
                                message, stat_exp, alloced)
        use :: newunit
        type(parameterization_spec_type), intent(in) :: spec
        integer(int32), intent(in) :: case
        type(test_parameter_type), intent(inout) :: param
        character(:), allocatable, intent(out) :: case_name
        type(arguments_presence_type), intent(inout) :: arg_pres

        ! arguments
        character(256), intent(out) :: test_name
        logical, intent(out) :: condition, stat, quiet
        ! expected
        character(256), intent(out) :: message
        logical, intent(out) :: stat_exp, alloced

        namelist /arguments/ test_name, condition, stat, quiet
        namelist /expected/ message, stat_exp, alloced

        param = spec%get_test_parameter_in(case)
        read (unit=param%arguments_namelist, nml=arguments)
        read (unit=param%expected_namelist, nml=expected)

        case_name = "it should get "//enclose(param%expected(), "{")// &
                    " when input "//enclose(param%arguments(), "{")

        arg_pres = spec%get_optional_arguments_presence_in(case)
    end subroutine setup_case_wo_un

    subroutine teardown_case_un(unit_number, buffer)
        integer(int32), intent(in) :: unit_number
        character(:), allocatable, intent(inout) :: buffer
        close (unit_number)
        call set_assertion_message_unit(output_unit)
        deallocate (buffer)
    end subroutine teardown_case_un

    subroutine teardown_case_wo_un(actual_message)
        character(:), allocatable, intent(inout) :: actual_message

        deallocate (actual_message)
    end subroutine teardown_case_wo_un

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

    function failure_message_msg(case_name, expected, actual, alloc, stat) result(msg)
        implicit none
        character(*), intent(in) :: case_name
        character(*), intent(in) :: expected
        character(*), intent(in) :: actual
        logical, intent(in) :: alloc(2)
        logical, intent(in), optional :: stat(2)
        character(:), allocatable :: msg

        character(:), allocatable :: alloc_exp, alloc_act, stat_exp, stat_act

        alloc_exp = to_string(alloc(1))
        alloc_act = to_string(alloc(2))
        stat_exp = ""
        stat_act = ""
        if (present(stat)) then
            stat_exp = to_string(stat(1))
            stat_act = to_string(stat(2))
        end if
        msg = case_name//new_line(" ")// &
              "    expected : "//expected//" "//alloc_exp//" "//stat_exp//new_line(" ")// &
              "    actual   : "//actual//" "//alloc_act//" "//stat_act
    end function failure_message_msg
end module test_common_check_unitTests_common
