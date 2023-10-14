module test_common_compare_equal_char_unitTests_common
    use, intrinsic :: iso_fortran_env
    use :: testdrive_util, only:to_string
    use :: par_funnel
    implicit none
    private
    public :: runner
    public :: failure_message
    public :: extract

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

    function failure_message(test_name, equals, equals_act) result(message)
        implicit none
        character(*), intent(in) :: test_name
        logical, intent(in) :: equals, equals_act
        character(:), allocatable :: message

        message = test_name//new_line(" ")// &
                  "    expected : "//to_string(equals)//new_line(" ")// &
                  "    actual   : "//to_string(equals_act)
    end function failure_message

    pure function extract(str) result(substr)
        implicit none
        character(*), intent(in) :: str
        character(:), allocatable :: substr

        ! extract substring
        ! "AbC |           "
        !  ^^^^
        substr = str(1:index(str, "|") - 1)
    end function extract
end module test_common_compare_equal_char_unitTests_common
