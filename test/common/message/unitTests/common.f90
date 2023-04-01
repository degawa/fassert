module test_common_message_unitTests_common
    use :: par_funnel
    implicit none
    private
    public :: runner

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
end module test_common_message_unitTests_common
