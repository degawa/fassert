module test_common_hasZero_unitTests_hasZero
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string ! use to_string declared in testdrive_util.
    use :: par_funnel
    use :: test_common_hasZero_unitTests_common
    use :: fassert_common_hasZero
    implicit none
    private
    public :: hasZero_parameterized_test

contains
    subroutine hasZero_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call runner(error, case_spec(), run_test_cases)
    contains
        function case_spec() result(spec)
            implicit none
            type(parameterization_spec_type) :: spec

            spec = new_parameterization_spec( &
                   [ &
                   new_test_parameter(arguments="size=1, idx=1", expected="retval=false") &
                   , new_test_parameter(arguments="size=1, idx=0", expected="retval=true") &
                   , new_test_parameter(arguments="size=2, idx=2,1", expected="retval=false") &
                   , new_test_parameter(arguments="size=2, idx=0,1", expected="retval=true") &
                   , new_test_parameter(arguments="size=2, idx=1,0", expected="retval=true") &
                   , new_test_parameter(arguments="size=2, idx=0,0", expected="retval=true") &
                   , new_test_parameter(arguments="size=3, idx=5,2,1", expected="retval=false") &
                   , new_test_parameter(arguments="size=3, idx=5,2,0", expected="retval=true") &
                   , new_test_parameter(arguments="size=3, idx=5,0,1", expected="retval=true") &
                   , new_test_parameter(arguments="size=3, idx=0,3,1", expected="retval=true") &
                   , new_test_parameter(arguments="size=3, idx=0,3,0", expected="retval=true") &
                   , new_test_parameter(arguments="size=3, idx=2,0,0", expected="retval=true") &
                   , new_test_parameter(arguments="size=3, idx=0,0,4", expected="retval=true") &
                   , new_test_parameter(arguments="size=3, idx=0,0,0", expected="retval=true") &
                   ] &
                   )
        end function case_spec

        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            integer(int32), allocatable :: idx(:)
            logical :: expected

            block
                character(:), allocatable :: test_name
                integer(int32) :: case
                logical :: actual

                do case = 1, results%get_number_of_test_cases()
                    call setup(spec, case, test_name, idx, expected)
                    write (output_unit, '(12X, "- ",A)') test_name

                    !v------------------------
                    actual = has_zero(idx)
                    !^------------------------

                    call results%check_test(case, (actual .eqv. expected), &
                                            failure_message(expected, actual, test_name))
                    call teardown(idx, expected)
                end do
            end block
        end subroutine run_test_cases

        subroutine setup(spec, case, test_name, idx_input, retval_exp)
            type(parameterization_spec_type), intent(in) :: spec
            integer(int32), intent(in) :: case

            character(:), allocatable, intent(out) :: test_name
            integer(int32), allocatable, intent(out) :: idx_input(:)
            logical, intent(out) :: retval_exp

            type(test_parameter_type) :: param

            integer(int32) :: size, idx(15)
            logical :: retval

            namelist /arguments/ size, idx
            namelist /expected/ retval

            param = spec%get_test_parameter_in(case)
            read (unit=param%arguments_namelist, nml=arguments)
            read (unit=param%expected_namelist, nml=expected)

            allocate (idx_input, source=idx(1:size))
            retval_exp = retval

            test_name = "it should return "//param%expected()// &
                        " when input "//param%arguments()
        end subroutine setup

        subroutine teardown(idx, retval)
            implicit none
            integer(int32), allocatable, intent(inout) :: idx(:)
            logical, intent(inout) :: retval

            deallocate (idx)
            retval = .false.
        end subroutine teardown

        function failure_message(expected, actual, test_name) result(msg)
            use, intrinsic :: iso_fortran_env
            implicit none
            logical, intent(in) :: expected
            logical, intent(in) :: actual
            character(*), intent(in) :: test_name
            character(:), allocatable :: msg

            msg = test_name//new_line(" ")// &
                  "    expected : "//to_string(expected)//new_line(" ")// &
                  "    actual   : "//to_string(actual)
        end function failure_message
    end subroutine hasZero_parameterized_test
end module test_common_hasZero_unitTests_hasZero
