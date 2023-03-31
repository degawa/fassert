module test_common_compare_equal_shape_unitTests_areSameLength
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string
    use :: strings_enclose
    use :: fassert_common_compare_equal_length
    implicit none
    private
    public :: areSameLengthStr_parameterized_test

contains
    subroutine areSameLengthStr_parameterized_test(error)
        use :: par_funnel
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        type(test_results_type) :: results

        spec = new_parameterization_spec( &
               [ &
               new_test_parameter(arguments="a='abc' b='xyz'", expected="same_length=true") &
               , new_test_parameter(arguments="a='abc' b='abca'", expected="same_length=false") &
               , new_test_parameter(arguments="a='abc' b='xyz '", expected="same_length=true") &
               ] &
               )
        call results%construct(spec)

        call run_test_cases(spec, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
        call results%destruct()
    contains
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            character(:), allocatable :: a
            character(:), allocatable :: b
            logical :: same_length

            namelist /arguments/ a, b
            namelist /expected/ same_length

            block
                character(:), allocatable :: test_name
                type(test_parameter_type) :: param
                integer(int32) :: case
                integer(int32) :: len_a(3) = [3, 3, 3], len_b(3) = [3, 4, 4]

                do case = 1, results%get_number_of_test_cases()
                    allocate (character(len_a(case)) :: a)
                    allocate (character(len_b(case)) :: b)

                    param = spec%get_test_parameter_in(case)
                    read (unit=param%arguments_namelist, nml=arguments)
                    read (unit=param%expected_namelist, nml=expected)

                    test_name = "it should return "//param%expected()// &
                                " when input "//param%arguments()

                    write (output_unit, '(12X, "- ",A)') test_name
                    call results%check_test(case, are_same_length(a, b) .eqv. same_length, &
                                            test_name//new_line(" ")// &
                                            "    expected : "//to_string(same_length)//new_line(" ")// &
                                            "    actual   : "//to_string(are_same_length(a, b)))

                    if (allocated(a)) deallocate (a)
                    if (allocated(b)) deallocate (b)
                end do
            end block
        end subroutine run_test_cases
    end subroutine areSameLengthStr_parameterized_test
end module test_common_compare_equal_shape_unitTests_areSameLength
