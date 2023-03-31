module test_common_compare_equal_shape_unitTests_areSameShape_D3
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: strings_enclose
    use :: test_common_compare_equal_shape_unitTests_common
    use :: fassert_common_compare_equal_shape
    implicit none
    private
    ! D: dimension
    public :: areSameShapeD3_parameterized_test

contains
    subroutine areSameShapeD3_parameterized_test(error)
        use :: par_funnel
        use :: fassert_common_unit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        type(test_results_type) :: results

        spec = new_parameterization_spec( &
               [ &
                 new_test_parameter(arguments="vtype='int32' dim_a=3,2,4 dim_b=3,2,4" , expected="same_shape=true") &
               , new_test_parameter(arguments="vtype='int32' dim_a=3,2,4 dim_b=2,3,1" , expected="same_shape=false") &
               , new_test_parameter(arguments="vtype='real32' dim_a=3,2,4 dim_b=3,2,4", expected="same_shape=true") &
               , new_test_parameter(arguments="vtype='real32' dim_a=3,2,4 dim_b=2,3,1", expected="same_shape=false") &
               , new_test_parameter(arguments="vtype='real64' dim_a=3,2,4 dim_b=3,2,4", expected="same_shape=true") &
               , new_test_parameter(arguments="vtype='real64' dim_a=3,2,4 dim_b=2,3,1", expected="same_shape=false") &
               ]) !&
        call results%construct(spec)

        call run_test_cases(spec, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
        call results%destruct()
    contains
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            character(64) :: vtype
            integer(int32) :: dim_a(3), dim_b(3)
            logical :: same_shape

            namelist /arguments/ vtype, dim_a, dim_b
            namelist /expected/ same_shape

            block
                character(:), allocatable :: test_name
                type(test_parameter_type) :: param
                integer(int32) :: case
                class(*), allocatable :: a(:, :, :), b(:, :, :)

                do case = 1, results%get_number_of_test_cases()
                    param = spec%get_test_parameter_in(case)
                    read (unit=param%arguments_namelist, nml=arguments)
                    read (unit=param%expected_namelist, nml=expected)

                    test_name = "it should return "//param%expected()// &
                                " when input "//param%arguments()
                    write (output_unit, '(12X, "- ",A)') test_name

                    select case (trim(adjustl(vtype)))
                    case ("int8")
                    case ("int16")
                    case ("int32")
                        block
                            allocate (integer(int32) :: a(dim_a(1), dim_a(2), dim_a(3)))
                            allocate (integer(int32) :: b(dim_b(1), dim_b(2), dim_b(3)))

                            call results%check_test(case, are_same_shape(a, b) .eqv. same_shape, &
                                                    failure_message(same_shape, are_same_shape(a, b), test_name))
                        end block
                    case ("int64")
                    case ("real32")
                        block
                            allocate (real(real32) :: a(dim_a(1), dim_a(2), dim_a(3)))
                            allocate (real(real32) :: b(dim_b(1), dim_b(2), dim_b(3)))

                            call results%check_test(case, are_same_shape(a, b) .eqv. same_shape, &
                                                    failure_message(same_shape, are_same_shape(a, b), test_name))
                        end block
                    case ("real64")
                        block
                            allocate (real(real64) :: a(dim_a(1), dim_a(2), dim_a(3)))
                            allocate (real(real64) :: b(dim_b(1), dim_b(2), dim_b(3)))

                            call results%check_test(case, are_same_shape(a, b) .eqv. same_shape, &
                                                    failure_message(same_shape, are_same_shape(a, b), test_name))
                        end block
                    end select
                    if (allocated(a)) deallocate (a)
                    if (allocated(b)) deallocate (b)
                end do
            end block
        end subroutine run_test_cases
    end subroutine areSameShapeD3_parameterized_test
end module test_common_compare_equal_shape_unitTests_areSameShape_D3
