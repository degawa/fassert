module test_common_compare_equal_shape_unitTests_areSameShape_D1
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: par_funnel
    use :: test_common_compare_equal_shape_unitTests_common
    use :: fassert_common_compare_equal_shape
    implicit none
    private
    ! D: dimension
    public :: areSameShapeD1_parameterized_test

contains
    subroutine areSameShapeD1_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        spec = new_parameterization_spec( &
               [ &
                 new_test_parameter(arguments="vtype='int32' dim_a=3 dim_b=3" , expected="same_shape=true") &
               , new_test_parameter(arguments="vtype='int32' dim_a=3 dim_b=2" , expected="same_shape=false") &
               , new_test_parameter(arguments="vtype='real32' dim_a=3 dim_b=3", expected="same_shape=true") &
               , new_test_parameter(arguments="vtype='real32' dim_a=3 dim_b=2", expected="same_shape=false") &
               , new_test_parameter(arguments="vtype='real64' dim_a=3 dim_b=3", expected="same_shape=true") &
               , new_test_parameter(arguments="vtype='real64' dim_a=3 dim_b=2", expected="same_shape=false") &
               ]) !&
        call runner(error, spec, run_test_cases)
    end subroutine areSameShapeD1_parameterized_test

    subroutine run_test_cases(spec, results)
        type(parameterization_spec_type), intent(in) :: spec
        type(test_results_type), intent(inout) :: results

        character(64) :: vtype
        integer(int32) :: dim_a(1), dim_b(1)
        logical :: same_shape

        namelist /arguments/ vtype, dim_a, dim_b
        namelist /expected/ same_shape

        block
            character(:), allocatable :: test_name
            type(test_parameter_type) :: param
            integer(int32) :: case
            class(*), allocatable :: a(:), b(:)

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
                        allocate (integer(int32) :: a(dim_a(1)))
                        allocate (integer(int32) :: b(dim_b(1)))

                        call results%check_test(case, are_same_shape(a, b) .eqv. same_shape, &
                                                failure_message(same_shape, are_same_shape(a, b), test_name))
                    end block
                case ("int64")
                case ("real32")
                    block
                        allocate (real(real32) :: a(dim_a(1)))
                        allocate (real(real32) :: b(dim_b(1)))

                        call results%check_test(case, are_same_shape(a, b) .eqv. same_shape, &
                                                failure_message(same_shape, are_same_shape(a, b), test_name))
                    end block
                case ("real64")
                    block
                        allocate (real(real64) :: a(dim_a(1)))
                        allocate (real(real64) :: b(dim_b(1)))

                        call results%check_test(case, are_same_shape(a, b) .eqv. same_shape, &
                                                failure_message(same_shape, are_same_shape(a, b), test_name))
                    end block
                end select
                if (allocated(a)) deallocate (a)
                if (allocated(b)) deallocate (b)
            end do
        end block
    end subroutine run_test_cases
end module test_common_compare_equal_shape_unitTests_areSameShape_D1
