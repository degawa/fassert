module test_common_optval_collection
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_optval_unitTests_r32
    use :: test_common_optval_unitTests_r64
    use :: test_common_optval_unitTests_l
    implicit none
    private
    public :: collect_optval

contains
    subroutine collect_optval(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("optval_real32(), parameterized test.", &
                                  optvalReal32_parameterized_test) &
                     , new_unittest("optval_real64(), parameterized test.", &
                                    optvalReal64_parameterized_test) &
                     , new_unittest("optval_logical(), parameterized test.", &
                                    optvalLogical_parameterized_test) &
                     ]
    end subroutine collect_optval
end module test_common_optval_collection
