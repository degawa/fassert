module test_common_hasZero_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_hasZero_unitTests_hasZero
    implicit none
    private
    public :: collect_hasZero

contains
    subroutine collect_hasZero(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("has_zero(), parameterized test.", &
                                  hasZero_parameterized_test) &
                     ]
    end subroutine collect_hasZero
end module test_common_hasZero_collection
