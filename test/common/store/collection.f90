module test_common_store_collection
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_store_unitTests
    implicit none
    private
    public :: collect_store

contains
    subroutine collect_store(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("store_logical(), stat should be the same as the argument `val` when input stat.", &
                                  stat_should_be_the_same_as_val_when_input_stat) &
                     , new_unittest("store_logical(), stat should not be updated when does not input stat.", &
                                    stat_should_not_be_updated_when_does_not_input_stat) &
                     ]
    end subroutine collect_store
end module test_common_store_collection
