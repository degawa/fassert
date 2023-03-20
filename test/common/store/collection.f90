module test_common_store_collection
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_store_unitTests_store
    use :: test_common_store_unitTests_append
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
                                  statLogical_should_be_the_same_as_val_when_input_stat) &
                     , new_unittest("store_logical(), stat should not be updated when does not input stat.", &
                                    statLogical_should_not_be_updated_when_does_not_input_stat) &
                     , new_unittest("append_string(), str should be the same as the string `val`"// &
                                    " when input unallocated str.", &
                                    str_should_be_the_same_as_val_when_input_unallocated_str) &
                     , new_unittest("append_string(), str should be concatenated with `val`"// &
                                    " when input allocated str.", &
                                    str_should_be_concatenated_with_val_when_input_allocated_str) &
                     ]
    end subroutine collect_store
end module test_common_store_collection
