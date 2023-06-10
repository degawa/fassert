module test_common_compare_approx_real_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_compare_approx_real_unitTests_isApproxEqual
    implicit none
    private
    public :: collect_is_approx_equal

contains
    subroutine collect_is_approx_equal(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("is_approx_equal_real32()", &
                                  is_approx_equal_real32) &
                     , new_unittest("is_approx_equal_real64()", &
                                    is_approx_equal_real64) &
                     ]
    end subroutine collect_is_approx_equal
end module test_common_compare_approx_real_collection
