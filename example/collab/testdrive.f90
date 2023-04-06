module collab_testdrive_unittests
    use :: testdrive, only:error_type, check
    use :: fassert
    implicit none
    private
    public :: doublify_return_20_when_input_10

contains
    subroutine doublify_return_20_when_input_10(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(doublify(10), 20, "it should return 20 when input 10", &
                          stat, output_message=msg)
        ! stat = .false.
        ! msg= FAILED: it should return 20 when input 10
        !          Expected: 20
        !          Actual  : 0
        call check(error, stat, msg)
    end subroutine doublify_return_20_when_input_10

    function doublify(input) result(retval)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), intent(in) :: input
        integer(int32) :: retval

        retval = input*0
    end function doublify
end module collab_testdrive_unittests

module collab_testdrive_collect
    use :: testdrive, only:unittest_type, new_unittest
    use :: collab_testdrive_unittests
    implicit none
    private
    public :: collect

contains
    subroutine collect(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("doublify() should return 20 when input 10", &
                                  doublify_return_20_when_input_10) &
                     ]
    end subroutine collect
end module collab_testdrive_collect

program collab_testdrive
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:testsuite_type, new_testsuite
    use :: testdrive_util, only:run_test
    use :: collab_testdrive_collect
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("collaboration with testdrive", collect) &
                  ]
    call run_test(test_suites)
end program collab_testdrive
