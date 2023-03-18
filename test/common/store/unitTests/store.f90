module test_common_store_unitTests
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: fassert_common_store
    implicit none
    private
    public :: statLogical_should_be_the_same_as_val_when_input_stat
    public :: statLogical_should_not_be_updated_when_does_not_input_stat

contains
    subroutine statLogical_should_be_the_same_as_val_when_input_stat(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat

        stat = .false.
        call store(stat, .true.)
        call check(error, stat .eqv. .true., &
                   "expected "//to_string(.true.)//", but got "//to_string(stat))
        if (occurred(error)) return

        call store(stat, .false.)
        call check(error, stat .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(stat))
    end subroutine statLogical_should_be_the_same_as_val_when_input_stat

    subroutine statLogical_should_not_be_updated_when_does_not_input_stat(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat

        stat = .false.
        call store(val=.true.)
        call check(error, stat .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(stat))
        if (occurred(error)) return

        stat = .true.
        call store(val=.false.)
        call check(error, stat .eqv. .true., &
                   "expected "//to_string(.true.)//", but got "//to_string(stat))
    end subroutine statLogical_should_not_be_updated_when_does_not_input_stat
end module test_common_store_unitTests
