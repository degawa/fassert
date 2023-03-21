module test_common_store_unitTests_append
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: strings_enclose
    use :: fassert_common_store
    use :: fassert_common_message
    use :: fassert_common_compare_equal_length
    implicit none
    private
    public :: str_should_be_the_same_as_val_when_input_unallocated_str
    public :: str_should_be_concatenated_with_val_when_input_allocated_str

contains
    subroutine str_should_be_the_same_as_val_when_input_unallocated_str(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: str, val, expected

        val = "string to append"
        expected = val

        call append(str, val)
        call check(error, are_same_length(str, expected), &
                   "expected "//to_string(len(expected))//", but got "//to_string(len(str)))
        if (occurred(error)) return

        call check(error, str == expected, &
                   "expected "//enclose(expected, '"')//", but got "//enclose(str, '"'))
    end subroutine str_should_be_the_same_as_val_when_input_unallocated_str

    subroutine str_should_be_concatenated_with_val_when_input_allocated_str(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: str, val, expected

        str = "string to be appended"
        val = "string to append"
        expected = str//NL//val

        call append(str, val)
        call check(error, are_same_length(str, expected), &
                   "expected "//to_string(len(expected))//", but got "//to_string(len(str)))
        if (occurred(error)) return

        call check(error, str == expected, &
                   "expected "//enclose(expected, '"')//", but got "//enclose(str, '"'))
    end subroutine str_should_be_concatenated_with_val_when_input_allocated_str
end module test_common_store_unitTests_append
