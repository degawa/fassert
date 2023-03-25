module assert_real32_rank3
    use, intrinsic :: iso_fortran_env
    use :: fassert_kit
    implicit none
    private
    public :: is_equal_real32_rank3
    public :: output_on_failure_real32_rank3

contains
    pure logical function is_equal_real32_rank3(actual, expected)
        implicit none
        class(*), intent(in) :: actual(:, :, :)
        class(*), intent(in) :: expected(:, :, :)

        is_equal_real32_rank3 = .false.
        select type (actual); type is (real(real32))
            select type (expected); type is (real(real32))

                is_equal_real32_rank3 = are_equal(actual, expected)

            end select
        end select
    end function is_equal_real32_rank3

    subroutine output_on_failure_real32_rank3(actual, expected)
        implicit none
        class(*), intent(in) :: actual(:, :, :)
        class(*), intent(in) :: expected(:, :, :)

        character(*), parameter :: fmt = '('//fmt_indent//',A,"[",3(E9.2e2,","),"...]")'

        select type (actual); type is (real(real32))
            select type (expected); type is (real(real32))

                write (assertion_message_unit, fmt) "Expected  : ", expected(1:3, 1, 1)
                write (assertion_message_unit, fmt) "Actual    : ", actual(1:3, 1, 1)
                write (assertion_message_unit, fmt) "Difference: ", expected(1:3, 1, 1) - actual(1:3, 1, 1)

            class default
                write (assertion_message_unit, '(A)') "Type mismatch: `expected` is not real(real32)"
            end select
        class default
            write (assertion_message_unit, '(A)') "Type mismatch: `actual` is not real(real32)"
        end select
    end subroutine output_on_failure_real32_rank3

end module assert_real32_rank3

program real32_rank3
    use, intrinsic :: iso_fortran_env
    use :: assert_real32_rank3
    use :: fassert
    implicit none

    real(real32), allocatable :: x(:, :, :), y(:, :, :)
    logical :: stat

    allocate (x(4, 3, 2), source=1e0)
    allocate (y(4, 3, 2), source=2e0)

    print *, "v--"
    call expect_equal(x, y, "x should equal to y", stat, &
                      is_equal_real32_rank3, output_on_failure_real32_rank3)
    print *, "^--"

    deallocate (x); allocate (x(4, 3, 2), source=1e0)
    deallocate (y); allocate (y(4, 3, 1), source=2e0)
    print *, "v-- capturing shape mismatch"
    call expect_equal(x, y, "x should equal to y", stat, &
                      is_equal_real32_rank3, output_on_failure_real32_rank3)
    print *, "^--"
end program real32_rank3
