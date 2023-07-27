module vector2d
    use, intrinsic :: iso_fortran_env
    use :: fassert_kit
    implicit none
    private
    public :: is_equal_vec2d
    public :: output_on_failure_vec2d
    public :: output_on_failure_vec2d_to_str

    type, public :: vector2d_type
        real(real32), public :: x
        real(real32), public :: y
    end type vector2d_type

contains
    pure logical function is_equal_vec2d(actual, expected)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected

        is_equal_vec2d = .false.
        select type (actual); type is (vector2d_type)
            select type (expected); type is (vector2d_type)

                is_equal_vec2d = &
                    all([is_equal(actual%x, expected%x), &
                         is_equal(actual%y, expected%y)])

            end select
        end select
    end function is_equal_vec2d

    subroutine output_on_failure_vec2d(actual, expected)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected

        character(*), parameter :: fmt = '('//fmt_indent//',A,"[",G0,",",G0,"]")'

        select type (actual); type is (vector2d_type)
            select type (expected); type is (vector2d_type)

                write (assertion_message_unit, fmt) "Expected: ", expected%x, expected%y
                write (assertion_message_unit, fmt) "Actual  : ", actual%x, actual%y
                write (assertion_message_unit, fmt) "Difference:", (expected%x - actual%x), (expected%y - actual%y)

            class default
                write (assertion_message_unit, '(A)') "Type mismatch: `expected` is not vector2d_type"
            end select
        class default
            write (assertion_message_unit, '(A)') "Type mismatch: `actual` is not vector2d_type"
        end select
    end subroutine output_on_failure_vec2d

    pure subroutine output_on_failure_vec2d_to_str(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected
        character(:), allocatable, intent(inout) :: output_message
        character(64) :: buffer

        character(*), parameter :: fmt = '('//fmt_indent//',A,"[",G0,",",G0,"]")'

        select type (actual); type is (vector2d_type)
            select type (expected); type is (vector2d_type)

                write (buffer, fmt) "Expected: ", expected%x, expected%y
                call append(output_message, trim(buffer))
                write (buffer, fmt) "Actual  : ", actual%x, actual%y
                call append(output_message, trim(buffer))
                write (buffer, fmt) "Difference:", (expected%x - actual%x), (expected%y - actual%y)
                call append(output_message, trim(buffer))

            class default
                call append(output_message, "Type mismatch: `expected` is not vector2d_type")
            end select
        class default
            call append(output_message, "Type mismatch: `actual` is not vector2d_type")
        end select
    end subroutine output_on_failure_vec2d_to_str
end module vector2d

program user_defined_vector2d
    use :: vector2d
    use :: fassert
    implicit none

    type(vector2d_type) :: x, y
    logical :: stat
    character(:), allocatable :: msg

    x = vector2d_type(1d0, 2d0)
    y = vector2d_type(2d0, 1d0)

    print *, "v--"
    call expect_equal(x, y, "vector x should equal to y", stat, &
                      is_equal_vec2d, output_on_failure_vec2d)
    print *, "^--"

    print *, "v--"
    call expect_equal(x, y, "vector x should equal to y", stat, &
                      is_equal_vec2d, output_on_failure_vec2d_to_str, &
                      output_message=msg)
    print '(A)', msg
    print *, "^--"

    print *, "v--"
    call expect_equal(x, 0d0, "vector x should equal to y", stat, &
                      is_equal_vec2d, output_on_failure_vec2d)
    print *, "^--"

    call assert_equal(x, y, "vector x should equal to y", &
                      is_equal_vec2d, output_on_failure_vec2d)
    ! FAILED: vector x should equal to y
    !     Expected: [2.00000000,1.00000000]
    !     Actual  : [1.00000000,2.00000000]
    !     Difference:[1.00000000,-1.00000000]
    ! ERROR STOP

end program user_defined_vector2d
