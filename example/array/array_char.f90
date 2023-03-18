program array_char
    use :: fassert
    implicit none

    logical :: stat

    call expect_equal(["a", "b", "c"], ["a", "b", "c"], &
                      "result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'", stat)
    call expect_equal(["c", "b", "a"], ["a", "b", "c"], &
                      "result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'", stat)
    !*
    !```Fortran
    ! call expect_equal(["a", "b", "c"], ["a", "b", "c"], &
    !                   "result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'", stat)
    ! ! PASSED: result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'
    !
    ! call expect_equal(["c", "b", "a"], ["a", "b", "c"], &
    !                   "result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'", stat)
    ! ! FAILED: result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'
    ! !     Expected: a b c
    ! !     Actual  : c b a
    !```
end program array_char
