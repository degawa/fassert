program array_char
    use :: fassert
    implicit none

    logical :: stat

    call expect_equal(["a", "b", "c"], ["a", "b", "c"], &
                      "result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'", stat)
    call expect_equal(["c", "b", "a"], ["a", "b", "c"], &
                      "result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'", stat)
    !|
    !```Fortran
    ! call expect_equal(["a", "b", "c"], ["a", "b", "c"], &
    !                   "result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'", stat)
    ! ! PASSED: result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'
    !
    ! call expect_equal(["c", "b", "a"], ["a", "b", "c"], &
    !                   "result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'", stat)
    ! ! FAILED: result should be ['a', 'b', 'c'] when input 'a', 'b', 'c'
    ! !     Expected: a
    ! !     Actual  : c
    ! !     Position: at (1)
    !```

    call expect_char_equal(["a", "b", "c"], ["a", "b", "c"], &
                           "[a, b, c] and [a, b, c] are equal", stat)
    !|
    !```Fortran
    ! call expect_char_equal(["a", "b", "c"], ["a", "b", "c"], &
    !                        "[a, b, c] and [a, b, c] are equal", stat)
    ! ! PASSED: [a, b, c] and [a, b, c] are equal
    !```

    call expect_char_equal(["a ", "b ", "c "], ["a", "b", "c"], &
                           "[a , b , c ] and [a, b, c] are equal", stat)
    !|
    !```Fortran
    ! call expect_char_equal(["a ", "b ", "c "], ["a", "b", "c"], &
    !                        "[a , b , c ] and [a, b, c] are equal", stat)
    ! ! PASSED: [a , b , c ] and [a, b, c] are equal
    !```

    call expect_char_equal(["a ", "b ", "c "], ["A", "B", "C"], &
                           "[a , b , c ] and [A, B, C] are equal", stat, ignore_case=.true.)
    !|
    !```Fortran
    ! call expect_char_equal(["a ", "b ", "c "], ["A", "B", "C"], &
    !                        "[a , b , c ] and [A, B, C] are equal", stat, ignore_case=.true.)
    ! ! PASSED: [a , b , c ] and [A, B, C] are equal
    !```

    call expect_char_equal(["a ", "b ", "c "], ["A", "B", "C"], &
                           "[a , b , c ] and [A, B, C] are equal", stat, ignore_case=.false.)
    !|
    !```Fortran
    ! call expect_char_equal(["a ", "b ", "c "], ["A", "B", "C"], &
    !                        "[a , b , c ] and [A, B, C] are equal", stat, ignore_case=.false.)
    ! ! FAILED: [a , b , c ] and [A, B, C] are equal
    ! !     Expected: A
    ! !     Actual  : a
    ! !     Position: at (1)
    !```

    call expect_char_equal(["a ", "b ", "c "], ["a", "b", "c"], &
                           "[a , b , c ] and [a, b, c] are equal", stat, verbose=.true.)
    !|
    !```Fortran
    ! call expect_char_equal(["a ", "b ", "c "], ["a", "b", "c"], &
    !                        "[a , b , c ] and [a, b, c] are equal", stat, verbose=.true.)
    ! ! PASSED: [a , b , c ] and [a, b, c] are equal
    ! !     All elements are equivalent.
    !```

    call expect_char_equal(["a ", "b ", "c "], ["A", "B", "C"], &
                           "[a , b , c ] and [A, B, C] are equal", stat, verbose=.false.)
    !|
    !```Fortran
    ! call expect_char_equal(["a ", "b ", "c "], ["A", "B", "C"], &
    !                        "[a , b , c ] and [A, B, C] are equal", stat, verbose=.false.)
    ! ! FAILED: [a , b , c ] and [A, B, C] are equal
    ! !     Expected: A
    ! !     Actual  : a
    ! !     Position: at (1)
    !```

    call expect_char_equal(["a ", "b ", "c "], ["A", "B", "C"], &
                           "[a , b , c ] and [A, B, C] are equal", stat, verbose=.false., quiet=.true.)
    !|
    !```Fortran
    ! call expect_char_equal(["a ", "b ", "c "], ["A", "B", "C"], &
    !                        "[a , b , c ] and [A, B, C] are equal", stat, verbose=.false., quiet=.true.)
    !```

    call expect_char_equal(["a ", "b ", "c "], ["a", "b", "c"], &
                           "[a , b , c ] and [a, b, c] are equal", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_char_equal(["a ", "b ", "c "], ["a", "b", "c"], &
    !                        "[a , b , c ] and [a, b, c] are equal", stat, verbose=.true., quiet=.true.)
    ! !    All elements are equivalent.
    !```

    call expect_char_equal(["a ", "b ", "c "], ["a", "B", "C"], &
                           "[a , b , c ] and [a, B, C] are equal", stat, verbose=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_char_equal(["a ", "b ", "c "], ["A", "B", "C"], &
    !                        "[a , b , c ] and [a, B, C] are equal", stat, verbose=.true., quiet=.true.)
    ! !    Expected: B
    ! !    Actual  : b
    ! !    Position: at (2)
    !```
end program array_char
