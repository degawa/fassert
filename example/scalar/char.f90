program char
    use :: fassert
    implicit none

    logical :: stat

    call expect_equal("a"//"b", "ab", "result should be 'ab' when input 'a', 'b'", stat)
    call expect_equal("b"//"a", "ab", "result should be 'ab' when input 'a', 'b'", stat)
    !|
    !```Fortran
    ! call expect_equal("a"//"b", "ab", "result should be 'ab' when input 'a', 'b'", stat)
    ! ! PASSED: result should be 1 when input 0+1'
    !
    ! call expect_equal("b"//"a", "ab", "result should be 'ab' when input 'a', 'b'", stat)
    ! ! FAILED: result should be 1 when input 0+1
    ! !     Expected: ab
    ! !     Actual  : ba
    !```

    call expect_equal("ab", "ab  ", "'ab' and 'ab  ' are not equal", stat)
    !|
    !```Fortran
    ! call expect_equal("ab", "ab  ", "'ab' and 'ab  ' are different", stat)
    ! ! FAILED: 'ab' and 'ab  ' are different [length check]
    ! !    Expected: 4
    ! !    Actual  : 2
    !```
    call expect_char_equal("ab", "ab  ", "'ab' and 'ab  ' are equal", stat)
    !|
    !```Fortran
    ! call expect_char_equal("ab", "ab  ", "'ab' and 'ab  ' is equal", stat)
    ! ! PASSED: 'ab' and 'ab  ' is equal
    !```

    call expect_char_equal("ab", "Ab  ", "'ab' and 'Ab  ' are equal", stat, ignore_case=.true.)
    !|
    !```Fortran
    ! call expect_char_equal("ab", "Ab  ", "'ab' and 'Ab  ' are equal", stat, ignore_case=.true.)
    ! ! PASSED: 'ab' and 'AB  ' are equal
    !```

    call expect_char_equal("ab", "Ab  ", "'ab' and 'Ab  ' are equal", stat, ignore_case=.true., verbose=.true.)
    !|
    !```Fortran
    ! call expect_char_equal("ab", "Ab  ", "'ab' and 'Ab  ' are equal", stat, ignore_case=.true., verbose=.true.)
    ! ! PASSED: 'ab' and 'Ab  ' are equal
    ! !     Expected: Ab
    ! !     Actual  : ab
    !```

    call expect_char_equal("ab", "Ab  ", "'ab' and 'Ab  ' are equal", stat, ignore_case=.true., quiet=.true.)
    !|
    !```Fortran
    ! call expect_char_equal("ab", "Ab  ", "'ab' and 'Ab  ' are equal", stat, ignore_case=.true., quiet=.true.)
    !```

    call expect_char_equal("ab", "Ab", "'ab' and 'Ab' are equal", stat)
    !|
    !```Fortran
    ! call expect_char_equal("ab", "Ab", "'ab' and 'Ab' are equal", stat)
    ! ! FAILED: 'ab' and 'Ab' are equal
    ! !     Expected: Ab
    ! !     Actual  : ab
    !```
    call expect_char_equal("ab", "Ab", "'ab' and 'Ab' are equal", stat, expected_failure=.true.)
    !|
    !```Fortran
    ! call expect_char_equal("ab", "Ab", "'ab' and 'Ab' are equal", stat, expected_failure=.true.)
    ! ! PASSED: 'ab' and 'Ab' are equal [expected failure]
    !```

end program char
