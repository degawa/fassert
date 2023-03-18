program char
    use :: fassert
    implicit none

    logical :: stat

    call expect_equal("a"//"b", "ab", "result should be 'ab' when input 'a', 'b'", stat)
    call expect_equal("b"//"a", "ab", "result should be 'ab' when input 'a', 'b'", stat)
    !*
    !```Fortran
    ! call expect_equal("a"//"b", "ab", "result should be 'ab' when input 'a', 'b'", stat)
    ! ! PASSED: result should be 1 when input 0+1'
    !
    ! call expect_equal("b"//"a", "ab", "result should be 'ab' when input 'a', 'b'", stat)
    ! ! FAILED: result should be 1 when input 0+1
    ! !     Expected: ab
    ! !     Actual  : ba
    !```

end program char
