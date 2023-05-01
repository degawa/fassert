program array_real
    use, intrinsic :: iso_fortran_env
    use :: fassert
    implicit none

    logical :: stat

    call expect_equal([real(real64) :: 0, 0, 0, 0, 0], [real(real64) :: 0, 0, 0, 0, 0], &
                      "result should be [0. 0. 0. 0. 0.] when input [1. 2. 3. 4. 5.]", stat)
    call expect_equal([real(real64) :: 1, 2, 3, 4, 5], [real(real64) :: 0, 0, 0, 0, 0], &
                      "result should be [0. 0. 0. 0. 0.] when input [1. 2. 3. 4. 5.]", stat)
    !|
    !```Fortran
    ! call expect_equal([real(real64) :: 0, 0, 0, 0, 0], [real(real64) :: 0, 0, 0, 0, 0], &
    !                   "result should be [0. 0. 0. 0. 0.] when input [1. 2. 3. 4. 5.]", stat)
    ! ! PASSED: result should be [0. 0. 0. 0. 0.] when input [1. 2. 3. 4. 5.]
    !
    ! call expect_equal([real(real64) :: 1, 2, 3, 4, 5], [real(real64) :: 0, 0, 0, 0, 0], &
    !                   "result should be [0. 0. 0. 0. 0.] when input [1. 2. 3. 4. 5.]", stat)
    ! ! FAILED: result should be [0. 0. 0. 0. 0.] when input [1. 2. 3. 4. 5.]
    ! !     Maximum Absolute Difference:  0.500000000000000E+001 at (5)
    ! !     Minimum Absolute Difference:  0.100000000000000E+001 at (1)
    !```

    call expect_equal(reshape([real(real64) :: 0, 0, 0, 0, 0, 0], [2, 3]), &
                      reshape([real(real64) :: 0, 0, 0, 0, 0, 0], [2, 3]), &
                      "result should be [[0. 0.] [0. 0.] [0. 0.]] when input [[1. 2.] [3. 4.] [5. 6.]]", stat)
    call expect_equal(reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [2, 3]), &
                      reshape([real(real64) :: 0, 0, 0, 0, 0, 0], [2, 3]), &
                      "result should be [[0. 0.] [0. 0.] [0. 0.]] when input [[1. 2.] [3. 4.] [5. 6.]]", stat)
    !|
    !```Fortran
    ! call expect_equal(reshape([real(real64) :: 0, 0, 0, 0, 0, 0], [2, 3]), &
    !                   reshape([real(real64) :: 0, 0, 0, 0, 0, 0], [2, 3]), &
    !                   "result should be [[0. 0.] [0. 0.] [0. 0.]] when input [[1. 2.] [3. 4.] [5. 6.]]", stat)
    ! ! PASSED: result should be [[0. 0.] [0. 0.] [0. 0.]] when input [[1. 2.] [3. 4.] [5. 6.]]
    !
    ! call expect_equal(reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [2, 3]), &
    !                   reshape([real(real64) :: 0, 0, 0, 0, 0, 0], [2, 3]), &
    !                   "result should be [[0. 0.] [0. 0.] [0. 0.]] when input [[1. 2.] [3. 4.] [5. 6.]]", stat)
    ! ! FAILED: result should be [[0. 0.] [0. 0.] [0. 0.]] when input [[1. 2.] [3. 4.] [5. 6.]]
    ! !     Maximum Absolute Difference:  0.600000000000000E+001 at (2,3)
    ! !     Minimum Absolute Difference:  0.100000000000000E+001 at (1,1)
    !```
end program array_real
