submodule(expectApproxEqual) expectApproxEqual_complex_implementaion
contains
    !>実測値`actual`と予測値`expected`の差が
    !>許容値`tolerance`より小さいかを比較し，
    !>出力を`output_message`に書き込む．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex32
        use :: fassert_common_compare_approx_complex
        implicit none

        logical :: has_same_value

        ! 二つの値の差が許容範囲内かを比較
        has_same_value = is_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_value, test_name, stat, quiet)
        else
            call check_true(has_same_value, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex32

    !>実測値`actual`と予測値`expected`の差が
    !>許容値`tolerance`より小さいかを比較し，
    !>出力を`output_message`に書き込む．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex64
        use :: fassert_common_compare_approx_complex
        implicit none

        logical :: has_same_value

        ! 二つの値の差が許容範囲内かを比較
        has_same_value = is_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_value, test_name, stat, quiet)
        else
            call check_true(has_same_value, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex64

    !>実測値`actual`と予測値`expected`の差が
    !>許容値`tolerance`より小さいかを比較し，
    !>出力を`output_message`に書き込む．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex128
        use :: fassert_common_compare_approx_complex
        implicit none

        logical :: has_same_value

        ! 二つの値の差が許容範囲内かを比較
        has_same_value = is_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_value, test_name, stat, quiet)
        else
            call check_true(has_same_value, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex128

    !>実測値`actual`と予測値`expected`の各要素の差が
    !>許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex32_rank1
        use :: expectSameShape
        use :: fassert_common_compare_approx_array
        implicit none

        logical :: has_same_values

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        ! 各要素の値を比較
        has_same_values = are_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_values, test_name, stat, quiet)
        else
            call check_true(has_same_values, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex32_rank1

    !>実測値`actual`と予測値`expected`の各要素の差が
    !>許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex32_rank2
        use :: expectSameShape
        use :: fassert_common_compare_approx_array
        implicit none

        logical :: has_same_values

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        ! 各要素の値を比較
        has_same_values = are_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_values, test_name, stat, quiet)
        else
            call check_true(has_same_values, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex32_rank2

    !>実測値`actual`と予測値`expected`の各要素の差が
    !>許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex32_rank3
        use :: expectSameShape
        use :: fassert_common_compare_approx_array
        implicit none

        logical :: has_same_values

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        ! 各要素の値を比較
        has_same_values = are_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_values, test_name, stat, quiet)
        else
            call check_true(has_same_values, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex32_rank3

    !>実測値`actual`と予測値`expected`の各要素の差が
    !>許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex64_rank1
        use :: expectSameShape
        use :: fassert_common_compare_approx_array
        implicit none

        logical :: has_same_values

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        ! 各要素の値を比較
        has_same_values = are_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_values, test_name, stat, quiet)
        else
            call check_true(has_same_values, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex64_rank1

    !>実測値`actual`と予測値`expected`の各要素の差が
    !>許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex64_rank2
        use :: expectSameShape
        use :: fassert_common_compare_approx_array
        implicit none

        logical :: has_same_values

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        ! 各要素の値を比較
        has_same_values = are_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_values, test_name, stat, quiet)
        else
            call check_true(has_same_values, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex64_rank2

    !>実測値`actual`と予測値`expected`の各要素の差が
    !>許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex64_rank3
        use :: expectSameShape
        use :: fassert_common_compare_approx_array
        implicit none

        logical :: has_same_values

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        ! 各要素の値を比較
        has_same_values = are_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_values, test_name, stat, quiet)
        else
            call check_true(has_same_values, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex64_rank3

    !>実測値`actual`と予測値`expected`の各要素の差が
    !>許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex128_rank1
        use :: expectSameShape
        use :: fassert_common_compare_approx_array
        implicit none

        logical :: has_same_values

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        ! 各要素の値を比較
        has_same_values = are_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_values, test_name, stat, quiet)
        else
            call check_true(has_same_values, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex128_rank1

    !>実測値`actual`と予測値`expected`の各要素の差が
    !>許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex128_rank2
        use :: expectSameShape
        use :: fassert_common_compare_approx_array
        implicit none

        logical :: has_same_values

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        ! 各要素の値を比較
        has_same_values = are_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_values, test_name, stat, quiet)
        else
            call check_true(has_same_values, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex128_rank2

    !>実測値`actual`と予測値`expected`の各要素の差が
    !>許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approx_equal_complex128_rank3
        use :: expectSameShape
        use :: fassert_common_compare_approx_array
        implicit none

        logical :: has_same_values

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        ! 各要素の値を比較
        has_same_values = are_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_values, test_name, stat, quiet)
        else
            call check_true(has_same_values, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approx_equal_complex128_rank3

end submodule expectApproxEqual_complex_implementaion
