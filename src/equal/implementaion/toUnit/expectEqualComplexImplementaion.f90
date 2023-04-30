submodule(expectEqual) expectEqual_complex_implementaion
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
    module procedure expect_approxequal_complex32
    use :: fassert_common_compare_equal_complex
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
    end procedure expect_approxequal_complex32

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
    module procedure expect_approxequal_complex64
    use :: fassert_common_compare_equal_complex
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
    end procedure expect_approxequal_complex64
end submodule expectEqual_complex_implementaion
