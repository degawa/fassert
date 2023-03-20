submodule(expectEqual) expectEqual_implementaion
contains
    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equal_int8
    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(actual == expected, test_name, stat, quiet)
    else
        call check_true(actual == expected, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_equal_int8

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equal_int16
    implicit none
    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(actual == expected, test_name, stat, quiet)
    else
        call check_true(actual == expected, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_equal_int16

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equal_int32
    implicit none
    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(actual == expected, test_name, stat, quiet)
    else
        call check_true(actual == expected, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_equal_int32

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equal_int64
    implicit none
    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(actual == expected, test_name, stat, quiet)
    else
        call check_true(actual == expected, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_equal_int64

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equal_int32_rank1
    use :: expectSameShape
    use :: fassert_common_compare_equal_array
    implicit none

    logical :: array_shape_stat, array_values_stat

    ! 値比較の前に配列要素数を比較
    call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                           quiet=optval(quiet, default_quiet_shape_check))
    if (is_test_failed(array_shape_stat)) then
        stat = failed
        return
    end if

    ! 各要素の値を比較
    if (are_equal(actual, expected)) then
        array_values_stat = passed
    else
        array_values_stat = failed
    end if

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(array_values_stat, test_name, stat, quiet)
    else
        call check_true(array_values_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_equal_int32_rank1

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equal_int32_rank2
    use :: expectSameShape
    use :: fassert_common_compare_equal_array
    implicit none

    logical :: array_shape_stat, array_values_stat

    ! 値比較の前に配列要素数を比較
    call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                           quiet=optval(quiet, default_quiet_shape_check))
    if (is_test_failed(array_shape_stat)) then
        stat = failed
        return
    end if

    ! 各要素の値を比較
    if (are_equal(actual, expected)) then
        array_values_stat = passed
    else
        array_values_stat = failed
    end if

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(array_values_stat, test_name, stat, quiet)
    else
        call check_true(array_values_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_equal_int32_rank2

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equal_int32_rank3
    use :: expectSameShape
    use :: fassert_common_compare_equal_array
    implicit none

    logical :: array_shape_stat, array_values_stat

    ! 値比較の前に配列要素数を比較
    call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                           quiet=optval(quiet, default_quiet_shape_check))
    if (is_test_failed(array_shape_stat)) then
        stat = failed
        return
    end if

    ! 各要素の値を比較
    if (are_equal(actual, expected)) then
        array_values_stat = passed
    else
        array_values_stat = failed
    end if

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(array_values_stat, test_name, stat, quiet)
    else
        call check_true(array_values_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_equal_int32_rank3

    !>実測値`actual`と予測値`expected`の差が
    !>許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approxequal_real32
    use :: fassert_common_compare_equal_real
    use :: fassert_common_optval
    implicit none

    logical :: val_stat

    ! 二つの値の差が許容範囲内かを比較
    val_stat = is_approx_equal(actual, expected, tolerance)

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(val_stat, test_name, stat, quiet)
    else
        call check_true(val_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approxequal_real32

    !>実測値`actual`と予測値`expected`の差が
    !>許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_approxequal_real64
    use :: fassert_common_compare_equal_real
    use :: fassert_common_optval
    implicit none

    logical :: val_stat

    ! 二つの値の差が許容範囲内かを比較
    val_stat = is_approx_equal(actual, expected, tolerance)

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(val_stat, test_name, stat, quiet)
    else
        call check_true(val_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approxequal_real64

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
    module procedure expect_approxequal_real32_rank1
    use :: expectSameShape
    use :: fassert_common_compare_equal_array
    use :: fassert_common_optval
    implicit none

    logical :: array_shape_stat, array_values_stat

    call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                           quiet=optval(quiet, default_quiet_shape_check))
    if (is_test_failed(array_shape_stat)) then
        stat = failed
        return
    end if

    ! 各要素の値を比較
    if (are_equal(actual, expected, tolerance)) then
        array_values_stat = passed
    else
        array_values_stat = failed
    end if

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(array_values_stat, test_name, stat, quiet)
    else
        call check_true(array_values_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approxequal_real32_rank1

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
    module procedure expect_approxequal_real32_rank2
    use :: expectSameShape
    use :: fassert_common_compare_equal_array
    use :: fassert_common_optval
    implicit none

    logical :: array_shape_stat, array_values_stat

    call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                           quiet=optval(quiet, default_quiet_shape_check))
    if (is_test_failed(array_shape_stat)) then
        stat = failed
        return
    end if

    ! 各要素の値を比較
    if (are_equal(actual, expected, tolerance)) then
        array_values_stat = passed
    else
        array_values_stat = failed
    end if

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(array_values_stat, test_name, stat, quiet)
    else
        call check_true(array_values_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approxequal_real32_rank2

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
    module procedure expect_approxequal_real32_rank3
    use :: expectSameShape
    use :: fassert_common_compare_equal_array
    use :: fassert_common_optval
    implicit none

    logical :: array_shape_stat, array_values_stat

    call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                           quiet=optval(quiet, default_quiet_shape_check))
    if (is_test_failed(array_shape_stat)) then
        stat = failed
        return
    end if

    ! 各要素の値を比較
    if (are_equal(actual, expected, tolerance)) then
        array_values_stat = passed
    else
        array_values_stat = failed
    end if

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(array_values_stat, test_name, stat, quiet)
    else
        call check_true(array_values_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approxequal_real32_rank3

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
    module procedure expect_approxequal_real64_rank1
    use :: expectSameShape
    use :: fassert_common_compare_equal_array
    use :: fassert_common_optval
    implicit none

    logical :: array_shape_stat, array_values_stat

    call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                           quiet=optval(quiet, default_quiet_shape_check))
    if (is_test_failed(array_shape_stat)) then
        stat = failed
        return
    end if

    ! 各要素の値を比較
    if (are_equal(actual, expected, tolerance)) then
        array_values_stat = passed
    else
        array_values_stat = failed
    end if

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(array_values_stat, test_name, stat, quiet)
    else
        call check_true(array_values_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approxequal_real64_rank1

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
    module procedure expect_approxequal_real64_rank2
    use :: expectSameShape
    use :: fassert_common_compare_equal_array
    use :: fassert_common_optval
    implicit none

    logical :: array_shape_stat, array_values_stat

    call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                           quiet=optval(quiet, default_quiet_shape_check))
    if (is_test_failed(array_shape_stat)) then
        stat = failed
        return
    end if

    ! 各要素の値を比較
    if (are_equal(actual, expected, tolerance)) then
        array_values_stat = passed
    else
        array_values_stat = failed
    end if

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(array_values_stat, test_name, stat, quiet)
    else
        call check_true(array_values_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approxequal_real64_rank2

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
    module procedure expect_approxequal_real64_rank3
    use :: expectSameShape
    use :: fassert_common_compare_equal_array
    use :: fassert_common_optval
    implicit none

    logical :: array_shape_stat, array_values_stat

    call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                           quiet=optval(quiet, default_quiet_shape_check))
    if (is_test_failed(array_shape_stat)) then
        stat = failed
        return
    end if

    ! 各要素の値を比較
    if (are_equal(actual, expected, tolerance)) then
        array_values_stat = passed
    else
        array_values_stat = failed
    end if

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(array_values_stat, test_name, stat, quiet)
    else
        call check_true(array_values_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_approxequal_real64_rank3

    !>実測値`actual`と予測値`expected`が等価かを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equiv_logical
    use :: fassert_common_check
    implicit none

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(actual .eqv. expected, test_name, stat, quiet)
    else
        call check_true(actual .eqv. expected, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_equiv_logical

    !>実測値`actual`と予測値`expected`が同じ文字列かを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equal_str
    use :: fassert_common_check
    implicit none

    logical :: is_same_length, string_stat

    ! 文字列の長さを比較
    call expect_equal(len(actual), len(expected), test_name//note_length_check, is_same_length, &
                      quiet=optval(quiet, default=default_quiet_shape_check))
    if (.not. is_same_length) then
        stat = failed
        return
    end if

    ! 同じ長さかつ文字列の各要素が同じ
    string_stat = is_same_length .and. (actual == expected)

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(string_stat, test_name, stat, quiet)
    else
        call check_true(string_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_equal_str

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equal_char_rank1
    use :: expectSameShape
    use :: fassert_common_compare_equal_array
    implicit none

    logical :: array_shape_stat, array_values_stat

    call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                           quiet=optval(quiet, default_quiet_shape_check))
    if (is_test_failed(array_shape_stat)) then
        stat = failed
        return
    end if

    ! 各要素の値を比較
    if (are_equal(actual, expected)) then
        array_values_stat = passed
    else
        array_values_stat = failed
    end if

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(array_values_stat, test_name, stat, quiet)
    else
        call check_true(array_values_stat, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_equal_char_rank1
end submodule expectEqual_implementaion
