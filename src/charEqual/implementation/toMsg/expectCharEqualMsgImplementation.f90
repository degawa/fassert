submodule(expectCharEqual) expectCharEqualMsg_implementaion
contains
    !>実測値`actual`と予測値`expected`が同じ文字列かを検査し，
    !>出力を`output_message`に書き込む．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_char_equal_msg
        use :: fassert_common_check
        implicit none
        logical :: is_same_string
        logical :: case_insensitive

        case_insensitive = optval(ignore_case, .false.)

        if (case_insensitive) then
            is_same_string = is_equal(actual, expected, ignore_case=case_insensitive, check_len=.false.)
        else
            is_same_string = (actual == expected)
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(is_same_string, test_name, stat, quiet)
        else
            call check_true(is_same_string, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected, output_message)
    end procedure expect_char_equal_msg

    !>実測値`actual`と予測値`expected`が同じ文字列かを検査し，
    !>出力を`output_message`に書き込む．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_char_equal_rank1_msg
        use :: fassert_common_check
        implicit none
        logical :: is_same_string
        logical :: case_insensitive

        case_insensitive = optval(ignore_case, .false.)

        if (case_insensitive) then
            is_same_string = all(is_equal(actual, expected, ignore_case=case_insensitive, check_len=.false.))
        else
            is_same_string = all(actual == expected)
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(is_same_string, test_name, stat, quiet)
        else
            call check_true(is_same_string, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected, output_message)
    end procedure expect_char_equal_rank1_msg

    !>実測値`actual`と予測値`expected`が同じ文字列かを検査し，
    !>出力を`output_message`に書き込む．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_char_equal_rank2_msg
        use :: fassert_common_check
        implicit none
        logical :: is_same_string
        logical :: case_insensitive

        case_insensitive = optval(ignore_case, .false.)

        if (case_insensitive) then
            is_same_string = all(is_equal(actual, expected, ignore_case=case_insensitive, check_len=.false.))
        else
            is_same_string = all(actual == expected)
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(is_same_string, test_name, stat, quiet)
        else
            call check_true(is_same_string, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected, output_message)
    end procedure expect_char_equal_rank2_msg

    !>実測値`actual`と予測値`expected`が同じ文字列かを検査し，
    !>出力を`output_message`に書き込む．
    !>文字列長さの検査は行わず，末尾の空白は無視される．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`ignore_case`が渡されていれば，英字の大小を無視して比較する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_char_equal_rank3_msg
        use :: fassert_common_check
        implicit none
        logical :: is_same_string
        logical :: case_insensitive

        case_insensitive = optval(ignore_case, .false.)

        if (case_insensitive) then
            is_same_string = all(is_equal(actual, expected, ignore_case=case_insensitive, check_len=.false.))
        else
            is_same_string = all(actual == expected)
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(is_same_string, test_name, stat, quiet)
        else
            call check_true(is_same_string, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected, output_message)
    end procedure expect_char_equal_rank3_msg
end submodule expectCharEqualMsg_implementaion