submodule(expectEqual) expectEqual_logical_implementaion
contains
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
end submodule expectEqual_logical_implementaion
