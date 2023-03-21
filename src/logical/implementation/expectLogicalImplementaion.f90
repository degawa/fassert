submodule(expectLogical) expectLogical_implementaion
contains
    !>実測値`actual`が真であるか検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，実測値が偽であることを検査する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    module procedure expect_true_
    use :: fassert_common_message_outputOnFailure_toUnit_equal
    implicit none

    logical :: expected

    if (is_test_of_expected_failure(expected_failure)) then
        expected = .false.
        call check_expected_failure(actual, test_name, stat, quiet)
    else
        expected = .true.
        call check_true(actual, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_true_

    !>実測値`actual`が偽であるか検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，実測値が真であることを検査する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    module procedure expect_false_
    use :: fassert_common_message_outputOnFailure_toUnit_equal
    implicit none

    logical :: expected

    if (is_test_of_expected_failure(expected_failure)) then
        expected = .true.
        call check_expected_failure(.not. actual, test_name, stat, quiet)
    else
        expected = .false.
        call check_true(.not. actual, test_name, stat, quiet)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_false_
end submodule expectLogical_implementaion
