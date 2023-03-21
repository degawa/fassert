submodule(expectLogical) expectLogicalMsg_implementaion
contains
    !>実測値`actual`が真であるか検査し，
    !>出力を`output_message`に書き込む．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，実測値が偽であることを検査する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    module procedure expect_true_msg
    use :: fassert_common_message_outputOnFailure_toUnit_equal
    implicit none

    logical :: expected

    if (is_test_of_expected_failure(expected_failure)) then
        expected = .false.
        call check_expected_failure(actual, test_name, stat, quiet, output_message)
    else
        expected = .true.
        call check_true(actual, test_name, stat, quiet, output_message)
    end if

    if (is_verbose_output(stat, verbose, quiet)) &
        call output_on_failure(actual, expected, output_message)
    end procedure expect_true_msg

    !>実測値`actual`が真であるか検査し，
    !>出力を`output_message`に書き込む．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，実測値が真であることを検査する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    module procedure expect_false_msg
    use :: fassert_common_message_outputOnFailure_toUnit_equal
    implicit none

    logical :: expected

    if (is_test_of_expected_failure(expected_failure)) then
        expected = .true.
        call check_expected_failure(.not. actual, test_name, stat, quiet, output_message)
    else
        expected = .false.
        call check_true(.not. actual, test_name, stat, quiet, output_message)
    end if

    if (is_verbose_output(stat, verbose, quiet)) &
        call output_on_failure(actual, expected, output_message)
    end procedure expect_false_msg
end submodule expectLogicalMsg_implementaion
