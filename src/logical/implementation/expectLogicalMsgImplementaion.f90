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
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toString_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(is_true(actual), test_name, stat, quiet, output_message)
        else
            call check_true(is_true(actual), test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_true_failure(actual, output_message)
    end procedure expect_true_msg

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
    module procedure expect_true_rank1_msg
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toString_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_true(actual), test_name, stat, quiet, output_message)
        else
            call check_true(are_true(actual), test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_true_failure(actual, output_message)
    end procedure expect_true_rank1_msg

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
    module procedure expect_true_rank2_msg
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toString_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_true(actual), test_name, stat, quiet, output_message)
        else
            call check_true(are_true(actual), test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_true_failure(actual, output_message)
    end procedure expect_true_rank2_msg

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
    module procedure expect_true_rank3_msg
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toString_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_true(actual), test_name, stat, quiet, output_message)
        else
            call check_true(are_true(actual), test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_true_failure(actual, output_message)
    end procedure expect_true_rank3_msg

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
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toString_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(is_false(actual), test_name, stat, quiet, output_message)
        else
            call check_true(is_false(actual), test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_false_failure(actual, output_message)
    end procedure expect_false_msg

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
    module procedure expect_false_rank1_msg
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toString_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_false(actual), test_name, stat, quiet, output_message)
        else
            call check_true(are_false(actual), test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_false_failure(actual, output_message)
    end procedure expect_false_rank1_msg

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
    module procedure expect_false_rank2_msg
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toString_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_false(actual), test_name, stat, quiet, output_message)
        else
            call check_true(are_false(actual), test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_false_failure(actual, output_message)
    end procedure expect_false_rank2_msg

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
    module procedure expect_false_rank3_msg
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toString_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_false(actual), test_name, stat, quiet, output_message)
        else
            call check_true(are_false(actual), test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_false_failure(actual, output_message)
    end procedure expect_false_rank3_msg

end submodule expectLogicalMsg_implementaion
