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
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toUnit_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(is_true(actual), test_name, stat, quiet)
        else
            call check_true(is_true(actual), test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_true_failure(actual)
    end procedure expect_true_

    module procedure expect_true_rank1
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toUnit_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_true(actual), test_name, stat, quiet)
        else
            call check_true(are_true(actual), test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_true_failure(actual)
    end procedure expect_true_rank1

    module procedure expect_true_rank2
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toUnit_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_true(actual), test_name, stat, quiet)
        else
            call check_true(are_true(actual), test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_true_failure(actual)
    end procedure expect_true_rank2

    module procedure expect_true_rank3
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toUnit_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_true(actual), test_name, stat, quiet)
        else
            call check_true(are_true(actual), test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_true_failure(actual)
    end procedure expect_true_rank3

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
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toUnit_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(is_false(actual), test_name, stat, quiet)
        else
            call check_true(is_false(actual), test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_false_failure(actual)
    end procedure expect_false_

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
    module procedure expect_false_rank1
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toUnit_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_false(actual), test_name, stat, quiet)
        else
            call check_true(are_false(actual), test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_false_failure(actual)
    end procedure expect_false_rank1

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
    module procedure expect_false_rank2
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toUnit_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_false(actual), test_name, stat, quiet)
        else
            call check_true(are_false(actual), test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_false_failure(actual)
    end procedure expect_false_rank2

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
    module procedure expect_false_rank3
        use :: fassert_common_compare_logical
        use :: fassert_common_message_outputOnFailure_toUnit_logical
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(are_false(actual), test_name, stat, quiet)
        else
            call check_true(are_false(actual), test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) &
            call output_on_expect_false_failure(actual)
    end procedure expect_false_rank3

end submodule expectLogical_implementaion
