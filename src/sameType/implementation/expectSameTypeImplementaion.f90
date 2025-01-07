submodule(expectSameType) expectSameType_implementaion
contains
    !>実測値`actual`と予測値`expected`の型が同一かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，型が同一でないことを検査する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    module procedure expect_same_type_
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(same_type_as(actual, expected), test_name, stat, quiet)
        else
            call check_true(same_type_as(actual, expected), test_name, stat, quiet)
        end if
    end procedure expect_same_type_

    !>実測値`actual`と予測値`expected`の型が同一かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，型が同一でないことを検査する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    module procedure expect_same_type_rank1
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(same_type_as(actual, expected), test_name, stat, quiet)
        else
            call check_true(same_type_as(actual, expected), test_name, stat, quiet)
        end if
    end procedure expect_same_type_rank1

    !>実測値`actual`と予測値`expected`の型が同一かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，型が同一でないことを検査する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    module procedure expect_same_type_rank2
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(same_type_as(actual, expected), test_name, stat, quiet)
        else
            call check_true(same_type_as(actual, expected), test_name, stat, quiet)
        end if
    end procedure expect_same_type_rank2

    !>実測値`actual`と予測値`expected`の型が同一かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，型が同一でないことを検査する．
    !>
    !>`quiet`が真の場合，出力を抑制する．
    !>
    module procedure expect_same_type_rank3
        implicit none

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(same_type_as(actual, expected), test_name, stat, quiet)
        else
            call check_true(same_type_as(actual, expected), test_name, stat, quiet)
        end if
    end procedure expect_same_type_rank3
end submodule expectSameType_implementaion
