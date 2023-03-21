submodule(expectSameShape) expectSameShape_implementation
    use :: fassert_common_compare_equal_shape
    use :: fassert_common_message
    use :: fassert_common_message_outputOnFailure_toUnit_shape
contains
    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    module procedure expect_same_shape_rank1
    implicit none
    call check_true(are_same_shape(actual, expected), test_name, stat, quiet)
    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_same_shape_rank1

    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    module procedure expect_same_shape_rank2
    implicit none
    call check_true(are_same_shape(actual, expected), test_name, stat, quiet)
    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_same_shape_rank2

    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    module procedure expect_same_shape_rank3
    implicit none
    call check_true(are_same_shape(actual, expected), test_name, stat, quiet)
    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end procedure expect_same_shape_rank3
end submodule expectSameShape_implementation
