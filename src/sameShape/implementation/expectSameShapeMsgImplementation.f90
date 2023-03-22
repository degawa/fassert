submodule(expectSameShape) expectSameShapeMsg_implementation
    use :: fassert_common_compare_equal_shape
    use :: fassert_common_message
    use :: fassert_common_message_outputOnFailure_toString_shape
contains
    !>二つの配列が同じ形状かを検査し，出力を`output_message`に書き込む．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    module procedure expect_same_shape_rank1_msg
    implicit none
    call check_true(are_same_shape(actual, expected), test_name, stat, quiet, output_message)
    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected, output_message)
    end procedure expect_same_shape_rank1_msg

    !>二つの配列が同じ形状かを検査し，出力を`output_message`に書き込む．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    module procedure expect_same_shape_rank2_msg
    implicit none
    call check_true(are_same_shape(actual, expected), test_name, stat, quiet, output_message)
    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected, output_message)
    end procedure expect_same_shape_rank2_msg

    !>二つの配列が同じ形状かを検査し，出力を`output_message`に書き込む．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    module procedure expect_same_shape_rank3_msg
    implicit none
    call check_true(are_same_shape(actual, expected), test_name, stat, quiet, output_message)
    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected, output_message)
    end procedure expect_same_shape_rank3_msg
end submodule expectSameShapeMsg_implementation
