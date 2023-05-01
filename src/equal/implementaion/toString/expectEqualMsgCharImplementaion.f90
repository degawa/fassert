submodule(expectEqual) expectEqualMsg_char_implementaion
contains
    !>実測値`actual`と予測値`expected`が同じ文字列かを比較し，
    !>出力を`output_message`に書き込む．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equal_char_msg
    use :: fassert_common_check
    implicit none

    logical :: same_length_stat, is_same_string

    ! 値比較の前に文字列の長さを比較し，異なっていれば失敗とする．
    block
        character(:), allocatable :: length_check_msg
        call expect_equal(len(actual), len(expected), test_name//note_length_check, same_length_stat, &
                          verbose=verbose, quiet=quiet, output_message=length_check_msg)
        if (is_test_failed(same_length_stat)) then
            stat = failed
            call write_message(length_check_msg, output_message)
            return
        end if
    end block

    ! 同じ長さかつ文字列の各要素が同じ
    is_same_string = same_length_stat .and. (actual == expected)

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(is_same_string, test_name, stat, quiet, output_message)
    else
        call check_true(is_same_string, test_name, stat, quiet, output_message)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected, output_message)
    end procedure expect_equal_char_msg

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較し，
    !>出力を`output_message`に書き込む．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    module procedure expect_equal_char_rank1_msg
    use :: expectSameShape
    use :: fassert_common_compare_equal_array
    implicit none

    logical :: has_same_values

    ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
    block
        logical :: array_shape_stat
        character(:), allocatable :: shape_check_msg

        call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                               verbose=verbose, quiet=quiet, output_message=shape_check_msg)
        if (is_test_failed(array_shape_stat)) then
            stat = failed
            call write_message(shape_check_msg, output_message)
            return
        end if
    end block

    ! 各要素の値を比較
    has_same_values = are_equal(actual, expected)

    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(has_same_values, test_name, stat, quiet, output_message)
    else
        call check_true(has_same_values, test_name, stat, quiet, output_message)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected, output_message)
    end procedure expect_equal_char_rank1_msg
end submodule expectEqualMsg_char_implementaion
