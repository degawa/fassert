submodule(expectEqual) expectEqualMsg_implementaion
contains
    module procedure expect_equal_int32_msg
    if (is_test_of_expected_failure(expected_failure)) then
        call check_expected_failure(actual == expected, test_name, stat, quiet, output_message)
    else
        call check_true(actual == expected, test_name, stat, quiet, output_message)
    end if

    if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected, output_message)
    end procedure expect_equal_int32_msg
end submodule expectEqualMsg_implementaion
