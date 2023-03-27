module test_common_message_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_message_unitTests_toString
    use :: test_common_message_unitTests_output
    use :: test_common_message_unitTests_write
    implicit none
    private
    public :: collect_message

contains
    subroutine collect_message(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("to_string_logical(), parameterized test.", &
                                  toString_parameterized_test) &
                     , new_unittest("does_output_message(), parameterized test.", &
                                    doesOutputMsg_parameterized_test) &
                     , new_unittest("does_not_output_message(), parameterized test.", &
                                    doesNotOutputMsg_parameterized_test) &
                     , new_unittest("is_verbose_output(), parameterized test.", &
                                    isVerboseOutput_parameterized_test) &
                     , new_unittest("write_message_to_string(), it should write `message` to `output_message` "// &
                                    "when `message` is allocated.", &
                                    writeMsgToString_should_write_msg_to_str_when_allocated) &
                     , new_unittest("write_message_to_string(), it should not write `message` to `output_message` "// &
                                    "when `message` is not allocated.", &
                                    writeMsgToString_should_not_write_msg_to_str_when_not_allocated) &
                     , new_unittest("write_message_to_unit(), it should write `message` to unit "// &
                                    "when `message` is allocated.", &
                                    writeMsgToUnit_should_write_msg_to_unit_when_allocated) &
                     , new_unittest("write_message_to_unit(), it should not write `message` to unit "// &
                                    "when `message` is not allocated.", &
                                    writeMsgToUnit_should_not_write_msg_to_unit_when_not_allocated) &
                     ]
    end subroutine collect_message
end module test_common_message_collection
