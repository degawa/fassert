module test_common_message_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_common_message_unitTests_toString
    implicit none
    private
    public :: collect_message

contains
    subroutine collect_message(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("to_string_logical(), it should return 'T' when input `.true.`.", &
                                  toString_should_return_T_when_input_true) &
                     , new_unittest("to_string_logical(), it should return 'F' when input `.false.`.", &
                                    toString_should_return_F_when_input_false) &
                     ]
    end subroutine collect_message
end module test_common_message_collection
