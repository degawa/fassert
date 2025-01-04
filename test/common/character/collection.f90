module test_common_character_collection
    use :: testdrive, only:new_unittest, unittest_type, to_string
    use :: test_common_character_unitTests_alphabeticalCharacter
    use :: test_common_character_unitTests_toUpper
    implicit none
    private
    public :: collect_char

contains
    subroutine collect_char(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("`lower_case` should have 26 alphabetical lower case characters", &
                                  lower_cases_have_26_alphabetical_characters) &
                     , new_unittest("`upper_case` should have 26 alphabetical upper case characters", &
                                    upper_cases_have_26_alphabetical_characters) &
                     , new_unittest("to_upper() should convert lower case characters to corresponding upper case characters", &
                                    to_upper_converts_lower_case_to_upper_case) &
                     ]
    end subroutine collect_char
end module test_common_character_collection
