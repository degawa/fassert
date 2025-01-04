module test_charEqual_expectCharEqual_unitTests_expect
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: fassert_common_message, only:NL
    use :: expectCharEqual
    use :: test_charEqual_expectCharEqual_unitTests_common
    implicit none
    private
    public :: char_equal_tests_success_cases
    public :: char_equal_tests_failure_cases
    public :: char_equal_tests_expected_failure_cases

contains
    subroutine char_equal_tests_success_cases(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        integer(int32) :: message_output_unit_number
        logical :: stat
        character(:), allocatable :: expected, test_name

        logical, parameter :: stat_exp = .true.

        test_name = "a unit test"
        call before_all(message_output_unit_number)

        !-v expected_char_equal(actual,expected,name,stat)
        call setup(message_output_unit_number)
        call expect_char_equal("1aBcdE", "1aBcdE", test_name, stat)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("2aBcdE ", "2aBcdE", test_name, stat)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F)
        call setup(message_output_unit_number)
        call expect_char_equal("3AbCDe", "3AbCDe", test_name, stat, ignore_case=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("4AbCDe  ", "4AbCDe", test_name, stat, ignore_case=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T)
        call setup(message_output_unit_number)
        call expect_char_equal("5ABcdE", "5aBcDe", test_name, stat, ignore_case=.true.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("6ABcdE ", "6aBcDe", test_name, stat, ignore_case=.true.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=F)
        call setup(message_output_unit_number)
        call expect_char_equal("7AbCDe", "7AbCDe", test_name, stat, verbose=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("8AbCDe   ", "8AbCDe", test_name, stat, verbose=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=F)
        call setup(message_output_unit_number)
        call expect_char_equal("9AbCDefg", "9AbCDefg", test_name, stat, ignore_case=.false., verbose=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("10AbCDefg    ", "10AbCDefg", test_name, stat, ignore_case=.false., verbose=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=F)
        call setup(message_output_unit_number)
        call expect_char_equal("11abcdefg", "11AbCDefg", test_name, stat, ignore_case=.true., verbose=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("12ABCDEFG    ", "12AbCDefg", test_name, stat, ignore_case=.true., verbose=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=F, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("13AbCDe", "13AbCDe", test_name, stat, verbose=.false., quiet=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("14AbCDe   ", "14AbCDe", test_name, stat, verbose=.false., quiet=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=F, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("15AbCDefg", "15AbCDefg", test_name, stat, ignore_case=.false., verbose=.false., quiet=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("16AbCDefg    ", "16AbCDefg", test_name, stat, ignore_case=.false., verbose=.false., quiet=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=F, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("17abcdefg", "17AbCDefg", test_name, stat, ignore_case=.true., verbose=.false., quiet=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("18ABCDEFG    ", "18AbCDefg", test_name, stat, ignore_case=.true., verbose=.false., quiet=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=F, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("19AbCDe", "19AbCDe", test_name, stat, verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("20AbCDe   ", "20AbCDe", test_name, stat, verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=F, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("21AbCDefg", "21AbCDefg", test_name, stat, ignore_case=.false., verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("22AbCDefg    ", "22AbCDefg", test_name, stat, ignore_case=.false., verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=F, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("23abcdefg", "23AbCDefg", test_name, stat, ignore_case=.true., verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("24ABCDEFG    ", "24AbCDefg", test_name, stat, ignore_case=.true., verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=T)
        call setup(message_output_unit_number)
        call expect_char_equal("25aBcdE", "25aBcdE", test_name, stat, verbose=.true.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 25aBcdE"//NL// &
            "    Actual  : 25aBcdE"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("26aBcdE ", "26aBcdE", test_name, stat, verbose=.true.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 26aBcdE"//NL// &
            "    Actual  : 26aBcdE"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=T)
        call setup(message_output_unit_number)
        call expect_char_equal("27aBcdE", "27aBcdE", test_name, stat, ignore_case=.false., verbose=.true.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 27aBcdE"//NL// &
            "    Actual  : 27aBcdE"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("28aBcdE ", "28aBcdE", test_name, stat, ignore_case=.false., verbose=.true.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 28aBcdE"//NL// &
            "    Actual  : 28aBcdE"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=T)
        call setup(message_output_unit_number)
        call expect_char_equal("29AbCDe", "29aBcdE", test_name, stat, ignore_case=.true., verbose=.true.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 29aBcdE"//NL// &
            "    Actual  : 29AbCDe"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("30ABCde ", "30aBcdE", test_name, stat, ignore_case=.true., verbose=.true.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 30aBcdE"//NL// &
            "    Actual  : 30ABCde"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=T, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("31AbCDefg", "31AbCDefg", test_name, stat, verbose=.true., quiet=.false.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 31AbCDefg"//NL// &
            "    Actual  : 31AbCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("32AbCDefg    ", "32AbCDefg", test_name, stat, verbose=.true., quiet=.false.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 32AbCDefg"//NL// &
            "    Actual  : 32AbCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=T, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("33AbCDefg", "33AbCDefg", test_name, stat, ignore_case=.false., verbose=.true., quiet=.false.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 33AbCDefg"//NL// &
            "    Actual  : 33AbCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("34AbCDefg    ", "34AbCDefg", test_name, stat, ignore_case=.false., verbose=.true., quiet=.false.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 34AbCDefg"//NL// &
            "    Actual  : 34AbCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=T, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("35abCDEfg", "35AbCDefg", test_name, stat, ignore_case=.true., verbose=.true., quiet=.false.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 35AbCDefg"//NL// &
            "    Actual  : 35abCDEfg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("36ABCDEFG    ", "36AbCDefg", test_name, stat, ignore_case=.true., verbose=.true., quiet=.false.)
        expected = &
            "PASSED: "//test_name//NL// &
            "    Expected: 36AbCDefg"//NL// &
            "    Actual  : 36ABCDEFG"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=T, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("37AbCDefg", "37AbCDefg", test_name, stat, verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 37AbCDefg"//NL// &
            "    Actual  : 37AbCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("38AbCDefg    ", "38AbCDefg", test_name, stat, verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 38AbCDefg"//NL// &
            "    Actual  : 38AbCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=T, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("39AbCDefg", "39AbCDefg", test_name, stat, ignore_case=.false., verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 39AbCDefg"//NL// &
            "    Actual  : 39AbCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("40AbCDefg    ", "40AbCDefg", test_name, stat, ignore_case=.false., verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 40AbCDefg"//NL// &
            "    Actual  : 40AbCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=T, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("41abCDEfg", "41AbCDefg", test_name, stat, ignore_case=.true., verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 41AbCDefg"//NL// &
            "    Actual  : 41abCDEfg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_char_equal("42ABCDEFG    ", "42AbCDefg", test_name, stat, ignore_case=.true., verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 42AbCDefg"//NL// &
            "    Actual  : 42ABCDEFG"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call after_all()
    end subroutine char_equal_tests_success_cases

    subroutine char_equal_tests_failure_cases(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        integer(int32) :: message_output_unit_number
        logical :: stat
        character(:), allocatable :: expected, test_name

        logical, parameter :: stat_exp = .false.

        test_name = "a unit test"
        call before_all(message_output_unit_number)

        !-v expected_char_equal(actual,expected,name,stat)
        call setup(message_output_unit_number)
        call expect_char_equal("1AbCDe", "1aBcdE", test_name, stat)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 1aBcdE"//NL// &
            "    Actual  : 1AbCDe"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F)
        call setup(message_output_unit_number)
        call expect_char_equal("2aBcdE", "2AbCDe", test_name, stat, ignore_case=.false.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 2AbCDe"//NL// &
            "    Actual  : 2aBcdE"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T)
        call setup(message_output_unit_number)
        call expect_char_equal(" 3ABcdEf", "3aBcDef", test_name, stat, ignore_case=.true.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 3aBcDef"//NL// &
            "    Actual  :  3ABcdEf"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=F)
        call setup(message_output_unit_number)
        call expect_char_equal("4aBcdE", "4AbCDe", test_name, stat, verbose=.false.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 4AbCDe"//NL// &
            "    Actual  : 4aBcdE"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=F)
        call setup(message_output_unit_number)
        call expect_char_equal("5aBcdEg", "5AbCDeg", test_name, stat, ignore_case=.false., verbose=.false.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 5AbCDeg"//NL// &
            "    Actual  : 5aBcdEg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=F)
        call setup(message_output_unit_number)
        call expect_char_equal("6abcdefh", "6AbCDefg", test_name, stat, ignore_case=.true., verbose=.false.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 6AbCDefg"//NL// &
            "    Actual  : 6abcdefh"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=F, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("7abCDe", "7AbCDe", test_name, stat, verbose=.false., quiet=.false.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 7AbCDe"//NL// &
            "    Actual  : 7abCDe"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=F, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("8abCDefg", "8AbCDefg", test_name, stat, ignore_case=.false., verbose=.false., quiet=.false.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 8AbCDefg"//NL// &
            "    Actual  : 8abCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=F, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("9aacdefg", "9AbCDefg", test_name, stat, ignore_case=.true., verbose=.false., quiet=.false.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 9AbCDefg"//NL// &
            "    Actual  : 9aacdefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=F, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("10abCDe", "10AbCDe", test_name, stat, verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=F, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("11abCDefg", "11AbCDefg", test_name, stat, ignore_case=.false., verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=F, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("12bbcdefg", "12AbCDefg", test_name, stat, ignore_case=.true., verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=T)
        call setup(message_output_unit_number)
        call expect_char_equal("13aBcde", "13aBcdE", test_name, stat, verbose=.true.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 13aBcdE"//NL// &
            "    Actual  : 13aBcdE"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=T)
        call setup(message_output_unit_number)
        call expect_char_equal("14aBcdE", "14aBcde", test_name, stat, ignore_case=.false., verbose=.true.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 14aBcde"//NL// &
            "    Actual  : 14aBcdE"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=T)
        call setup(message_output_unit_number)
        call expect_char_equal("15AbCDd", "15aBcdE", test_name, stat, ignore_case=.true., verbose=.true.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 15aBcdE"//NL// &
            "    Actual  : 15AbCDd"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=T, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("16abCDefg", "16AbCDefg", test_name, stat, verbose=.true., quiet=.false.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 16AbCDefg"//NL// &
            "    Actual  : 16abCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=T, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("17abCDefg", "17AbCDefg", test_name, stat, ignore_case=.false., verbose=.true., quiet=.false.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 17AbCDefg"//NL// &
            "    Actual  : 17abCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=T, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("18abCDEgg", "18AbCDefg", test_name, stat, ignore_case=.true., verbose=.true., quiet=.false.)
        expected = &
            "FAILED: "//test_name//NL// &
            "    Expected: 18AbCDefg"//NL// &
            "    Actual  : 18abCDEgg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,verbose=T, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("19aBcdEFG", "19AbCDefg", test_name, stat, verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 19AbCDefg"//NL// &
            "    Actual  : 19aBcdEFG"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=F, verbose=T, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("20aBcdEFG", "20AbCDefg", test_name, stat, ignore_case=.false., verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 20AbCDefg"//NL// &
            "    Actual  : 20aBcdEFG"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,ignore_case=T, verbose=T, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("21aBddEFG", "21AbCDefg", test_name, stat, ignore_case=.true., verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 21AbCDefg"//NL// &
            "    Actual  : 21aBddEFG"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call after_all()
    end subroutine char_equal_tests_failure_cases

    subroutine char_equal_tests_expected_failure_cases(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        integer(int32) :: message_output_unit_number
        logical :: stat
        character(:), allocatable :: expected, test_name

        logical, parameter :: stat_exp = .true.

        test_name = "a unit test"
        call before_all(message_output_unit_number)

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T)
        call setup(message_output_unit_number)
        call expect_char_equal("1AbCDe", "1aBcdE", test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=F)
        call setup(message_output_unit_number)
        call expect_char_equal("2aBcdE", "2AbCDe", test_name, stat, expected_failure=.true., ignore_case=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=T)
        call setup(message_output_unit_number)
        call expect_char_equal(" 3ABcdEf", "3aBcDef", test_name, stat, expected_failure=.true., ignore_case=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,verbose=F)
        call setup(message_output_unit_number)
        call expect_char_equal("4aBcdE", "4AbCDe", test_name, stat, expected_failure=.true., verbose=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=F, verbose=F)
        call setup(message_output_unit_number)
        call expect_char_equal("5aBcdEg", "5AbCDeg", test_name, stat, expected_failure=.true., &
                               ignore_case=.false., verbose=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=T, verbose=F)
        call setup(message_output_unit_number)
        call expect_char_equal("6abcdefh", "6AbCDefg", test_name, stat, expected_failure=.true., &
                               ignore_case=.true., verbose=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,verbose=F, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("7abCDe", "7AbCDe", test_name, stat, expected_failure=.true., &
                               verbose=.false., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=F, verbose=F, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("8abCDefg", "8AbCDefg", test_name, stat, expected_failure=.true., &
                               ignore_case=.false., verbose=.false., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=T, verbose=F, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("9aacdefg", "9AbCDefg", test_name, stat, expected_failure=.true., &
                               ignore_case=.true., verbose=.false., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,verbose=F, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("10abCDe", "10AbCDe", test_name, stat, expected_failure=.true., &
                               verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=F, verbose=F, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("11abCDefg", "11AbCDefg", test_name, stat, expected_failure=.true., &
                               ignore_case=.false., verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=T, verbose=F, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("12bbcdefg", "12AbCDefg", test_name, stat, expected_failure=.true., &
                               ignore_case=.true., verbose=.false., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,verbose=T)
        call setup(message_output_unit_number)
        call expect_char_equal("13aBcde", "13aBcdE", test_name, stat, expected_failure=.true., verbose=.true.)
        expected = &
            "PASSED: "//test_name//" [expected failure]"//NL// &
            "    Expected: 13aBcdE"//NL// &
            "    Actual  : 13aBcdE"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=F, verbose=T)
        call setup(message_output_unit_number)
        call expect_char_equal("14aBcdE", "14aBcde", test_name, stat, expected_failure=.true., &
                               ignore_case=.false., verbose=.true.)
        expected = &
            "PASSED: "//test_name//" [expected failure]"//NL// &
            "    Expected: 14aBcde"//NL// &
            "    Actual  : 14aBcdE"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=T, verbose=T)
        call setup(message_output_unit_number)
        call expect_char_equal("15AbCDd", "15aBcdE", test_name, stat, expected_failure=.true., &
                               ignore_case=.true., verbose=.true.)
        expected = &
            "PASSED: "//test_name//" [expected failure]"//NL// &
            "    Expected: 15aBcdE"//NL// &
            "    Actual  : 15AbCDd"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,verbose=T, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("16abCDefg", "16AbCDefg", test_name, stat, expected_failure=.true., &
                               verbose=.true., quiet=.false.)
        expected = &
            "PASSED: "//test_name//" [expected failure]"//NL// &
            "    Expected: 16AbCDefg"//NL// &
            "    Actual  : 16abCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=F, verbose=T, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("17abCDefg", "17AbCDefg", test_name, stat, expected_failure=.true., &
                               ignore_case=.false., verbose=.true., quiet=.false.)
        expected = &
            "PASSED: "//test_name//" [expected failure]"//NL// &
            "    Expected: 17AbCDefg"//NL// &
            "    Actual  : 17abCDefg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=T, verbose=T, quiet=F)
        call setup(message_output_unit_number)
        call expect_char_equal("18abCDEgg", "18AbCDefg", test_name, stat, expected_failure=.true., &
                               ignore_case=.true., verbose=.true., quiet=.false.)
        expected = &
            "PASSED: "//test_name//" [expected failure]"//NL// &
            "    Expected: 18AbCDefg"//NL// &
            "    Actual  : 18abCDEgg"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,verbose=T, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("19aBcdEFG", "19AbCDefg", test_name, stat, expected_failure=.true., &
                               verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 19AbCDefg"//NL// &
            "    Actual  : 19aBcdEFG"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=F, verbose=T, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("20aBcdEFG", "20AbCDefg", test_name, stat, expected_failure=.true., &
                               ignore_case=.false., verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 20AbCDefg"//NL// &
            "    Actual  : 20aBcdEFG"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expected_char_equal(actual,expected,name,stat,expected_failure=T,ignore_case=T, verbose=T, quiet=T)
        call setup(message_output_unit_number)
        call expect_char_equal("21aBddEFG", "21AbCDefg", test_name, stat, expected_failure=.true., &
                               ignore_case=.true., verbose=.true., quiet=.true.)
        expected = &
            "    Expected: 21AbCDefg"//NL// &
            "    Actual  : 21aBddEFG"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call after_all()
    end subroutine char_equal_tests_expected_failure_cases
end module test_charEqual_expectCharEqual_unitTests_expect
