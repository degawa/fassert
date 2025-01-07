module test_sameType_expectSameType_unitTests_expect_D2
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type
    use :: testdrive_util, only:occurred, to_string
    use :: expectSameType
    use :: test_sameType_expectSameType_unitTests_expect_common
    implicit none
    private
    public :: expect_same_type_d2_tests_success_cases
    public :: expect_same_type_d2_tests_failure_cases
    public :: expect_same_type_d2_tests_expected_failure_cases

contains
    subroutine expect_same_type_d2_tests_success_cases(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        integer(int32) :: message_output_unit_number
        logical :: stat
        character(:), allocatable :: expected, test_name

        type(a_parent_type) :: p_1(1, 2), p_2(2, 3)
        type(an_extended_type) :: e1_1(2, 1), e1_2(5, 1)
        type(another_extended_type) :: e2_1(1, 3), e2_2(2, 2)

        logical, parameter :: stat_exp = .true.

        test_name = "a unit test"
        call before_all(message_output_unit_number)

        !-v expect_same_type(actual,expected,name,stat)
        call setup(message_output_unit_number)
        call expect_same_type(p_1, p_2, test_name, stat)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e1_2, test_name, stat)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e2_1, e2_2, test_name, stat)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expect_same_type(actual,expected,name,stat, quiet=F)
        call setup(message_output_unit_number)
        call expect_same_type(p_1, p_2, test_name, stat, quiet=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e1_2, test_name, stat, quiet=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e2_1, e2_2, test_name, stat, quiet=.false.)
        expected = "PASSED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expect_same_type(actual,expected,name,stat, quiet=T)
        call setup(message_output_unit_number)
        call expect_same_type(p_1, p_2, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e1_2, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e2_1, e2_2, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return
        call after_all()
    end subroutine expect_same_type_d2_tests_success_cases

    subroutine expect_same_type_d2_tests_failure_cases(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        integer(int32) :: message_output_unit_number
        logical :: stat
        character(:), allocatable :: expected, test_name

        type(a_parent_type) :: p_1(1, 2), p_2(2, 3)
        type(an_extended_type) :: e1_1(2, 1), e1_2(5, 1)
        type(another_extended_type) :: e2_1(1, 3), e2_2(2, 2)

        logical, parameter :: stat_exp = .false.

        test_name = "a unit test"
        call before_all(message_output_unit_number)

        !-v expect_same_type(actual,expected,name,stat)
        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_1, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_2, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_1, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_2, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_1, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_2, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_1, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_2, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_1, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_2, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_1, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_2, test_name, stat)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expect_same_type(actual,expected,name,stat, quiet=F)
        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_1, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_2, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_1, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_2, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_1, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_2, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_1, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_2, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_1, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_2, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_1, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_2, test_name, stat, quiet=.false.)
        expected = "FAILED: "//test_name
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expect_same_type(actual,expected,name,stat, quiet=T)
        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_1, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_2, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_1, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_2, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_1, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_2, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_1, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_2, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_1, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_2, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_1, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_2, test_name, stat, quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call after_all()
    end subroutine expect_same_type_d2_tests_failure_cases

    subroutine expect_same_type_d2_tests_expected_failure_cases(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        integer(int32) :: message_output_unit_number
        logical :: stat
        character(:), allocatable :: expected, test_name

        type(a_parent_type) :: p_1(1, 2), p_2(2, 3)
        type(an_extended_type) :: e1_1(2, 1), e1_2(5, 1)
        type(another_extended_type) :: e2_1(1, 3), e2_2(2, 2)

        logical, parameter :: stat_exp = .true.

        test_name = "a unit test"
        call before_all(message_output_unit_number)

        !-v expect_same_type(actual,expected,name,stat,expected_failure=T)
        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_1, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_2, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_1, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_2, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_1, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_2, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_1, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_2, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_1, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_2, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_1, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_2, test_name, stat, expected_failure=.true.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expect_same_type(actual,expected,name,stat,expected_failure=T, quiet=F)
        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_1, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_2, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_1, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_2, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_1, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_2, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_1, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_2, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_1, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_2, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_1, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_2, test_name, stat, expected_failure=.true., quiet=.false.)
        expected = "PASSED: "//test_name//" [expected failure]"
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        !-v expect_same_type(actual,expected,name,stat,expected_failure=T, quiet=T)
        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_1, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e1_2, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_1, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e1_2, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_1, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_1, e2_2, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_1, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(p_2, e2_2, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_1, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_1, e2_2, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_1, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call setup(message_output_unit_number)
        call expect_same_type(e1_2, e2_2, test_name, stat, expected_failure=.true., quiet=.true.)
        expected = ""
        call check_result(error, stat, expected, stat_exp, message_output_unit_number)
        call teardown(message_output_unit_number)
        if (occurred(error)) return

        call after_all()
    end subroutine expect_same_type_d2_tests_expected_failure_cases
end module test_sameType_expectSameType_unitTests_expect_D2
