module assert_common_status
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: is_test_passed
    public :: is_test_failed

    logical, public, parameter :: passed = .true.
        !! テスト成功を意味する定数
    logical, public, parameter :: failed = .not. passed
        !! テスト失敗を意味する定数

    logical, public, parameter :: default_test_of_expected_failure = .false.

contains
    !>渡されたテスト結果を確認し，テストが成功している場合に`.true.`
    !>失敗している場合に`.false.`を返す．
    pure elemental logical function is_test_passed(test_status)
        implicit none
        logical, intent(in) :: test_status
            !! テスト結果

        is_test_passed = (test_status .eqv. passed)
    end function is_test_passed

    !>渡されたテスト結果を確認し，テストが失敗している場合に`.true.`
    !>成功している場合に`.false.`を返す．
    pure elemental logical function is_test_failed(test_status)
        implicit none
        logical, intent(in) :: test_status
            !! テスト結果

        is_test_failed = .not. is_test_passed(test_status)
    end function is_test_failed
end module assert_common_status
