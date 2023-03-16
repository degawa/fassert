module expectSameShape
    use, intrinsic :: iso_fortran_env
    use :: assert_common_check
    use :: assert_common_optval
    use :: assert_common_message, only:default_quiet_shape_check
    implicit none
    private
    public :: expect_same_shape

    interface expect_same_shape
        procedure :: expect_same_shape_rank1_int32
        procedure :: expect_same_shape_rank2_int32
        procedure :: expect_same_shape_rank3_int32
        procedure :: expect_same_shape_rank1_real32
        procedure :: expect_same_shape_rank2_real32
        procedure :: expect_same_shape_rank3_real32
        procedure :: expect_same_shape_rank1_real64
        procedure :: expect_same_shape_rank2_real64
        procedure :: expect_same_shape_rank3_real64
        procedure :: expect_same_shape_rank1_char
    end interface

contains
    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine expect_same_shape_rank1_int32(actual, expected, test_name, stat, quiet)
        use :: assert_sameShape_compareArrayShape
        implicit none

        integer(int32), intent(in) :: actual(:)
            !! 実測値
        integer(int32), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ

        call check_true(are_same_shape(actual, expected), test_name, stat, &
                        quiet=optval(quiet, default_quiet_shape_check))
    end subroutine expect_same_shape_rank1_int32

    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine expect_same_shape_rank2_int32(actual, expected, test_name, stat, quiet)
        use :: assert_sameShape_compareArrayShape
        implicit none

        integer(int32), intent(in) :: actual(:, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ

        call check_true(are_same_shape(actual, expected), test_name, stat, &
                        quiet=optval(quiet, default_quiet_shape_check))
    end subroutine expect_same_shape_rank2_int32

    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine expect_same_shape_rank3_int32(actual, expected, test_name, stat, quiet)
        use :: assert_sameShape_compareArrayShape
        implicit none

        integer(int32), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ

        call check_true(are_same_shape(actual, expected), test_name, stat, &
                        quiet=optval(quiet, default_quiet_shape_check))
    end subroutine expect_same_shape_rank3_int32

    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine expect_same_shape_rank1_real32(actual, expected, test_name, stat, quiet)
        use :: assert_sameShape_compareArrayShape
        implicit none

        real(real32), intent(in) :: actual(:)
            !! 実測値
        real(real32), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ

        call check_true(are_same_shape(actual, expected), test_name, stat, &
                        quiet=optval(quiet, default_quiet_shape_check))
    end subroutine expect_same_shape_rank1_real32

    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine expect_same_shape_rank2_real32(actual, expected, test_name, stat, quiet)
        use :: assert_sameShape_compareArrayShape
        implicit none

        real(real32), intent(in) :: actual(:, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ

        call check_true(are_same_shape(actual, expected), test_name, stat, &
                        quiet=optval(quiet, default_quiet_shape_check))
    end subroutine expect_same_shape_rank2_real32

    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine expect_same_shape_rank3_real32(actual, expected, test_name, stat, quiet)
        use :: assert_sameShape_compareArrayShape
        implicit none

        real(real32), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ

        call check_true(are_same_shape(actual, expected), test_name, stat, &
                        quiet=optval(quiet, default_quiet_shape_check))
    end subroutine expect_same_shape_rank3_real32

    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine expect_same_shape_rank1_real64(actual, expected, test_name, stat, quiet)
        use :: assert_sameShape_compareArrayShape
        implicit none

        real(real64), intent(in) :: actual(:)
            !! 実測値
        real(real64), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ

        call check_true(are_same_shape(actual, expected), test_name, stat, &
                        quiet=optval(quiet, default_quiet_shape_check))
    end subroutine expect_same_shape_rank1_real64

    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine expect_same_shape_rank2_real64(actual, expected, test_name, stat, quiet)
        use :: assert_sameShape_compareArrayShape
        implicit none

        real(real64), intent(in) :: actual(:, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ

        call check_true(are_same_shape(actual, expected), test_name, stat, &
                        quiet=optval(quiet, default_quiet_shape_check))
    end subroutine expect_same_shape_rank2_real64

    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine expect_same_shape_rank3_real64(actual, expected, test_name, stat, quiet)
        use :: assert_sameShape_compareArrayShape
        implicit none

        real(real64), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ

        call check_true(are_same_shape(actual, expected), test_name, stat, &
                        quiet=optval(quiet, default_quiet_shape_check))
    end subroutine expect_same_shape_rank3_real64

    !>二つの配列が同じ形状かを検査する．
    !>
    !>検査結果を`stat`に書き込む．
    !>
    !>`quiet`が真の場合，表示を抑制する．
    subroutine expect_same_shape_rank1_char(actual, expected, test_name, stat, quiet)
        use :: assert_sameShape_compareArrayShape
        implicit none

        character, intent(in) :: actual(:)
            !! 実測値
        character, intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の配列形状が同じの場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: quiet
            !! 表示を抑制するかのフラグ

        call check_true(are_same_shape(actual, expected), test_name, stat, &
                        quiet=optval(quiet, default_quiet_shape_check))
    end subroutine expect_same_shape_rank1_char
end module expectSameShape
