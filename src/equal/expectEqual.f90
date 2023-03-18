module expectEqual
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_check
    use :: fassert_common_optval
    use :: fassert_common_message
    use :: fassert_common_status
    use :: fassert_common_message_outputOnFailure_equal
    implicit none
    private
    public :: expect_equal

    interface expect_equal
        procedure :: expect_equal_int8
        procedure :: expect_equal_int16
        procedure :: expect_equal_int32
        procedure :: expect_equal_int64
        procedure :: expect_equal_int32_rank1
        procedure :: expect_equal_int32_rank2
        procedure :: expect_equal_int32_rank3
        procedure :: expect_approxequal_real32
        procedure :: expect_approxequal_real64
        procedure :: expect_approxequal_real32_rank1
        procedure :: expect_approxequal_real32_rank2
        procedure :: expect_approxequal_real32_rank3
        procedure :: expect_approxequal_real64_rank1
        procedure :: expect_approxequal_real64_rank2
        procedure :: expect_approxequal_real64_rank3
        procedure :: expect_equiv_logical
        procedure :: expect_equal_str
        procedure :: expect_equal_char_rank1
    end interface

contains
    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_equal_int8(actual, expected, test_name, stat, &
                                 verbose, expected_failure, quiet)
        implicit none
        integer(int8), intent(in) :: actual
            !! 実測値
        integer(int8), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(actual == expected, test_name, stat, quiet)
        else
            call check_true(actual == expected, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_equal_int8

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_equal_int16(actual, expected, test_name, stat, &
                                  verbose, expected_failure, quiet)
        implicit none
        integer(int16), intent(in) :: actual
            !! 実測値
        integer(int16), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(actual == expected, test_name, stat, quiet)
        else
            call check_true(actual == expected, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_equal_int16

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_equal_int32(actual, expected, test_name, stat, &
                                  verbose, expected_failure, quiet)
        implicit none
        integer(int32), intent(in) :: actual
            !! 実測値
        integer(int32), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(actual == expected, test_name, stat, quiet)
        else
            call check_true(actual == expected, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_equal_int32

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_equal_int64(actual, expected, test_name, stat, &
                                  verbose, expected_failure, quiet)
        implicit none
        integer(int64), intent(in) :: actual
            !! 実測値
        integer(int64), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(actual == expected, test_name, stat, quiet)
        else
            call check_true(actual == expected, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_equal_int64

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_equal_int32_rank1(actual, expected, test_name, stat, &
                                        verbose, expected_failure, quiet)
        use :: expectSameShape
        use :: fassert_equal_compareArrayValues
        implicit none

        integer(int32), intent(in) :: actual(:)
            !! 実測値
        integer(int32), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: array_shape_stat, array_values_stat

        ! 値比較の前に配列要素数を比較
        call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                               quiet=optval(quiet, default_quiet_shape_check))
        if (is_test_failed(array_shape_stat)) then
            stat = failed
            return
        end if

        ! 各要素の値を比較
        if (are_equal(actual, expected)) then
            array_values_stat = passed
        else
            array_values_stat = failed
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(array_values_stat, test_name, stat, quiet)
        else
            call check_true(array_values_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_equal_int32_rank1

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_equal_int32_rank2(actual, expected, test_name, stat, &
                                        verbose, expected_failure, quiet)
        use :: expectSameShape
        use :: fassert_equal_compareArrayValues
        implicit none

        integer(int32), intent(in) :: actual(:, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: array_shape_stat, array_values_stat

        ! 値比較の前に配列要素数を比較
        call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                               quiet=optval(quiet, default_quiet_shape_check))
        if (is_test_failed(array_shape_stat)) then
            stat = failed
            return
        end if

        ! 各要素の値を比較
        if (are_equal(actual, expected)) then
            array_values_stat = passed
        else
            array_values_stat = failed
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(array_values_stat, test_name, stat, quiet)
        else
            call check_true(array_values_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_equal_int32_rank2

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_equal_int32_rank3(actual, expected, test_name, stat, &
                                        verbose, expected_failure, quiet)
        use :: expectSameShape
        use :: fassert_equal_compareArrayValues
        implicit none

        integer(int32), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: array_shape_stat, array_values_stat

        ! 値比較の前に配列要素数を比較
        call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                               quiet=optval(quiet, default_quiet_shape_check))
        if (is_test_failed(array_shape_stat)) then
            stat = failed
            return
        end if

        ! 各要素の値を比較
        if (are_equal(actual, expected)) then
            array_values_stat = passed
        else
            array_values_stat = failed
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(array_values_stat, test_name, stat, quiet)
        else
            call check_true(array_values_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_equal_int32_rank3

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_approxequal_real32(actual, expected, test_name, stat, &
                                         tolerance, &
                                         verbose, expected_failure, quiet)
        use :: fassert_equal_compareArrayValues
        use :: fassert_common_optval
        implicit none

        real(real32), intent(in) :: actual
            !! 実測値
        real(real32), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が許容値以下の場合`.true.`，
            !! そうでない場合`.false.`
        real(real32), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: val_stat

        ! 二つの値の差が許容範囲内かを比較
        val_stat = is_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(val_stat, test_name, stat, quiet)
        else
            call check_true(val_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_real32

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_approxequal_real64(actual, expected, test_name, stat, &
                                         tolerance, &
                                         verbose, expected_failure, quiet)
        use :: fassert_equal_compareArrayValues
        use :: fassert_common_optval
        implicit none

        real(real64), intent(in) :: actual
            !! 実測値
        real(real64), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が許容値以下の場合`.true.`，
            !! そうでない場合`.false.`
        real(real64), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: val_stat

        ! 二つの値の差が許容範囲内かを比較
        val_stat = is_approx_equal(actual, expected, tolerance)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(val_stat, test_name, stat, quiet)
        else
            call check_true(val_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_real64

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_approxequal_real32_rank1(actual, expected, test_name, stat, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        use :: expectSameShape
        use :: fassert_equal_compareArrayValues
        use :: fassert_common_optval
        implicit none

        real(real32), intent(in) :: actual(:)
            !! 実測値
        real(real32), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が許容値以下の場合`.true.`，
            !! そうでない場合`.false.`
        real(real32), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: array_shape_stat, array_values_stat

        call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                               quiet=optval(quiet, default_quiet_shape_check))
        if (is_test_failed(array_shape_stat)) then
            stat = failed
            return
        end if

        ! 各要素の値を比較
        if (are_equal(actual, expected, tolerance)) then
            array_values_stat = passed
        else
            array_values_stat = failed
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(array_values_stat, test_name, stat, quiet)
        else
            call check_true(array_values_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_real32_rank1

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_approxequal_real32_rank2(actual, expected, test_name, stat, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        use :: expectSameShape
        use :: fassert_equal_compareArrayValues
        use :: fassert_common_optval
        implicit none

        real(real32), intent(in) :: actual(:, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が許容値以下の場合`.true.`，
            !! そうでない場合`.false.`
        real(real32), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: array_shape_stat, array_values_stat

        call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                               quiet=optval(quiet, default_quiet_shape_check))
        if (is_test_failed(array_shape_stat)) then
            stat = failed
            return
        end if

        ! 各要素の値を比較
        if (are_equal(actual, expected, tolerance)) then
            array_values_stat = passed
        else
            array_values_stat = failed
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(array_values_stat, test_name, stat, quiet)
        else
            call check_true(array_values_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_real32_rank2

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_approxequal_real32_rank3(actual, expected, test_name, stat, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        use :: expectSameShape
        use :: fassert_equal_compareArrayValues
        use :: fassert_common_optval
        implicit none

        real(real32), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が許容値以下の場合`.true.`，
            !! そうでない場合`.false.`
        real(real32), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: array_shape_stat, array_values_stat

        call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                               quiet=optval(quiet, default_quiet_shape_check))
        if (is_test_failed(array_shape_stat)) then
            stat = failed
            return
        end if

        ! 各要素の値を比較
        if (are_equal(actual, expected, tolerance)) then
            array_values_stat = passed
        else
            array_values_stat = failed
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(array_values_stat, test_name, stat, quiet)
        else
            call check_true(array_values_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_real32_rank3

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_approxequal_real64_rank1(actual, expected, test_name, stat, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        use :: expectSameShape
        use :: fassert_equal_compareArrayValues
        use :: fassert_common_optval
        implicit none

        real(real64), intent(in) :: actual(:)
            !! 実測値
        real(real64), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が許容値以下の場合`.true.`，
            !! そうでない場合`.false.`
        real(real64), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: array_shape_stat, array_values_stat

        call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                               quiet=optval(quiet, default_quiet_shape_check))
        if (is_test_failed(array_shape_stat)) then
            stat = failed
            return
        end if

        ! 各要素の値を比較
        if (are_equal(actual, expected, tolerance)) then
            array_values_stat = passed
        else
            array_values_stat = failed
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(array_values_stat, test_name, stat, quiet)
        else
            call check_true(array_values_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_real64_rank1

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_approxequal_real64_rank2(actual, expected, test_name, stat, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        use :: expectSameShape
        use :: fassert_equal_compareArrayValues
        use :: fassert_common_optval
        implicit none

        real(real64), intent(in) :: actual(:, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が許容値以下の場合`.true.`，
            !! そうでない場合`.false.`
        real(real64), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: array_shape_stat, array_values_stat

        call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                               quiet=optval(quiet, default_quiet_shape_check))
        if (is_test_failed(array_shape_stat)) then
            stat = failed
            return
        end if

        ! 各要素の値を比較
        if (are_equal(actual, expected, tolerance)) then
            array_values_stat = passed
        else
            array_values_stat = failed
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(array_values_stat, test_name, stat, quiet)
        else
            call check_true(array_values_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_real64_rank2

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を標準出力に出力する．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_approxequal_real64_rank3(actual, expected, test_name, stat, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        use :: expectSameShape
        use :: fassert_equal_compareArrayValues
        use :: fassert_common_optval
        implicit none

        real(real64), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が許容値以下の場合`.true.`，
            !! そうでない場合`.false.`
        real(real64), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: array_shape_stat, array_values_stat

        call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                               quiet=optval(quiet, default_quiet_shape_check))
        if (is_test_failed(array_shape_stat)) then
            stat = failed
            return
        end if

        ! 各要素の値を比較
        if (are_equal(actual, expected, tolerance)) then
            array_values_stat = passed
        else
            array_values_stat = failed
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(array_values_stat, test_name, stat, quiet)
        else
            call check_true(array_values_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_real64_rank3

    !>実測値`actual`と予測値`expected`が等価かを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_equiv_logical(actual, expected, test_name, stat, &
                                    verbose, expected_failure, quiet)
        use :: fassert_common_check
        implicit none

        logical, intent(in) :: actual
            !! 実測値
        logical, intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を標準出力に表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(actual .eqv. expected, test_name, stat, quiet)
        else
            call check_true(actual .eqv. expected, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_equiv_logical

    !>実測値`actual`と予測値`expected`が同じ文字列かを比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_equal_str(actual, expected, test_name, stat, &
                                verbose, expected_failure, quiet)
        use :: fassert_common_check
        implicit none

        character(*), intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が同じ文字列の場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: is_same_length, string_stat

        ! 文字列の長さを比較
        call expect_equal(len(actual), len(expected), test_name//note_length_check, is_same_length, &
                          quiet=optval(quiet, default=default_quiet_shape_check))
        if (.not. is_same_length) then
            stat = failed
            return
        end if

        ! 同じ長さかつ文字列の各要素が同じ
        string_stat = is_same_length .and. (actual == expected)

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(string_stat, test_name, stat, quiet)
        else
            call check_true(string_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_equal_str

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の表示を抑制する．
    !>
    subroutine expect_equal_char_rank1(actual, expected, test_name, stat, &
                                       verbose, expected_failure, quiet)
        use :: expectSameShape
        use :: fassert_equal_compareArrayValues
        implicit none

        character, intent(in) :: actual(:)
            !! 実測値
        character, intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値の全要素が等しい場合`.true.`，
            !! そうでない場合`.false.`
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を表示するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に表示を抑制するかのフラグ

        logical :: array_shape_stat, array_values_stat

        call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                               quiet=optval(quiet, default_quiet_shape_check))
        if (is_test_failed(array_shape_stat)) then
            stat = failed
            return
        end if

        ! 各要素の値を比較
        if (are_equal(actual, expected)) then
            array_values_stat = passed
        else
            array_values_stat = failed
        end if

        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(array_values_stat, test_name, stat, quiet)
        else
            call check_true(array_values_stat, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet)) call output_on_failure(actual, expected)
    end subroutine expect_equal_char_rank1
end module expectEqual
