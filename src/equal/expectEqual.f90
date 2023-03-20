module expectEqual
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_check
    use :: fassert_common_optval
    use :: fassert_common_message
    use :: fassert_common_status
    use :: fassert_common_message_outputOnFailure_toUnit_equal
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

        procedure :: expect_equal_int32_msg
    end interface

    interface
        !>実測値`actual`と予測値`expected`の等値性を比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_equal_int8(actual, expected, test_name, stat, &
                                            verbose, expected_failure, quiet)
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
                !! 実測値と予測値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_equal_int8

        !>実測値`actual`と予測値`expected`の等値性を比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_equal_int16(actual, expected, test_name, stat, &
                                             verbose, expected_failure, quiet)
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
                !! 実測値と予測値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_equal_int16

        !>実測値`actual`と予測値`expected`の等値性を比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_equal_int32(actual, expected, test_name, stat, &
                                             verbose, expected_failure, quiet)
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
                !! 実測値と予測値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_equal_int32

        !>実測値`actual`と予測値`expected`の等値性を比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_equal_int64(actual, expected, test_name, stat, &
                                             verbose, expected_failure, quiet)
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
                !! 実測値と予測値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_equal_int64

        !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_equal_int32_rank1(actual, expected, test_name, stat, &
                                                   verbose, expected_failure, quiet)
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
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_equal_int32_rank1

        !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_equal_int32_rank2(actual, expected, test_name, stat, &
                                                   verbose, expected_failure, quiet)
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
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_equal_int32_rank2

        !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_equal_int32_rank3(actual, expected, test_name, stat, &
                                                   verbose, expected_failure, quiet)
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
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_equal_int32_rank3

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approxequal_real32(actual, expected, test_name, stat, &
                                                    tolerance, &
                                                    verbose, expected_failure, quiet)
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
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approxequal_real32

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approxequal_real64(actual, expected, test_name, stat, &
                                                    tolerance, &
                                                    verbose, expected_failure, quiet)
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
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approxequal_real64

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approxequal_real32_rank1(actual, expected, test_name, stat, &
                                                          tolerance, &
                                                          verbose, expected_failure, quiet)
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
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approxequal_real32_rank1

        !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approxequal_real32_rank2(actual, expected, test_name, stat, &
                                                          tolerance, &
                                                          verbose, expected_failure, quiet)
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
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approxequal_real32_rank2

        !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approxequal_real32_rank3(actual, expected, test_name, stat, &
                                                          tolerance, &
                                                          verbose, expected_failure, quiet)
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
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approxequal_real32_rank3

        !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approxequal_real64_rank1(actual, expected, test_name, stat, &
                                                          tolerance, &
                                                          verbose, expected_failure, quiet)
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
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approxequal_real64_rank1

        !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approxequal_real64_rank2(actual, expected, test_name, stat, &
                                                          tolerance, &
                                                          verbose, expected_failure, quiet)
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
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approxequal_real64_rank2

        !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approxequal_real64_rank3(actual, expected, test_name, stat, &
                                                          tolerance, &
                                                          verbose, expected_failure, quiet)
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
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approxequal_real64_rank3

        !>実測値`actual`と予測値`expected`が等価かを比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_equiv_logical(actual, expected, test_name, stat, &
                                               verbose, expected_failure, quiet)
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
                !! 実測値と予測値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_equiv_logical

        !>実測値`actual`と予測値`expected`が同じ文字列かを比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_equal_str(actual, expected, test_name, stat, &
                                           verbose, expected_failure, quiet)
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
                !! 実測値と予測値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_equal_str

        !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_equal_char_rank1(actual, expected, test_name, stat, &
                                                  verbose, expected_failure, quiet)
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
                !! 実測値と予測値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_equal_char_rank1
    end interface

    interface
        module subroutine expect_equal_int32_msg(actual, expected, test_name, stat, &
                                                 verbose, expected_failure, quiet, &
                                                 output_message)
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
                !! 実測値と予測値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 結果を格納する文字列
        end subroutine expect_equal_int32_msg
    end interface
end module expectEqual
