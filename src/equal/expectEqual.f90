module expectEqual
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_check
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
        procedure :: expect_approxequal_real128
        procedure :: expect_approxequal_complex32
        procedure :: expect_approxequal_complex64
        procedure :: expect_approxequal_complex128
        procedure :: expect_approxequal_real32_rank1
        procedure :: expect_approxequal_real32_rank2
        procedure :: expect_approxequal_real32_rank3
        procedure :: expect_approxequal_real64_rank1
        procedure :: expect_approxequal_real64_rank2
        procedure :: expect_approxequal_real64_rank3
        procedure :: expect_equiv_logical
        procedure :: expect_equal_str
        procedure :: expect_equal_char_rank1

        procedure :: expect_equal_int8_msg
        procedure :: expect_equal_int16_msg
        procedure :: expect_equal_int32_msg
        procedure :: expect_equal_int64_msg
        procedure :: expect_equal_int32_rank1_msg
        procedure :: expect_equal_int32_rank2_msg
        procedure :: expect_equal_int32_rank3_msg
        procedure :: expect_approxequal_real32_msg
        procedure :: expect_approxequal_real64_msg
        procedure :: expect_approxequal_real128_msg
        procedure :: expect_approxequal_complex32_msg
        procedure :: expect_approxequal_complex64_msg
        procedure :: expect_approxequal_complex128_msg
        procedure :: expect_approxequal_real32_rank1_msg
        procedure :: expect_approxequal_real32_rank2_msg
        procedure :: expect_approxequal_real32_rank3_msg
        procedure :: expect_approxequal_real64_rank1_msg
        procedure :: expect_approxequal_real64_rank2_msg
        procedure :: expect_approxequal_real64_rank3_msg
        procedure :: expect_equiv_logical_msg
        procedure :: expect_equal_str_msg
        procedure :: expect_equal_char_rank1_msg

        procedure :: expect_equal_user_defined
        procedure :: expect_equal_user_defined_msg
        procedure :: expect_equal_user_defined_rank1
        procedure :: expect_equal_user_defined_rank1_msg
        procedure :: expect_equal_user_defined_rank2
        procedure :: expect_equal_user_defined_rank2_msg
        procedure :: expect_equal_user_defined_rank3
        procedure :: expect_equal_user_defined_rank3_msg
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
        module subroutine expect_approxequal_real128(actual, expected, test_name, stat, &
                                                     tolerance, &
                                                     verbose, expected_failure, quiet)
            real(real128), intent(in) :: actual
                !! 実測値
            real(real128), intent(in) :: expected
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in), optional :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approxequal_real128

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

        !>実測値`actual`と予測値`expected`の差が
        !>
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
        module subroutine expect_approxequal_complex32(actual, expected, test_name, stat, &
                                                       tolerance, &
                                                       verbose, expected_failure, quiet)
            complex(real32), intent(in) :: actual
                !! 実測値
            complex(real32), intent(in) :: expected
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
        end subroutine expect_approxequal_complex32

        !>実測値`actual`と予測値`expected`の差が
        !>
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
        module subroutine expect_approxequal_complex64(actual, expected, test_name, stat, &
                                                       tolerance, &
                                                       verbose, expected_failure, quiet)
            complex(real64), intent(in) :: actual
                !! 実測値
            complex(real64), intent(in) :: expected
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
        end subroutine expect_approxequal_complex64

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
        module subroutine expect_approxequal_complex128(actual, expected, test_name, stat, &
                                                        tolerance, &
                                                        verbose, expected_failure, quiet)
            complex(real128), intent(in) :: actual
                !! 実測値
            complex(real128), intent(in) :: expected
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in), optional :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approxequal_complex128

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
        !>実測値`actual`と予測値`expected`の等値性を比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_equal_int8_msg(actual, expected, test_name, stat, &
                                                     verbose, expected_failure, quiet, &
                                                     output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_equal_int8_msg

        !>実測値`actual`と予測値`expected`の等値性を比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_equal_int16_msg(actual, expected, test_name, stat, &
                                                      verbose, expected_failure, quiet, &
                                                      output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_equal_int16_msg

        !>実測値`actual`と予測値`expected`の等値性を比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_equal_int32_msg(actual, expected, test_name, stat, &
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
                !! 出力を格納する文字列
        end subroutine expect_equal_int32_msg

        !>実測値`actual`と予測値`expected`の等値性を比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_equal_int64_msg(actual, expected, test_name, stat, &
                                                      verbose, expected_failure, quiet, &
                                                      output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_equal_int64_msg

        !>実測値`actual`と予測値`expected`の全要素の等値性を比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_equal_int32_rank1_msg(actual, expected, test_name, stat, &
                                                            verbose, expected_failure, quiet, &
                                                            output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_equal_int32_rank1_msg

        !>実測値`actual`と予測値`expected`の全要素の等値性を比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_equal_int32_rank2_msg(actual, expected, test_name, stat, &
                                                            verbose, expected_failure, quiet, &
                                                            output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_equal_int32_rank2_msg

        !>実測値`actual`と予測値`expected`の全要素の等値性を比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_equal_int32_rank3_msg(actual, expected, test_name, stat, &
                                                            verbose, expected_failure, quiet, &
                                                            output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_equal_int32_rank3_msg

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_approxequal_real32_msg(actual, expected, test_name, stat, &
                                                             tolerance, &
                                                             verbose, expected_failure, quiet, &
                                                             output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_real32_msg

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
        pure module subroutine expect_approxequal_real64_msg(actual, expected, test_name, stat, &
                                                             tolerance, &
                                                             verbose, expected_failure, quiet, &
                                                             output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_real64_msg

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_approxequal_real128_msg(actual, expected, test_name, stat, &
                                                              tolerance, &
                                                              verbose, expected_failure, quiet, &
                                                              output_message)
            real(real128), intent(in) :: actual
                !! 実測値
            real(real128), intent(in) :: expected
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in), optional :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_real128_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_approxequal_real32_rank1_msg(actual, expected, test_name, stat, &
                                                                   tolerance, &
                                                                   verbose, expected_failure, quiet, &
                                                                   output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_real32_rank1_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_approxequal_real32_rank2_msg(actual, expected, test_name, stat, &
                                                                   tolerance, &
                                                                   verbose, expected_failure, quiet, &
                                                                   output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_real32_rank2_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_approxequal_real32_rank3_msg(actual, expected, test_name, stat, &
                                                                   tolerance, &
                                                                   verbose, expected_failure, quiet, &
                                                                   output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_real32_rank3_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_approxequal_real64_rank1_msg(actual, expected, test_name, stat, &
                                                                   tolerance, &
                                                                   verbose, expected_failure, quiet, &
                                                                   output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_real64_rank1_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_approxequal_real64_rank2_msg(actual, expected, test_name, stat, &
                                                                   tolerance, &
                                                                   verbose, expected_failure, quiet, &
                                                                   output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_real64_rank2_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_approxequal_real64_rank3_msg(actual, expected, test_name, stat, &
                                                                   tolerance, &
                                                                   verbose, expected_failure, quiet, &
                                                                   output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_real64_rank3_msg

        !>実測値`actual`と予測値`expected`の差が
        !>
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
        module subroutine expect_approxequal_complex32_msg(actual, expected, test_name, stat, &
                                                           tolerance, &
                                                           verbose, expected_failure, quiet, &
                                                           output_message)
            complex(real32), intent(in) :: actual
                !! 実測値
            complex(real32), intent(in) :: expected
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_complex32_msg

        !>実測値`actual`と予測値`expected`の差が
        !>
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
        module subroutine expect_approxequal_complex64_msg(actual, expected, test_name, stat, &
                                                           tolerance, &
                                                           verbose, expected_failure, quiet, &
                                                           output_message)
            complex(real64), intent(in) :: actual
                !! 実測値
            complex(real64), intent(in) :: expected
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_complex64_msg

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_approxequal_complex128_msg(actual, expected, test_name, stat, &
                                                                 tolerance, &
                                                                 verbose, expected_failure, quiet, &
                                                                 output_message)
            complex(real128), intent(in) :: actual
                !! 実測値
            complex(real128), intent(in) :: expected
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in), optional :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approxequal_complex128_msg

        !>実測値`actual`と予測値`expected`が等価かを比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_equiv_logical_msg(actual, expected, test_name, stat, &
                                                        verbose, expected_failure, quiet, &
                                                        output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_equiv_logical_msg

        !>実測値`actual`と予測値`expected`が同じ文字列かを比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_equal_str_msg(actual, expected, test_name, stat, &
                                                    verbose, expected_failure, quiet, &
                                                    output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_equal_str_msg

        !>実測値`actual`と予測値`expected`の全要素の等値性を比較し，
        !>出力を`output_message`に書き込む．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        pure module subroutine expect_equal_char_rank1_msg(actual, expected, test_name, stat, &
                                                           verbose, expected_failure, quiet, &
                                                           output_message)
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
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_equal_char_rank1_msg
    end interface

contains
    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine expect_equal_user_defined(actual, expected, test_name, stat, &
                                         comparator, verbose_message_writer, &
                                         verbose, expected_failure, quiet)
        use :: fassert_interface
        implicit none
        class(*), intent(in) :: actual
            !! 実測値
        class(*), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        procedure(Iis_equal) :: comparator
            !! ユーザ定義型変数`actual`, `expected`の等価演算を行う手続
        procedure(Ioutput_on_failure_to_unit), optional :: verbose_message_writer
            !! 値の出力を行う手続
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: has_same_value

        has_same_value = comparator(actual, expected)
        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_value, test_name, stat, quiet)
        else
            call check_true(has_same_value, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet) &
            .and. present(verbose_message_writer)) &
            call verbose_message_writer(actual, expected)
    end subroutine expect_equal_user_defined

    !>実測値`actual`と予測値`expected`の等値性を比較し，
    !>出力を`output_message`に書き込む．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine expect_equal_user_defined_msg(actual, expected, test_name, stat, &
                                             comparator, verbose_message_writer, &
                                             verbose, expected_failure, quiet, &
                                             output_message)
        use :: fassert_interface
        implicit none
        class(*), intent(in) :: actual
            !! 実測値
        class(*), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        procedure(Iis_equal) :: comparator
            !! ユーザ定義型変数`actual`, `expected`の等価演算を行う手続
        procedure(Ioutput_on_failure_to_string), optional :: verbose_message_writer
            !! 値の出力を行う手続
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ
        character(:), allocatable, intent(out) :: output_message
            !! 出力を格納する文字列

        logical :: has_same_value

        has_same_value = comparator(actual, expected)
        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_value, test_name, stat, quiet, output_message)
        else
            call check_true(has_same_value, test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet) &
            .and. present(verbose_message_writer)) &
            call verbose_message_writer(actual, expected, output_message)
    end subroutine expect_equal_user_defined_msg

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine expect_equal_user_defined_rank1(actual, expected, test_name, stat, &
                                               comparator, verbose_message_writer, &
                                               verbose, expected_failure, quiet)
        use :: fassert_interface
        use :: expectSameShape
        implicit none
        class(*), intent(in) :: actual(:)
            !! 実測値
        class(*), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        procedure(Iis_equal_rank1) :: comparator
            !! ユーザ定義型変数`actual`, `expected`の等価演算を行う手続
        procedure(Ioutput_on_failure_to_unit_rank1), optional :: verbose_message_writer
            !! 値の出力を行う手続
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: has_same_value

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                   verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        has_same_value = comparator(actual, expected)
        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_value, test_name, stat, quiet)
        else
            call check_true(has_same_value, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet) &
            .and. present(verbose_message_writer)) &
            call verbose_message_writer(actual, expected)
    end subroutine expect_equal_user_defined_rank1

    !>実測値`actual`と予測値`expected`の等値性を比較し，
    !>出力を`output_message`に書き込む．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    pure subroutine expect_equal_user_defined_rank1_msg(actual, expected, test_name, stat, &
                                                        comparator, verbose_message_writer, &
                                                        verbose, expected_failure, quiet, &
                                                        output_message)
        use :: fassert_interface
        use :: expectSameShape
        implicit none
        class(*), intent(in) :: actual(:)
            !! 実測値
        class(*), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        procedure(Iis_equal_rank1) :: comparator
            !! ユーザ定義型変数`actual`, `expected`の等価演算を行う手続
        procedure(Ioutput_on_failure_to_string_rank1), optional :: verbose_message_writer
            !! 値の出力を行う手続
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ
        character(:), allocatable, intent(out) :: output_message
            !! 出力を格納する文字列

        logical :: has_same_value

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                   verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(output_message, shape_check_msg)
                return
            end if
        end block

        has_same_value = comparator(actual, expected)
        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_value, test_name, stat, quiet, output_message)
        else
            call check_true(has_same_value, test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet) &
            .and. present(verbose_message_writer)) &
            call verbose_message_writer(actual, expected, output_message)
    end subroutine expect_equal_user_defined_rank1_msg

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine expect_equal_user_defined_rank2(actual, expected, test_name, stat, &
                                               comparator, verbose_message_writer, &
                                               verbose, expected_failure, quiet)
        use :: fassert_interface
        use :: expectSameShape
        implicit none
        class(*), intent(in) :: actual(:, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        procedure(Iis_equal_rank2) :: comparator
            !! ユーザ定義型変数`actual`, `expected`の等価演算を行う手続
        procedure(Ioutput_on_failure_to_unit_rank2), optional :: verbose_message_writer
            !! 値の出力を行う手続
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: has_same_value

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                   verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        has_same_value = comparator(actual, expected)
        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_value, test_name, stat, quiet)
        else
            call check_true(has_same_value, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet) &
            .and. present(verbose_message_writer)) &
            call verbose_message_writer(actual, expected)
    end subroutine expect_equal_user_defined_rank2

    !>実測値`actual`と予測値`expected`の等値性を比較し，
    !>出力を`output_message`に書き込む．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    pure subroutine expect_equal_user_defined_rank2_msg(actual, expected, test_name, stat, &
                                                        comparator, verbose_message_writer, &
                                                        verbose, expected_failure, quiet, &
                                                        output_message)
        use :: fassert_interface
        use :: expectSameShape
        implicit none
        class(*), intent(in) :: actual(:, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        procedure(Iis_equal_rank2) :: comparator
            !! ユーザ定義型変数`actual`, `expected`の等価演算を行う手続
        procedure(Ioutput_on_failure_to_string_rank2), optional :: verbose_message_writer
            !! 値の出力を行う手続
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ
        character(:), allocatable, intent(out) :: output_message
            !! 出力を格納する文字列

        logical :: has_same_value

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                   verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(output_message, shape_check_msg)
                return
            end if
        end block

        has_same_value = comparator(actual, expected)
        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_value, test_name, stat, quiet, output_message)
        else
            call check_true(has_same_value, test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet) &
            .and. present(verbose_message_writer)) &
            call verbose_message_writer(actual, expected, output_message)
    end subroutine expect_equal_user_defined_rank2_msg

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine expect_equal_user_defined_rank3(actual, expected, test_name, stat, &
                                               comparator, verbose_message_writer, &
                                               verbose, expected_failure, quiet)
        use :: fassert_interface
        use :: expectSameShape
        implicit none
        class(*), intent(in) :: actual(:, :, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        procedure(Iis_equal_rank3) :: comparator
            !! ユーザ定義型変数`actual`, `expected`の等価演算を行う手続
        procedure(Ioutput_on_failure_to_unit_rank3), optional :: verbose_message_writer
            !! 値の出力を行う手続
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: has_same_value

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                   verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(shape_check_msg)
                return
            end if
        end block

        has_same_value = comparator(actual, expected)
        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_value, test_name, stat, quiet)
        else
            call check_true(has_same_value, test_name, stat, quiet)
        end if

        if (is_verbose_output(stat, verbose, quiet) &
            .and. present(verbose_message_writer)) &
            call verbose_message_writer(actual, expected)
    end subroutine expect_equal_user_defined_rank3

    !>実測値`actual`と予測値`expected`の等値性を比較し，
    !>出力を`output_message`に書き込む．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真であれば，比較が失敗することを検査する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    pure subroutine expect_equal_user_defined_rank3_msg(actual, expected, test_name, stat, &
                                                        comparator, verbose_message_writer, &
                                                        verbose, expected_failure, quiet, &
                                                        output_message)
        use :: fassert_interface
        use :: expectSameShape
        implicit none
        class(*), intent(in) :: actual(:, :, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(out) :: stat
            !! 比較結果の真偽値<br>
            !! 実測値と予測値が等しい場合`.true.`，
            !! そうでない場合`.false.`
        procedure(Iis_equal_rank3) :: comparator
            !! ユーザ定義型変数`actual`, `expected`の等価演算を行う手続
        procedure(Ioutput_on_failure_to_string_rank3), optional :: verbose_message_writer
            !! 値の出力を行う手続
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ
        character(:), allocatable, intent(out) :: output_message
            !! 出力を格納する文字列

        logical :: has_same_value

        ! 値比較の前に配列要素数を比較し，異なっていれば失敗とする．
        block
            logical :: array_shape_stat
            character(:), allocatable :: shape_check_msg

            call expect_same_shape(actual, expected, test_name//note_shape_check, array_shape_stat, &
                                   verbose=verbose, quiet=quiet, output_message=shape_check_msg)
            if (is_test_failed(array_shape_stat)) then
                stat = failed
                call write_message(output_message, shape_check_msg)
                return
            end if
        end block

        has_same_value = comparator(actual, expected)
        if (is_test_of_expected_failure(expected_failure)) then
            call check_expected_failure(has_same_value, test_name, stat, quiet, output_message)
        else
            call check_true(has_same_value, test_name, stat, quiet, output_message)
        end if

        if (is_verbose_output(stat, verbose, quiet) &
            .and. present(verbose_message_writer)) &
            call verbose_message_writer(actual, expected, output_message)
    end subroutine expect_equal_user_defined_rank3_msg
end module expectEqual
