module expectApproxEqual
    use, intrinsic :: iso_fortran_env
    use :: fassert_common_check
    use :: fassert_common_message
    use :: fassert_common_status
    use :: fassert_common_message_outputOnFailure_toUnit_equal
    implicit none
    private
    public :: expect_approx_equal

    interface expect_approx_equal
        procedure :: expect_approx_equal_real32
        procedure :: expect_approx_equal_real64
        procedure :: expect_approx_equal_real128
        procedure :: expect_approx_equal_complex32
        procedure :: expect_approx_equal_complex64
        procedure :: expect_approx_equal_complex128
        procedure :: expect_approx_equal_real32_rank1
        procedure :: expect_approx_equal_real64_rank1
        procedure :: expect_approx_equal_real128_rank1
        procedure :: expect_approx_equal_complex32_rank1
        procedure :: expect_approx_equal_complex64_rank1
        procedure :: expect_approx_equal_complex128_rank1
        procedure :: expect_approx_equal_real32_rank2
        procedure :: expect_approx_equal_real64_rank2
        procedure :: expect_approx_equal_real128_rank2
        procedure :: expect_approx_equal_complex32_rank2
        procedure :: expect_approx_equal_complex64_rank2
        procedure :: expect_approx_equal_complex128_rank2
        procedure :: expect_approx_equal_real32_rank3
        procedure :: expect_approx_equal_real64_rank3
        procedure :: expect_approx_equal_real128_rank3
        procedure :: expect_approx_equal_complex32_rank3
        procedure :: expect_approx_equal_complex64_rank3
        procedure :: expect_approx_equal_complex128_rank3

        procedure :: expect_approx_equal_real32_msg
        procedure :: expect_approx_equal_real64_msg
        procedure :: expect_approx_equal_real128_msg
        procedure :: expect_approx_equal_complex32_msg
        procedure :: expect_approx_equal_complex64_msg
        procedure :: expect_approx_equal_complex128_msg
        procedure :: expect_approx_equal_real32_rank1_msg
        procedure :: expect_approx_equal_real64_rank1_msg
        procedure :: expect_approx_equal_real128_rank1_msg
        procedure :: expect_approx_equal_complex32_rank1_msg
        procedure :: expect_approx_equal_complex64_rank1_msg
        procedure :: expect_approx_equal_complex128_rank1_msg
        procedure :: expect_approx_equal_real32_rank2_msg
        procedure :: expect_approx_equal_real64_rank2_msg
        procedure :: expect_approx_equal_real128_rank2_msg
        procedure :: expect_approx_equal_complex32_rank2_msg
        procedure :: expect_approx_equal_complex64_rank2_msg
        procedure :: expect_approx_equal_complex128_rank2_msg
        procedure :: expect_approx_equal_real32_rank3_msg
        procedure :: expect_approx_equal_real64_rank3_msg
        procedure :: expect_approx_equal_real128_rank3_msg
        procedure :: expect_approx_equal_complex32_rank3_msg
        procedure :: expect_approx_equal_complex64_rank3_msg
        procedure :: expect_approx_equal_complex128_rank3_msg
    end interface

    interface
        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real32(actual, expected, test_name, stat, &
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
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real32

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real64(actual, expected, test_name, stat, &
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
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real64

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real128(actual, expected, test_name, stat, &
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
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real128

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real32_rank1(actual, expected, test_name, stat, &
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
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real32_rank1

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real32_rank2(actual, expected, test_name, stat, &
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
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real32_rank2

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real32_rank3(actual, expected, test_name, stat, &
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
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real32_rank3

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real64_rank1(actual, expected, test_name, stat, &
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
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real64_rank1

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real64_rank2(actual, expected, test_name, stat, &
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
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real64_rank2

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real64_rank3(actual, expected, test_name, stat, &
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
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real64_rank3

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real128_rank1(actual, expected, test_name, stat, &
                                                            tolerance, &
                                                            verbose, expected_failure, quiet)
            real(real128), intent(in) :: actual(:)
                !! 実測値
            real(real128), intent(in) :: expected(:)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real128_rank1

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real128_rank2(actual, expected, test_name, stat, &
                                                            tolerance, &
                                                            verbose, expected_failure, quiet)
            real(real128), intent(in) :: actual(:, :)
                !! 実測値
            real(real128), intent(in) :: expected(:, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real128_rank2

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_real128_rank3(actual, expected, test_name, stat, &
                                                            tolerance, &
                                                            verbose, expected_failure, quiet)
            real(real128), intent(in) :: actual(:, :, :)
                !! 実測値
            real(real128), intent(in) :: expected(:, :, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_real128_rank3

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex32(actual, expected, test_name, stat, &
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
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex32

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex64(actual, expected, test_name, stat, &
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
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex64

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex128(actual, expected, test_name, stat, &
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
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex128

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex32_rank1(actual, expected, test_name, stat, &
                                                              tolerance, &
                                                              verbose, expected_failure, quiet)
            complex(real32), intent(in) :: actual(:)
                !! 実測値
            complex(real32), intent(in) :: expected(:)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex32_rank1

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex32_rank2(actual, expected, test_name, stat, &
                                                              tolerance, &
                                                              verbose, expected_failure, quiet)
            complex(real32), intent(in) :: actual(:, :)
                !! 実測値
            complex(real32), intent(in) :: expected(:, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex32_rank2

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex32_rank3(actual, expected, test_name, stat, &
                                                              tolerance, &
                                                              verbose, expected_failure, quiet)
            complex(real32), intent(in) :: actual(:, :, :)
                !! 実測値
            complex(real32), intent(in) :: expected(:, :, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex32_rank3

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex64_rank1(actual, expected, test_name, stat, &
                                                              tolerance, &
                                                              verbose, expected_failure, quiet)
            complex(real64), intent(in) :: actual(:)
                !! 実測値
            complex(real64), intent(in) :: expected(:)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex64_rank1

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex64_rank2(actual, expected, test_name, stat, &
                                                              tolerance, &
                                                              verbose, expected_failure, quiet)
            complex(real64), intent(in) :: actual(:, :)
                !! 実測値
            complex(real64), intent(in) :: expected(:, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex64_rank2

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex64_rank3(actual, expected, test_name, stat, &
                                                              tolerance, &
                                                              verbose, expected_failure, quiet)
            complex(real64), intent(in) :: actual(:, :, :)
                !! 実測値
            complex(real64), intent(in) :: expected(:, :, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex64_rank3

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex128_rank1(actual, expected, test_name, stat, &
                                                               tolerance, &
                                                               verbose, expected_failure, quiet)
            complex(real128), intent(in) :: actual(:)
                !! 実測値
            complex(real128), intent(in) :: expected(:)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex128_rank1

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex128_rank2(actual, expected, test_name, stat, &
                                                               tolerance, &
                                                               verbose, expected_failure, quiet)
            complex(real128), intent(in) :: actual(:, :)
                !! 実測値
            complex(real128), intent(in) :: expected(:, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex128_rank2

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査する．
        !>
        !>`stat`が渡されていれば，比較結果を`stat`に書き込む．
        !>
        !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
        !>
        !>`expected_failure`が真であれば，比較が失敗することを検査する．
        !>
        !>`quiet`が真の場合，成功時の出力を抑制する．
        !>
        module subroutine expect_approx_equal_complex128_rank3(actual, expected, test_name, stat, &
                                                               tolerance, &
                                                               verbose, expected_failure, quiet)
            complex(real128), intent(in) :: actual(:, :, :)
                !! 実測値
            complex(real128), intent(in) :: expected(:, :, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
        end subroutine expect_approx_equal_complex128_rank3

    end interface

    interface
        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real32_msg(actual, expected, test_name, stat, &
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
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real32_msg

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real64_msg(actual, expected, test_name, stat, &
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
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real64_msg

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real128_msg(actual, expected, test_name, stat, &
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
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real128_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real32_rank1_msg(actual, expected, test_name, stat, &
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
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real32_rank1_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real32_rank2_msg(actual, expected, test_name, stat, &
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
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real32_rank2_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real32_rank3_msg(actual, expected, test_name, stat, &
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
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real32_rank3_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real64_rank1_msg(actual, expected, test_name, stat, &
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
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real64_rank1_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real64_rank2_msg(actual, expected, test_name, stat, &
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
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real64_rank2_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real64_rank3_msg(actual, expected, test_name, stat, &
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
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real64_rank3_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real128_rank1_msg(actual, expected, test_name, stat, &
                                                                     tolerance, &
                                                                     verbose, expected_failure, quiet, &
                                                                     output_message)
            real(real128), intent(in) :: actual(:)
                !! 実測値
            real(real128), intent(in) :: expected(:)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real128_rank1_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real128_rank2_msg(actual, expected, test_name, stat, &
                                                                     tolerance, &
                                                                     verbose, expected_failure, quiet, &
                                                                     output_message)
            real(real128), intent(in) :: actual(:, :)
                !! 実測値
            real(real128), intent(in) :: expected(:, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real128_rank2_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_real128_rank3_msg(actual, expected, test_name, stat, &
                                                                     tolerance, &
                                                                     verbose, expected_failure, quiet, &
                                                                     output_message)
            real(real128), intent(in) :: actual(:, :, :)
                !! 実測値
            real(real128), intent(in) :: expected(:, :, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_real128_rank3_msg

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex32_msg(actual, expected, test_name, stat, &
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
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex32_msg

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex64_msg(actual, expected, test_name, stat, &
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
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex64_msg

        !>実測値`actual`と予測値`expected`の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex128_msg(actual, expected, test_name, stat, &
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
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値，それらの差を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex128_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex32_rank1_msg(actual, expected, test_name, stat, &
                                                                       tolerance, &
                                                                       verbose, expected_failure, quiet, &
                                                                       output_message)
            complex(real32), intent(in) :: actual(:)
                !! 実測値
            complex(real32), intent(in) :: expected(:)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex32_rank1_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex32_rank2_msg(actual, expected, test_name, stat, &
                                                                       tolerance, &
                                                                       verbose, expected_failure, quiet, &
                                                                       output_message)
            complex(real32), intent(in) :: actual(:, :)
                !! 実測値
            complex(real32), intent(in) :: expected(:, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex32_rank2_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex32_rank3_msg(actual, expected, test_name, stat, &
                                                                       tolerance, &
                                                                       verbose, expected_failure, quiet, &
                                                                       output_message)
            complex(real32), intent(in) :: actual(:, :, :)
                !! 実測値
            complex(real32), intent(in) :: expected(:, :, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real32), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex32_rank3_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex64_rank1_msg(actual, expected, test_name, stat, &
                                                                       tolerance, &
                                                                       verbose, expected_failure, quiet, &
                                                                       output_message)
            complex(real64), intent(in) :: actual(:)
                !! 実測値
            complex(real64), intent(in) :: expected(:)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex64_rank1_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex64_rank2_msg(actual, expected, test_name, stat, &
                                                                       tolerance, &
                                                                       verbose, expected_failure, quiet, &
                                                                       output_message)
            complex(real64), intent(in) :: actual(:, :)
                !! 実測値
            complex(real64), intent(in) :: expected(:, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex64_rank2_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex64_rank3_msg(actual, expected, test_name, stat, &
                                                                       tolerance, &
                                                                       verbose, expected_failure, quiet, &
                                                                       output_message)
            complex(real64), intent(in) :: actual(:, :, :)
                !! 実測値
            complex(real64), intent(in) :: expected(:, :, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real64), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex64_rank3_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex128_rank1_msg(actual, expected, test_name, stat, &
                                                                        tolerance, &
                                                                        verbose, expected_failure, quiet, &
                                                                        output_message)
            complex(real128), intent(in) :: actual(:)
                !! 実測値
            complex(real128), intent(in) :: expected(:)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex128_rank1_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex128_rank2_msg(actual, expected, test_name, stat, &
                                                                        tolerance, &
                                                                        verbose, expected_failure, quiet, &
                                                                        output_message)
            complex(real128), intent(in) :: actual(:, :)
                !! 実測値
            complex(real128), intent(in) :: expected(:, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex128_rank2_msg

        !>実測値`actual`と予測値`expected`の各要素の差が
        !>許容値`tolerance`より小さいかを検査し，
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
        pure module subroutine expect_approx_equal_complex128_rank3_msg(actual, expected, test_name, stat, &
                                                                        tolerance, &
                                                                        verbose, expected_failure, quiet, &
                                                                        output_message)
            complex(real128), intent(in) :: actual(:, :, :)
                !! 実測値
            complex(real128), intent(in) :: expected(:, :, :)
                !! 予測値
            character(*), intent(in) :: test_name
                !! テスト名
            logical, intent(out) :: stat
                !! 比較結果の真偽値<br>
                !! 実測値と予測値が許容値以下の場合`.true.`，
                !! そうでない場合`.false.`
            real(real128), intent(in) :: tolerance
                !! 実測値と予測値が等しいと見なす許容値
            logical, intent(in), optional :: verbose
                !! 実測値と予測値の差の最大・最小値を出力するフラグ
            logical, intent(in), optional :: expected_failure
                !! 予期された失敗を検査するかのフラグ
            logical, intent(in), optional :: quiet
                !! 成功時に出力を抑制するかのフラグ
            character(:), allocatable, intent(out) :: output_message
                !! 出力を格納する文字列
        end subroutine expect_approx_equal_complex128_rank3_msg

    end interface
end module expectApproxEqual
