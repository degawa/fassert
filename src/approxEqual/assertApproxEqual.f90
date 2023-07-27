module assertApproxEqual
    use, intrinsic :: iso_fortran_env
    use :: expectApproxEqual
    use :: fassert_common_halt
    implicit none
    private
    public :: assert_approx_equal

    interface assert_approx_equal
        procedure :: assert_approx_equal_real32
        procedure :: assert_approx_equal_real64
        procedure :: assert_approx_equal_real128
        procedure :: assert_approx_equal_real32_rank1
        procedure :: assert_approx_equal_real64_rank1
        procedure :: assert_approx_equal_real128_rank1
        procedure :: assert_approx_equal_real32_rank2
        procedure :: assert_approx_equal_real64_rank2
        procedure :: assert_approx_equal_real128_rank2
        procedure :: assert_approx_equal_real32_rank3
        procedure :: assert_approx_equal_real64_rank3
        procedure :: assert_approx_equal_real128_rank3
        procedure :: assert_approx_equal_complex32
        procedure :: assert_approx_equal_complex64
        procedure :: assert_approx_equal_complex128
        procedure :: assert_approx_equal_complex32_rank1
        procedure :: assert_approx_equal_complex64_rank1
        procedure :: assert_approx_equal_complex128_rank1
        procedure :: assert_approx_equal_complex32_rank2
        procedure :: assert_approx_equal_complex64_rank2
        procedure :: assert_approx_equal_complex128_rank2
        procedure :: assert_approx_equal_complex32_rank3
        procedure :: assert_approx_equal_complex64_rank3
        procedure :: assert_approx_equal_complex128_rank3
    end interface

contains
    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real32(actual, expected, test_name, &
                                          tolerance, &
                                          verbose, expected_failure, quiet)
        implicit none

        real(real32), intent(in) :: actual
            !! 実測値
        real(real32), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real32

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real64(actual, expected, test_name, &
                                          tolerance, &
                                          verbose, expected_failure, quiet)
        implicit none

        real(real64), intent(in) :: actual
            !! 実測値
        real(real64), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real64

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real128(actual, expected, test_name, &
                                           tolerance, &
                                           verbose, expected_failure, quiet)
        implicit none

        real(real128), intent(in) :: actual
            !! 実測値
        real(real128), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real128), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real128

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real32_rank1(actual, expected, test_name, &
                                                tolerance, &
                                                verbose, expected_failure, quiet)
        implicit none

        real(real32), intent(in) :: actual(:)
            !! 実測値
        real(real32), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real32_rank1

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real32_rank2(actual, expected, test_name, &
                                                tolerance, &
                                                verbose, expected_failure, quiet)
        implicit none

        real(real32), intent(in) :: actual(:, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real32_rank2

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real32_rank3(actual, expected, test_name, &
                                                tolerance, &
                                                verbose, expected_failure, quiet)
        implicit none

        real(real32), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real32_rank3

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real64_rank1(actual, expected, test_name, &
                                                tolerance, &
                                                verbose, expected_failure, quiet)
        implicit none

        real(real64), intent(in) :: actual(:)
            !! 実測値
        real(real64), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real64_rank1

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real64_rank2(actual, expected, test_name, &
                                                tolerance, &
                                                verbose, expected_failure, quiet)
        implicit none

        real(real64), intent(in) :: actual(:, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real64_rank2

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real64_rank3(actual, expected, test_name, &
                                                tolerance, &
                                                verbose, expected_failure, quiet)
        implicit none

        real(real64), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real64_rank3

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real128_rank1(actual, expected, test_name, &
                                                 tolerance, &
                                                 verbose, expected_failure, quiet)
        implicit none

        real(real128), intent(in) :: actual(:)
            !! 実測値
        real(real128), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real128), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real128_rank1

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real128_rank2(actual, expected, test_name, &
                                                 tolerance, &
                                                 verbose, expected_failure, quiet)
        implicit none

        real(real128), intent(in) :: actual(:, :)
            !! 実測値
        real(real128), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real128), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real128_rank2

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_real128_rank3(actual, expected, test_name, &
                                                 tolerance, &
                                                 verbose, expected_failure, quiet)
        implicit none

        real(real128), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real128), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real128), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_real128_rank3

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex32(actual, expected, test_name, &
                                             tolerance, &
                                             verbose, expected_failure, quiet)
        implicit none

        complex(real32), intent(in) :: actual
            !! 実測値
        complex(real32), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex32

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex64(actual, expected, test_name, &
                                             tolerance, &
                                             verbose, expected_failure, quiet)
        implicit none

        complex(real64), intent(in) :: actual
            !! 実測値
        complex(real64), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex64

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex128(actual, expected, test_name, &
                                              tolerance, &
                                              verbose, expected_failure, quiet)
        implicit none

        complex(real128), intent(in) :: actual
            !! 実測値
        complex(real128), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real128), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex128

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex32_rank1(actual, expected, test_name, &
                                                   tolerance, &
                                                   verbose, expected_failure, quiet)
        implicit none

        complex(real32), intent(in) :: actual(:)
            !! 実測値
        complex(real32), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex32_rank1

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex32_rank2(actual, expected, test_name, &
                                                   tolerance, &
                                                   verbose, expected_failure, quiet)
        implicit none

        complex(real32), intent(in) :: actual(:, :)
            !! 実測値
        complex(real32), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex32_rank2

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex32_rank3(actual, expected, test_name, &
                                                   tolerance, &
                                                   verbose, expected_failure, quiet)
        implicit none

        complex(real32), intent(in) :: actual(:, :, :)
            !! 実測値
        complex(real32), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex32_rank3

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex64_rank1(actual, expected, test_name, &
                                                   tolerance, &
                                                   verbose, expected_failure, quiet)
        implicit none

        complex(real64), intent(in) :: actual(:)
            !! 実測値
        complex(real64), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex64_rank1

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex64_rank2(actual, expected, test_name, &
                                                   tolerance, &
                                                   verbose, expected_failure, quiet)
        implicit none

        complex(real64), intent(in) :: actual(:, :)
            !! 実測値
        complex(real64), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex64_rank2

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex64_rank3(actual, expected, test_name, &
                                                   tolerance, &
                                                   verbose, expected_failure, quiet)
        implicit none

        complex(real64), intent(in) :: actual(:, :, :)
            !! 実測値
        complex(real64), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex64_rank3

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex128_rank1(actual, expected, test_name, &
                                                    tolerance, &
                                                    verbose, expected_failure, quiet)
        implicit none

        complex(real128), intent(in) :: actual(:)
            !! 実測値
        complex(real128), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real128), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex128_rank1

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex128_rank2(actual, expected, test_name, &
                                                    tolerance, &
                                                    verbose, expected_failure, quiet)
        implicit none

        complex(real128), intent(in) :: actual(:, :)
            !! 実測値
        complex(real128), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real128), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex128_rank2

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを検査する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approx_equal_complex128_rank3(actual, expected, test_name, &
                                                    tolerance, &
                                                    verbose, expected_failure, quiet)
        implicit none

        complex(real128), intent(in) :: actual(:, :, :)
            !! 実測値
        complex(real128), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real128), intent(in) :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_approx_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approx_equal_complex128_rank3

end module assertApproxEqual
