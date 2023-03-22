module assertEqual
    use, intrinsic :: iso_fortran_env
    use :: expectEqual
    use :: fassert_common_halt
    implicit none
    private
    public :: assert_equal

    interface assert_equal
        procedure :: assert_equal_int8
        procedure :: assert_equal_int16
        procedure :: assert_equal_int32
        procedure :: assert_equal_int64
        procedure :: assert_equal_int32_rank1
        procedure :: assert_equal_int32_rank2
        procedure :: assert_equal_int32_rank3
        procedure :: assert_approxequal_real32
        procedure :: assert_approxequal_real64
        procedure :: assert_approxequal_real32_rank1
        procedure :: assert_approxequal_real32_rank2
        procedure :: assert_approxequal_real32_rank3
        procedure :: assert_approxequal_real64_rank1
        procedure :: assert_approxequal_real64_rank2
        procedure :: assert_approxequal_real64_rank3
        procedure :: assert_equiv_logical
        procedure :: assert_equal_str
        procedure :: assert_equal_char_rank1
        procedure :: assert_equal_user_defined
        procedure :: assert_equal_user_defined_rank1
        procedure :: assert_equal_user_defined_rank2
        procedure :: assert_equal_user_defined_rank3
    end interface

contains
    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_int8(actual, expected, test_name, &
                                 verbose, expected_failure, quiet)
        implicit none
        integer(int8), intent(in) :: actual
            !! 実測値
        integer(int8), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_int8

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_int16(actual, expected, test_name, &
                                  verbose, expected_failure, quiet)
        implicit none
        integer(int16), intent(in) :: actual
            !! 実測値
        integer(int16), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_int16

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_int32(actual, expected, test_name, &
                                  verbose, expected_failure, quiet)
        implicit none
        integer(int32), intent(in) :: actual
            !! 実測値
        integer(int32), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, &
                          verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_int32

    !>実測値`actual`と予測値`expected`の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_int64(actual, expected, test_name, &
                                  verbose, expected_failure, quiet)
        implicit none
        integer(int64), intent(in) :: actual
            !! 実測値
        integer(int64), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_int64

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_int32_rank1(actual, expected, test_name, &
                                        verbose, expected_failure, quiet)
        implicit none

        integer(int32), intent(in) :: actual(:)
            !! 実測値
        integer(int32), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_int32_rank1

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_int32_rank2(actual, expected, test_name, &
                                        verbose, expected_failure, quiet)
        implicit none

        integer(int32), intent(in) :: actual(:, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_int32_rank2

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_int32_rank3(actual, expected, test_name, &
                                        verbose, expected_failure, quiet)
        implicit none

        integer(int32), intent(in) :: actual(:, :, :)
            !! 実測値
        integer(int32), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_int32_rank3

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approxequal_real32(actual, expected, test_name, &
                                         tolerance, &
                                         verbose, expected_failure, quiet)
        implicit none

        real(real32), intent(in) :: actual
            !! 実測値
        real(real32), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approxequal_real32

    !>実測値`actual`と予測値`expected`の差が許容値`tolerance`より小さいかを比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値，それらの差を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approxequal_real64(actual, expected, test_name, &
                                         tolerance, &
                                         verbose, expected_failure, quiet)
        implicit none

        real(real64), intent(in) :: actual
            !! 実測値
        real(real64), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値，それらの差を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approxequal_real64

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approxequal_real32_rank1(actual, expected, test_name, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        implicit none

        real(real32), intent(in) :: actual(:)
            !! 実測値
        real(real32), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approxequal_real32_rank1

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approxequal_real32_rank2(actual, expected, test_name, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        implicit none

        real(real32), intent(in) :: actual(:, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approxequal_real32_rank2

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approxequal_real32_rank3(actual, expected, test_name, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        implicit none

        real(real32), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real32), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real32), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approxequal_real32_rank3

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approxequal_real64_rank1(actual, expected, test_name, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        implicit none

        real(real64), intent(in) :: actual(:)
            !! 実測値
        real(real64), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approxequal_real64_rank1

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approxequal_real64_rank2(actual, expected, test_name, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        implicit none

        real(real64), intent(in) :: actual(:, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approxequal_real64_rank2

    !>実測値`actual`と予測値`expected`の各要素の差が許容値`tolerance`より小さいかを比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値の差の最大・最小値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_approxequal_real64_rank3(actual, expected, test_name, &
                                               tolerance, &
                                               verbose, expected_failure, quiet)
        implicit none

        real(real64), intent(in) :: actual(:, :, :)
            !! 実測値
        real(real64), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        real(real64), intent(in), optional :: tolerance
            !! 実測値と予測値が等しいと見なす許容値
        logical, intent(in), optional :: verbose
            !! 実測値と予測値の差の最大・最小値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, tolerance, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_approxequal_real64_rank3

    !>実測値`actual`と予測値`expected`が等価かを比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equiv_logical(actual, expected, test_name, &
                                    verbose, expected_failure, quiet)
        implicit none

        logical, intent(in) :: actual
            !! 実測値
        logical, intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equiv_logical

    !>実測値`actual`と予測値`expected`が同じ文字列かを比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_str(actual, expected, test_name, &
                                verbose, expected_failure, quiet)
        implicit none

        character(*), intent(in) :: actual
            !! 実測値
        character(*), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_str

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_char_rank1(actual, expected, test_name, &
                                       verbose, expected_failure, quiet)
        implicit none

        character, intent(in) :: actual(:)
            !! 実測値
        character, intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
        logical, intent(in), optional :: verbose
            !! 実測値と予測値を出力するフラグ
        logical, intent(in), optional :: expected_failure
            !! 予期された失敗を検査するかのフラグ
        logical, intent(in), optional :: quiet
            !! 成功時に出力を抑制するかのフラグ

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_char_rank1

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_user_defined(actual, expected, test_name, &
                                         comparator, verbose_message_writer, &
                                         verbose, expected_failure, quiet)
        use :: fassert_interface
        class(*), intent(in) :: actual
            !! 実測値
        class(*), intent(in) :: expected
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
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

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, &
                          comparator, verbose_message_writer, &
                          verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_user_defined

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_user_defined_rank1(actual, expected, test_name, &
                                               comparator, verbose_message_writer, &
                                               verbose, expected_failure, quiet)
        use :: fassert_interface
        class(*), intent(in) :: actual(:)
            !! 実測値
        class(*), intent(in) :: expected(:)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
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

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, &
                          comparator, verbose_message_writer, &
                          verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_user_defined_rank1

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_user_defined_rank2(actual, expected, test_name, &
                                               comparator, verbose_message_writer, &
                                               verbose, expected_failure, quiet)
        use :: fassert_interface
        class(*), intent(in) :: actual(:, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
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

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, &
                          comparator, verbose_message_writer, &
                          verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_user_defined_rank2

    !>実測値`actual`と予測値`expected`の全要素の等値性を比較する．
    !>比較結果が偽であれば，プログラムの実行を停止する．
    !>比較には，手続`comparator`が用いられる．
    !>
    !>`verbose`が真であれば，実測値と予測値を出力する．
    !>実測値と予測値の出力には，手続`verbose_message_writer`
    !>が用いられる．
    !>
    !>`expected_failure`が真の場合，比較が失敗することを検査し，
    !>プログラムを停止しない．比較が成功すると停止する．
    !>
    !>`quiet`が真の場合，成功時の出力を抑制する．
    !>
    subroutine assert_equal_user_defined_rank3(actual, expected, test_name, &
                                               comparator, verbose_message_writer, &
                                               verbose, expected_failure, quiet)
        use :: fassert_interface
        class(*), intent(in) :: actual(:, :, :)
            !! 実測値
        class(*), intent(in) :: expected(:, :, :)
            !! 予測値
        character(*), intent(in) :: test_name
            !! テスト名
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

        logical :: stat

        call expect_equal(actual, expected, test_name, stat, &
                          comparator, verbose_message_writer, &
                          verbose, expected_failure, quiet)
        call halt_on_failure(stat)
    end subroutine assert_equal_user_defined_rank3
end module assertEqual
