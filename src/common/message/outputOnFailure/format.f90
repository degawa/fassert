module fassert_common_message_outputOnFailure_format
    use :: fassert_common_message, only:default_verbose_format_indent
    implicit none
    private

    character(*), public, parameter :: fmt_indent = default_verbose_format_indent
        !! 出力時にインデントするための書式指定文字列

    character(*), public, parameter :: int_specifier = "I0"
        !! 整数型変数の出力書式指定子
    character(*), public, parameter :: real32_specifier = "E13.6e2"
        !! 4バイト浮動小数点数型変数の出力書式指定子
    character(*), public, parameter :: real64_specifier = "E23.15e3"
        !! 8バイト浮動小数点数型変数の出力書式指定子
    character(*), public, parameter :: real128_specifier = "E40.32e5"
        !! 16バイト浮動小数点数型変数の出力書式指定子
    character(*), public, parameter :: complex32_specifier = '"(",'//real32_specifier//',",",'//real32_specifier//',")"'
        !! 4バイト複素数型変数の出力書式指定子
    character(*), public, parameter :: complex64_specifier = '"(",'//real64_specifier//',",",'//real64_specifier//',")"'
        !! 8バイト複素数型変数の出力書式指定子
    character(*), public, parameter :: complex128_specifier = '"(",'//real128_specifier//',",",'//real128_specifier//',")"'
        !! 16バイト複素数型変数の出力書式指定子

    character(*), private, parameter :: fmt_index_separator = ',",",'
        !! 配列添字の各要素の区切り
    character(*), private, parameter :: fmt_index = 'I0'
        !! 配列添字の書式指定文字列
    character(*), public, parameter :: fmt_index_rank1 = fmt_index
        !! 1次元配列に対する配列添字の書式指定文字列
    character(*), public, parameter :: fmt_index_rank2 = fmt_index//repeat(fmt_index_separator//fmt_index, 2 - 1)
        !! 2次元配列に対する配列添字の書式指定文字列
    character(*), public, parameter :: fmt_index_rank3 = fmt_index//repeat(fmt_index_separator//fmt_index, 3 - 1)
        !! 3次元配列に対する配列添字の書式指定文字列

    character(*), public, parameter :: fmt_int = '('//fmt_indent//',A,'//int_specifier//')'
        !! 整数の文字列出力書式
    character(*), public, parameter :: fmt_real32 = '('//fmt_indent//',A,'//real32_specifier//')'
        !! 4バイト浮動小数点数の文字列出力書式
    character(*), public, parameter :: fmt_real64 = '('//fmt_indent//',A,'//real64_specifier//')'
        !! 8バイト浮動小数点数の文字列出力書式
    character(*), public, parameter :: fmt_real128 = '('//fmt_indent//',A,'//real128_specifier//')'
        !! 16バイト浮動小数点数の文字列出力書式
    character(*), public, parameter :: fmt_complex32 = '('//fmt_indent//',A,'//complex32_specifier//')'
        !! 4バイト複素数の文字列出力書式
    character(*), public, parameter :: fmt_complex64 = '('//fmt_indent//',A,'//complex64_specifier//')'
        !! 8バイト複素数の文字列出力書式
    character(*), public, parameter :: fmt_complex128 = '('//fmt_indent//',A,'//complex128_specifier//')'
        !! 16バイト複素数の文字列出力書式
    character(*), public, parameter :: fmt_str = '('//fmt_indent//',A,A)'
        !! 文字列の出力書式
    character(*), public, parameter :: fmt_char = '('//fmt_indent//',A,*(A:," "))'
        !! 文字の出力書式

    character(*), public, parameter :: fmt_int_rank1 &
                                        = '('//fmt_indent//',A,'//int_specifier//'," at (",'//fmt_index_rank1//',")")'
        !! 整数型配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_int_rank2 &
                                        = '('//fmt_indent//',A,'//int_specifier//'," at (",'//fmt_index_rank2//',")")'
        !! 整数型配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_int_rank3 &
                                        = '('//fmt_indent//',A,'//int_specifier//'," at (",'//fmt_index_rank3//',")")'
        !! 整数型配列における実測値と予測値の差の出力書式

    character(*), public, parameter :: fmt_real32_rank1 &
                                        = '('//fmt_indent//',A,'//real32_specifier//'," at (",'//fmt_index_rank1//',")")'
        !! 実数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_real32_rank2 &
                                        = '('//fmt_indent//',A,'//real32_specifier//'," at (",'//fmt_index_rank2//',")")'
        !! 実数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_real32_rank3 &
                                        = '('//fmt_indent//',A,'//real32_specifier//'," at (",'//fmt_index_rank3//',")")'
        !! 実数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_real64_rank1 &
                                        = '('//fmt_indent//',A,'//real64_specifier//'," at (",'//fmt_index_rank1//',")")'
        !! 実数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_real64_rank2 &
                                        = '('//fmt_indent//',A,'//real64_specifier//'," at (",'//fmt_index_rank2//',")")'
        !! 実数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_real64_rank3 &
                                        = '('//fmt_indent//',A,'//real64_specifier//'," at (",'//fmt_index_rank3//',")")'
        !! 実数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_real128_rank1 &
                                        = '('//fmt_indent//',A,'//real128_specifier//'," at (",'//fmt_index_rank1//',")")'
        !! 実数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_real128_rank2 &
                                        = '('//fmt_indent//',A,'//real128_specifier//'," at (",'//fmt_index_rank2//',")")'
        !! 実数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_real128_rank3 &
                                        = '('//fmt_indent//',A,'//real128_specifier//'," at (",'//fmt_index_rank3//',")")'
        !! 実数配列における実測値と予測値の差の出力書式

    character(*), public, parameter :: fmt_complex32_rank1 &
                                        = '('//fmt_indent//',A,'//complex32_specifier//'," at (",'//fmt_index_rank1//',")")'
        !! 複素数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_complex32_rank2 &
                                        = '('//fmt_indent//',A,'//complex32_specifier//'," at (",'//fmt_index_rank2//',")")'
        !! 複素数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_complex32_rank3 &
                                        = '('//fmt_indent//',A,'//complex32_specifier//'," at (",'//fmt_index_rank3//',")")'
        !! 複素数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_complex64_rank1 &
                                        = '('//fmt_indent//',A,'//complex64_specifier//'," at (",'//fmt_index_rank1//',")")'
        !! 複素数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_complex64_rank2 &
                                        = '('//fmt_indent//',A,'//complex64_specifier//'," at (",'//fmt_index_rank2//',")")'
        !! 複素数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_complex64_rank3 &
                                        = '('//fmt_indent//',A,'//complex64_specifier//'," at (",'//fmt_index_rank3//',")")'
        !! 複素数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_complex128_rank1 &
                                        = '('//fmt_indent//',A,'//complex128_specifier//'," at (",'//fmt_index_rank1//',")")'
        !! 複素数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_complex128_rank2 &
                                        = '('//fmt_indent//',A,'//complex128_specifier//'," at (",'//fmt_index_rank2//',")")'
        !! 複素数配列における実測値と予測値の差の出力書式
    character(*), public, parameter :: fmt_complex128_rank3 &
                                        = '('//fmt_indent//',A,'//complex128_specifier//'," at (",'//fmt_index_rank3//',")")'
        !! 複素数配列における実測値と予測値の差の出力書式

    character(*), public, parameter :: fmt_position_rank1 &
                                        = '('//fmt_indent//',A,"at (",'//fmt_index_rank1//',")")'
        !! 実測値と予測値が異なる配列インデックスの出力書式
    character(*), public, parameter :: fmt_position_rank2 &
                                        = '('//fmt_indent//',A,"at (",'//fmt_index_rank2//',")")'
        !! 実測値と予測値が異なる配列インデックスの出力書式
    character(*), public, parameter :: fmt_position_rank3 &
                                        = '('//fmt_indent//',A,"at (",'//fmt_index_rank3//',")")'
        !! 実測値と予測値が異なる配列インデックスの出力書式
end module fassert_common_message_outputOnFailure_format
