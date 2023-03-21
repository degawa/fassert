module fassert_common_message_outputOnFailure_format
    use :: fassert_common_message, only:default_verbose_format_indent
    implicit none
    private

    character(*), public, parameter :: fmt_indent = default_verbose_format_indent
        !! 出力時にインデントするための書式指定文字列

    character(*), private, parameter :: fmt_index_separator = ',",",'
        !! 配列添字の各要素の区切り
    character(*), public, parameter :: fmt_index_rank1 = 'i0'
        !! 1次元配列に対する配列添字の書式指定文字列
    character(*), public, parameter :: fmt_index_rank2 = fmt_index_rank1// &
                                       repeat(fmt_index_separator//fmt_index_rank1, 2 - 1)
        !! 2次元配列に対する配列添字の書式指定文字列
    character(*), public, parameter :: fmt_index_rank3 = fmt_index_rank1// &
                                       repeat(fmt_index_separator//fmt_index_rank1, 3 - 1)
        !! 3次元配列に対する配列添字の書式指定文字列
end module fassert_common_message_outputOnFailure_format
