module fassert_common_userSpecified
    use, intrinsic :: iso_fortran_env
    implicit none
    private

#if !defined (_ULP) || (defined _ULP && _ULP<1)
#define _ULP 2
#endif
    integer(int32), public, parameter :: ULP = _ULP
        !! 実数の等値性を判定するULPの差<br>
        !! 1以上でなければならない
end module fassert_common_userSpecified
