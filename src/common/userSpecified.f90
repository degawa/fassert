module fassert_common_userSpecified
    use, intrinsic :: iso_fortran_env
    implicit none
    private

#if !defined (FASSERT_ULP) || (defined FASSERT_ULP && FASSERT_ULP<1)
#define FASSERT_ULP 2
#endif
    integer(int32), public, parameter :: ULP = FASSERT_ULP
        !! 実数の等値性を判定するULPの差<br>
        !! 1以上でなければならない
end module fassert_common_userSpecified
