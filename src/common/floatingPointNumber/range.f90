module fassert_common_floatingPointNumber_range
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    private
    public :: get_lower_bound
    public :: get_upper_bound

    interface get_lower_bound
        procedure :: get_lower_bound_real32
        procedure :: get_lower_bound_real64
        procedure :: get_lower_bound_real128
    end interface

    interface get_upper_bound
        procedure :: get_upper_bound_real32
        procedure :: get_upper_bound_real64
        procedure :: get_upper_bound_real128
    end interface
contains
    !>単精度実数`f`から負の無限大の方向に`ulp`ULP離れた実数を返す．
    !>ただし，`f`が負の最大値あるいは`ulp`ULP以内に負の最大値がある場合は，
    !>負の最大値を返す．
    pure function get_lower_bound_real32(f, ulp) result(lower_bound)
        implicit none
        real(real32), intent(in) :: f
            !! 単精度実数
        integer(int32), intent(in) :: ulp
            !! 単精度実数`f`からの負の無限大方向への距離（ULP単位）
        real(real32) :: lower_bound
            !! `f`から`ulp`ULP離れた単精度実数

        integer(int32) :: i

        lower_bound = f
        do i = 1, ulp
            ! fから1 ulpずつ離れた値を段階的に求める．
            ! 第1引数が負の最大値の場合，ieee_next_afterは
            ! 負の最大値を返す．
            lower_bound = ieee_next_after(lower_bound, -huge(f))

            ! lower_boundが負の最大値に至った時点で，それを判定して
            ! 値を返すことも可能だが，`ulp`は2~4程度であるため，
            ! ループを回しきっても大した負荷にはならないと推定される．
        end do
    end function get_lower_bound_real32

    !>単精度実数`f`から正の無限大の方向に`ulp`ULP離れた実数を返す．
    !>ただし，`f`が正の最大値あるいは`ulp`ULP以内に正の最大値がある場合は，
    !>正の最大値を返す．
    pure function get_upper_bound_real32(f, ulp) result(upper_bound)
        implicit none
        real(real32), intent(in) :: f
            !! 単精度実数
        integer(int32), intent(in) :: ulp
            !! 単精度実数`f`からの正の無限大方向への距離（ULP単位）
        real(real32) :: upper_bound
            !! `f`から`ulp`ULP離れた単精度実数

        integer(int32) :: i

        upper_bound = f
        do i = 1, ulp
            ! fから1 ulpずつ離れた値を段階的に求める
            ! 第1引数が正の最大値の場合，ieee_next_afterは
            ! 正の最大値を返す．
            upper_bound = ieee_next_after(upper_bound, huge(f))

            ! upper_boundが正の最大値に至った時点で，それを判定して
            ! 値を返すことも可能だが，`ulp`は2~4程度であるため，
            ! ループを回しきっても大した負荷にはならないと推定される．
        end do
    end function get_upper_bound_real32

    !------------------------------------------------------------------!
    !>倍精度実数`f`から負の無限大の方向に`ulp`ULP離れた実数を返す．
    !>ただし，`f`が負の最大値あるいは`ulp`ULP以内に負の最大値がある場合は，
    !>負の最大値を返す．
    pure function get_lower_bound_real64(f, ulp) result(lower_bound)
        implicit none
        real(real64), intent(in) :: f
            !! 倍精度実数
        integer(int32), intent(in) :: ulp
            !! 倍精度実数`f`からの負の無限大方向への距離（ULP単位）
        real(real64) :: lower_bound
            !! `f`から`ulp`ULP離れた倍精度実数

        integer(int32) :: i

        lower_bound = f
        do i = 1, ulp
            ! fから1 ulpずつ離れた値を段階的に求める．
            ! 第1引数が負の最大値の場合，ieee_next_afterは
            ! 負の最大値を返す．
            lower_bound = ieee_next_after(lower_bound, -huge(f))

            ! lower_boundが負の最大値に至った時点で，それを判定して
            ! 値を返すことも可能だが，`ulp`は2~4程度であるため，
            ! ループを回しきっても大した負荷にはならないと推定される．
        end do
    end function get_lower_bound_real64

    !>倍精度実数`f`から正の無限大の方向に`ulp`ULP離れた実数を返す．
    !>ただし，`f`が正の最大値あるいは`ulp`ULP以内に正の最大値がある場合は，
    !>正の最大値を返す．
    pure function get_upper_bound_real64(f, ulp) result(upper_bound)
        implicit none
        real(real64), intent(in) :: f
            !! 倍精度実数
        integer(int32), intent(in) :: ulp
            !! 倍精度実数`f`からの正の無限大方向への距離（ULP単位）
        real(real64) :: upper_bound
            !! `f`から`ulp`ULP離れた倍精度実数

        integer(int32) :: i

        upper_bound = f
        do i = 1, ulp
            ! fから1 ulpずつ離れた値を段階的に求める
            ! 第1引数が正の最大値の場合，ieee_next_afterは
            ! 正の最大値を返す．
            upper_bound = ieee_next_after(upper_bound, huge(f))

            ! upper_boundが正の最大値に至った時点で，それを判定して
            ! 値を返すことも可能だが，`ulp`は2~4程度であるため，
            ! ループを回しきっても大した負荷にはならないと推定される．
        end do
    end function get_upper_bound_real64

    !------------------------------------------------------------------!
    !>4倍精度実数`f`から負の無限大の方向に`ulp`ULP離れた実数を返す．
    !>ただし，`f`が負の最大値あるいは`ulp`ULP以内に負の最大値がある場合は，
    !>負の最大値を返す．
    pure function get_lower_bound_real128(f, ulp) result(lower_bound)
        implicit none
        real(real128), intent(in) :: f
            !! 4倍精度実数
        integer(int32), intent(in) :: ulp
            !! 4倍精度実数`f`からの負の無限大方向への距離（ULP単位）
        real(real128) :: lower_bound
            !! `f`から`ulp`ULP離れた4倍精度実数

        integer(int32) :: i

        lower_bound = f
        do i = 1, ulp
            ! fから1 ulpずつ離れた値を段階的に求める．
            ! 第1引数が負の最大値の場合，ieee_next_afterは
            ! 負の最大値を返す．
            lower_bound = ieee_next_after(lower_bound, -huge(f))

            ! lower_boundが負の最大値に至った時点で，それを判定して
            ! 値を返すことも可能だが，`ulp`は2~4程度であるため，
            ! ループを回しきっても大した負荷にはならないと推定される．
        end do
    end function get_lower_bound_real128

    !>4倍精度実数`f`から正の無限大の方向に`ulp`ULP離れた実数を返す．
    !>ただし，`f`が正の最大値あるいは`ulp`ULP以内に正の最大値がある場合は，
    !>正の最大値を返す．
    pure function get_upper_bound_real128(f, ulp) result(upper_bound)
        implicit none
        real(real128), intent(in) :: f
            !! 4倍精度実数
        integer(int32), intent(in) :: ulp
            !! 4倍精度実数`f`からの正の無限大方向への距離（ULP単位）
        real(real128) :: upper_bound
            !! `f`から`ulp`ULP離れた4倍精度実数

        integer(int32) :: i

        upper_bound = f
        do i = 1, ulp
            ! fから1 ulpずつ離れた値を段階的に求める
            ! 第1引数が正の最大値の場合，ieee_next_afterは
            ! 正の最大値を返す．
            upper_bound = ieee_next_after(upper_bound, huge(f))

            ! upper_boundが正の最大値に至った時点で，それを判定して
            ! 値を返すことも可能だが，`ulp`は2~4程度であるため，
            ! ループを回しきっても大した負荷にはならないと推定される．
        end do
    end function get_upper_bound_real128
end module fassert_common_floatingPointNumber_range
