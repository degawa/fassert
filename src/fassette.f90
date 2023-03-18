module fassert
    use :: fassert_common_unit
    use :: assertEqual
    use :: expectEqual
    use :: expectSameShape
    implicit none
    private
    public :: set_assertion_message_unit
    public :: assert_equal
    public :: expect_equal
    public :: expect_same_shape
end module fassert
