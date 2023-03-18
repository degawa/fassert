module fassert
    use :: fassert_common_unit
    use :: assertEqual
    use :: expectEqual
    use :: expectSameShape
    use :: assertLogical
    use :: expectLogical
    implicit none
    private
    public :: set_assertion_message_unit
    public :: assert_equal
    public :: expect_equal
    public :: expect_same_shape
    public :: assert_true
    public :: assert_false
    public :: expect_true
    public :: expect_false
end module fassert
