module fassert
    use :: fassert_common_unit
    use :: assertEqual
    use :: expectEqual
    use :: assertApproxEqual
    use :: expectApproxEqual
    use :: expectSameShape
    use :: assertLogical
    use :: expectLogical
    use :: expectCharEqual
    use :: assertCharEqual
    use :: expectSameType
    use :: assertSameType
    implicit none
    private
    ! assert procedures
    public :: assert_equal
    public :: assert_approx_equal
    public :: assert_true
    public :: assert_false
    public :: assert_char_equal
    public :: assert_same_type
    ! expect procedures
    public :: expect_equal
    public :: expect_approx_equal
    public :: expect_true
    public :: expect_false
    public :: expect_char_equal
    public :: expect_same_type
    public :: expect_same_shape
    ! utility procedures
    public :: set_assertion_message_unit
end module fassert
