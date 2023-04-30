module fassert
    use :: fassert_common_unit
    use :: assertEqual
    use :: expectEqual
    use :: expectSameShape
    use :: assertLogical
    use :: expectLogical
    implicit none
    private
    ! assert procedures
    public :: assert_equal
    public :: assert_true
    public :: assert_false
    ! expect procedures
    public :: expect_equal
    public :: expect_true
    public :: expect_false
    public :: expect_same_shape
    ! utility procedures
    public :: set_assertion_message_unit
end module fassert
