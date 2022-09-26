module real_or_complex_mod
    use iso_fortran_env, only: stdout => output_unit
    implicit none(type, external)
    private
    public :: number_t, real_number_t

    type, abstract :: number_t
    contains
        private
        procedure(add_prim_real_t), deferred :: add_prim_real
        procedure(add_other_t), deferred :: add_other
        generic, public :: operator(+) => add_prim_real, add_other

        procedure(print_me_t), public, deferred :: print
    endtype number_t

    type, extends(number_t) :: real_number_t
        real :: val
    contains
        procedure :: add_prim_real => real_add_prim_real
        procedure :: add_other => real_add_other
        procedure :: print => real_print_me
    endtype real_number_t

    abstract interface
        pure function add_prim_real_t(this, x) result(res)
            import :: number_t
            implicit none
            class(number_t), intent(in) :: this
            real, intent(in) :: x
            class(number_t), allocatable :: res
        end function

        pure function add_other_t(this, other) result(res)
            import :: number_t, real_number_t
            implicit none
            class(number_t), intent(in) :: this
            class(number_t), intent(in) :: other
            class(number_t), allocatable :: res
        end function

        subroutine print_me_t(this)
            import :: number_t
            class(number_t), intent(in) :: this
        end subroutine
    end interface


contains

    pure function real_add_prim_real(this, x) result(res)
        class(real_number_t), intent(in) :: this
        real, intent(in) :: x
        class(number_t), allocatable :: res
        res = real_number_t(this%val + x)
    end function

    pure function real_add_other_real(this, other) result(res)
        class(real_number_t), intent(in) :: this, other
        class(number_t), allocatable :: res
        res = real_number_t(this%val + other%val)
    end function

    pure function real_add_other(this, other) result(res)
        class(real_number_t), intent(in) :: this
        class(number_t), intent(in) :: other
        class(number_t), allocatable :: res
        select type(other)
        type is (real_number_t)
            res = real_number_t(this%val + other%val)
        class default
            error stop
        end select
    end function

    subroutine real_print_me(this)
        class(real_number_t), intent(in) :: this
        write(*, *) 'real:', this%val
    end subroutine

end module
