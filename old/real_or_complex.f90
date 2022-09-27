
module real_or_complex
    use iso_fortran_env, only: stdout => output_unit
    implicit none

    type, abstract :: number_t
        ! class(*), allocatable :: val
    contains
        procedure(add_generic_t), deferred :: add_generic
    endtype number_t

    abstract interface
        elemental function add_generic_t(x, y) result(z)
            import :: number_t
            implicit none
            class(number_t), intent(in) :: x, y
            class(number_t), allocatable :: z
        end function add_generic_t
    end interface

    type, extends(number_t) :: real_number_t
        real :: val = 0
        ! procedure(add_generic_t) ::
    endtype real_number_t

    type, extends(number_t) :: complex_number_t
        complex :: val = cmplx(0,0)
    endtype complex_number_t

contains


end module real_or_complex
