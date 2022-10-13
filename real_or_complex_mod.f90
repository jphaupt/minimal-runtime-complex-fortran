! using an idea I got from Robert this time
! I will start with functionality for only reals, then benchmark
! if it performs well compared to primitive reals, then I enable complex with fypp
module real_or_complex_mod
    use iso_fortran_env, only: stdout => output_unit, dp => real64
    implicit none(type, external)
    private
    public :: real_or_complex_array_t

    type, public :: real_or_complex_array_t
        !! please give it a better name if put into TCHInt :D
        integer :: ARRAY_SIZE
        logical :: complex_wf
        real(dp), allocatable :: vals_real(:)
        complex(dp), allocatable :: vals_cmplx(:)
        !! would be nice to have a vals point => vals_real or vals_complex
        !! but alas, Fortran is not that nice
    contains
        private
        procedure, public :: sum_real
        procedure, public :: clear
            !! destructor
    endtype real_or_complex_array_t

    interface real_or_complex_array_t
        module procedure :: real_or_complex_ctor
            !! type constructor
    end interface real_or_complex_array_t

 contains
    type(real_or_complex_array_t) function real_or_complex_ctor(complex_wf, array_size) &
                                        result(this)
        ! type(real_or_complex_array_t) :: this
        logical :: complex_wf
        integer :: array_size
        this%complex_wf = complex_wf
        this%ARRAY_SIZE = array_size
        if(this%complex_wf) then
            allocate(this%vals_cmplx(this%ARRAY_SIZE))
        else ! .not. complex_wf
            allocate(this%vals_real(this%ARRAY_SIZE))
        endif
    end function real_or_complex_ctor

    subroutine clear(this)
        class(real_or_complex_array_t) :: this
        if(allocated(this%vals_real)) deallocate(this%vals_real)
        if(allocated(this%vals_cmplx)) deallocate(this%vals_cmplx)
    end subroutine clear

    pure real(dp) function sum_real(this) result(vals_sum)
        class(real_or_complex_array_t), intent(in) :: this
        vals_sum = sum(this%vals_real)
    end function sum_real

end module
