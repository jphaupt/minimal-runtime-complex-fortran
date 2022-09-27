! modified from Oskar's code (thanks Oskar!)
module real_or_complex_mod
    use iso_fortran_env, only: stdout => output_unit
    implicit none(type, external)
    private
    public :: number_t, real_number_t

    type, abstract :: number_t
        integer :: ARRAY_SIZE
    contains
        private
        procedure(add_prim_real_t), deferred :: add_prim_real
        ! procedure(add_prim_real_reverse_t), deferred :: add_prim_real_reverse
            !! to add primitive real
        procedure(add_other_t), deferred :: add_other
        generic, public :: operator(+) => add_prim_real, add_other ! , add_prim_real_reverse

        procedure(print_me_t), public, deferred :: print
        ! procedure(string_me_t), public, deferred :: string
    endtype number_t

    type, extends(number_t) :: real_number_t
        real, allocatable :: vals(:)
    contains
        procedure :: add_prim_real => real_add_prim_real
        ! procedure :: add_prim_real_reverse => real_add_prim_real_reverse
        procedure :: add_other => real_add_other
        procedure :: print => real_print_me
        ! procedure :: string => real_string_me
    endtype real_number_t

    interface real_number_t
        module procedure :: real_number_ctor
    end interface real_number_t

    abstract interface
        pure function add_prim_real_t(this, x) result(res)
            import :: number_t
            implicit none
            class(number_t), intent(in) :: this
            real, intent(in) :: x(this%ARRAY_SIZE)
            class(number_t), allocatable :: res
        end function

        ! pure function add_prim_real_reverse_t(x, this) result(res)
        !     import :: number_t
        !     implicit none
        !     class(number_t), intent(in) :: this
        !     real, intent(in) :: x
        !     class(number_t), allocatable :: res
        ! end function

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

        function string_me_t(this, fmt) result(outstr)
            import :: number_t
            class(number_t), intent(in) :: this
            character(len=*), intent(in), optional :: fmt
            character(len=20) :: outstr
        end function string_me_t
    end interface


contains

    pure type(real_number_t) function real_number_ctor(vals, ARRAY_SIZE) result(this)
        integer, intent(in) :: ARRAY_SIZE
        real, intent(in) :: vals(ARRAY_SIZE)
        this%ARRAY_SIZE = ARRAY_SIZE
        allocate(this%vals(this%ARRAY_SIZE))
        this%vals = vals
    end function real_number_ctor

    pure function real_add_prim_real(this, x) result(res)
        class(real_number_t), intent(in) :: this
        real, intent(in) :: x(this%ARRAY_SIZE)
        class(number_t), allocatable :: res
        res = real_number_t(this%vals + x, this%ARRAY_SIZE)
    end function

    pure function real_add_other_real(this, other) result(res)
        class(real_number_t), intent(in) :: this, other
        class(number_t), allocatable :: res
        ! assumes this%ARRAY_SIZE == other%ARRAY_SIZE
        res = real_number_t(this%vals + other%vals, this%ARRAY_SIZE)
    end function

    pure function real_add_other(this, other) result(res)
        class(real_number_t), intent(in) :: this
        class(number_t), intent(in) :: other
        class(number_t), allocatable :: res
        select type(other)
        type is (real_number_t)
            res = real_number_t(this%vals + other%vals, this%ARRAY_SIZE)
        class default
            error stop
        end select
    end function

    subroutine real_print_me(this)
        class(real_number_t), intent(in) :: this
        write(*, *) 'real:', this%vals
    end subroutine

    ! pure function real_string_me(this, fmt) result(outstr)
    ! ! not really sure what to do here :(
    !     class(real_number_t), intent(in) :: this
    !     character(len=*), intent(in), optional :: fmt
    !     character(len=20) :: outstr
    !         !! NOTE: length 20 was chosen arbitrarily
    !     if(present(fmt)) then
    !         write(outstr, fmt) this%val
    !     else
    !         write(outstr, *) this%val
    !     endif
    ! end function real_string_me

end module
