program main
    use real_or_complex_mod, only: number_t, real_number_t
    implicit none(type, external)
    class(number_t), allocatable :: x

    x = real_number_t(3.)

    M = M + M

    x = x + 1.

    x = x + x

    call x%print()

end program
