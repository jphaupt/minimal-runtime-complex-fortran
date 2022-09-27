program main
    use iso_fortran_env, only: stdout => output_unit
    use real_or_complex_mod, only: number_t, real_number_t
    implicit none(type, external)
    class(number_t), allocatable :: x
    real :: a = 8.

    x = real_number_t(3.)

    x = x + 1.

    x = x + x

    call x%print()

    print*, a, 1, 2, 3
    print*, x%string(), 1, 2, 3

end program
