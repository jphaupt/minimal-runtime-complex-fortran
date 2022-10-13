program speed_test
    use iso_fortran_env, only: stdout => output_unit, dp => real64
    use real_or_complex_mod, only: real_or_complex_array_t
    implicit none(type, external)
    integer, parameter :: ARRAY_SIZE = 1000000000
    type(real_or_complex_array_t) :: number_array
    real(dp) :: x(ARRAY_SIZE)
        !! benchmark reals
    real(dp) :: tick, tock
        !! timer
    real(dp), allocatable :: sum_tot
        !! one problem: this would need to be done via fypp(!)
    integer :: i

    ! benchmark: primitives
    call cpu_time(tick)
    x = 1. ! allocate
    print*, 'sum=', sum(x) ! sum and output
    call cpu_time(tock)
    print '("Benchmark = ",e10.5," s")', tock - tick

    ! struct of array
    ! aos = real_number_t([1.],1)
    call cpu_time(tick)
    number_array = real_or_complex_array_t(.false., ARRAY_SIZE)
    sum_tot = number_array%sum_real()

    ! call aos%print()
    print*, 'sum='
    call cpu_time(tock)
    print '("AoS = ",e10.5," s")', tock - tick

end program speed_test