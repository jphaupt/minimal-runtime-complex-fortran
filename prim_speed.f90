program prim_speed
    use iso_fortran_env, only: stdout => output_unit, dp => real64
    implicit none(type, external)
    integer, parameter :: ARRAY_SIZE = 1000000000
    real(dp) :: x(ARRAY_SIZE)
        !! benchmark reals
    real(dp) :: tick, tock
        !! timer

    ! benchmark: primitives
    call cpu_time(tick)
    x = 1. ! allocate
    print*, 'sum=', sum(x) ! sum and output
    call cpu_time(tock)
    print '("Benchmark = ",e10.5," s")', tock - tick

end program prim_speed
