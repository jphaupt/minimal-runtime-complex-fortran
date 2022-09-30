program speed_test
    use iso_fortran_env, only: stdout => output_unit
    use real_or_complex_mod, only: number_t, real_number_t
    implicit none(type, external)
    integer, parameter :: ARRAY_SIZE = 1000000
    class(number_t), allocatable :: aos
        !! now struct of arrays
        !! not any more array of structs
    real :: x(ARRAY_SIZE)
        !! benchmark reals
    ! TODO struct of array
    real :: tick, tock
        !! timer
    class(number_t), allocatable :: sum_tot
    integer :: i

    sum_tot = real_number_t(0.)

    ! benchmark: primitives
    call cpu_time(tick)
    x = 1. ! allocate
    print*, 'sum=', sum(x) ! sum and output
    call cpu_time(tock)
    print '("Benchmark = ",e10.5," s")', tock - tick

    ! struct of array
    ! aos = real_number_t([1.],1)
    call cpu_time(tick)
    aos = real_number_t(ARRAY_SIZE=ARRAY_SIZE)
    select type(aos)
    type is(real_number_t)
        allocate(aos%vals(ARRAY_SIZE))
        aos%vals = 1.
        sum_tot = real_number_t([sum(aos%vals)], 1)
    end select

    ! call aos%print()
    print*, 'sum='
    call sum_tot%print()
    call cpu_time(tock)
    print '("AoS = ",e10.5," s")', tock - tick

    ! ! "array of struct" - defunct
    ! not sure why this doesn't work
    ! call cpu_time(tick)
    ! ! allocate(real_number_t :: aos(ARRAY_SIZE))
    ! ! aos = real_number_t(1.)
    ! ! call aos(ARRAY_SIZE)%print()
    ! ! aos(1) = real_number_t(1.)
    ! do i=1,ARRAY_SIZE
    !     aos(i) = real_number_t(1.)
    ! enddo
    ! ! aos = real_number_t(1.)
    ! ! aos%print()
    ! ! I think this is the only way to do this, at least for now
    ! do i=1,ARRAY_SIZE
    !     sum_tot = aos(i) + sum_tot
    ! enddo
    ! call sum_tot%print()
    ! call cpu_time(tock)
    ! print '("AoS = ",f6.5," s")', tock - tick

    ! TODO soa

end program speed_test