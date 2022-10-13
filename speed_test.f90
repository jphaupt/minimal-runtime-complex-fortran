program speed_test
    use iso_fortran_env, only: stdout => output_unit, dp => real64
    use real_or_complex_mod, only: real_or_complex_array_t
    implicit none(type, external)
    integer, parameter :: ARRAY_SIZE = 1000000000
    type(real_or_complex_array_t) :: number_array
    real(dp) :: tick, tock
        !! timer
    real(dp), allocatable :: sum_tot
        !! one problem: this would need to be done via fypp(!)
    ! integer :: i

    ! struct of array
    ! aos = real_number_t([1.],1)
    call cpu_time(tick)
    number_array = real_or_complex_array_t(.false., ARRAY_SIZE)
    number_array%vals_real = 1.
        !! again, will need a preprocessor to do this, or otherwise somehow do
        !! it in the function itself
        !! (I guess we can redefine the operator(=))
    ! print*, number_array%vals_real
    sum_tot = number_array%sum_real()

    print*, 'sum=', sum_tot
    call cpu_time(tock)
    print '("real_or_complex = ",e10.5," s")', tock - tick

end program speed_test