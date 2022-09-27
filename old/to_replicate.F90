program to_replicate
    ! want to have this sort of behaviour but with just one binary
    use iso_fortran_env, only: stdout => output_unit
    implicit none
    ! in practice the following would be a #ifdef
#if 1
    real :: x,y,z
    x = 1
    y = 2
#else
    complex :: x,y,z
    x = cmplx(1,0)
    y = cmplx(2,0)
#endif

    z = x + y
    write(stdout,*) z

end program to_replicate