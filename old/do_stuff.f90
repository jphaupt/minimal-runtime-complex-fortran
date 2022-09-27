program do_stuff
    use real_or_complex, only: real_number_t, complex_number_t, number_t
    implicit none

    class(number_t), allocatable :: x, y, z
    logical :: is_complex = .true.

    if (is_complex) then
        x = complex_number_t()
        y = complex_number_t()
        z = complex_number_t()
    else
        x = real_number_t()
        y = real_number_t()
        z = real_number_t()
    endif

    ! x = 1.
    ! if(is_complex) then
    !     y = cmplx(2,3)
    ! else
    !     y = 2.
    ! endif
    ! y = 2.
    ! z = x - y
    ! z = z + 1.
    ! z%print()

end program do_stuff
