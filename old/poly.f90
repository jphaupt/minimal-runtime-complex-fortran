! taken and modified from stack exchange
! Main
program poly
    implicit none
    type, abstract :: MyType
        class(*), allocatable :: AllData(:)
    contains
        ! procedure, public :: Constructor
    endtype MyType

    type(MyType) :: t1
    integer :: i, nsize = 4
    logical :: if_complex = .true.
    if(if_complex) then
        allocate(complex(8)::t1%AllData(Nsize))
    else
        allocate(real(8)::t1%AllData(Nsize))
    endif
    ! call t1%Constructor(4, .true.)
    call Square(t1%AllData)

contains
    subroutine Square(Array)
        class(*), intent(inout) :: Array(:)
        select type(Array)
        type is(real(8))
            Array = Array**2
        type is(complex(8))
            Array = Array**2
        endselect
    endsubroutine Square

    ! subroutine Constructor(this, Nsize, IfComplex)
    !     class(MyType), intent(inout) :: this
    !     integer, intent(in) :: Nsize
    !     logical, intent(in) :: IfComplex
    !     if(IfComplex) then
    !         allocate(complex(8)::this%AllData(Nsize))
    !     else
    !         allocate(real(8)::this%AllData(Nsize))
    !     endif
    ! endsubroutine
endprogram poly
