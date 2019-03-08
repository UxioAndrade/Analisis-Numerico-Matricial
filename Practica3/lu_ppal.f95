program lu_ppal

    use leerMatriz_interface
    use sistu_interface
    use residuo_interface
    use lu_interface

    implicit none
    real, dimension(:,:), allocatable :: a
    real, dimension(:), allocatable :: b
    real, dimension(:,:), allocatable :: aa
    real, dimension(:), allocatable :: bb
    real, dimension(:), allocatable :: r
    real, dimension(:), allocatable :: u
    real :: deter

    integer :: n,i

    write(*,*) "Introduce a orde do sistema a tratar:"
    read(*,*) n
    write(*,*) "O sistema é de orde ", n

    allocate(a(n,n),b(n),u(n),r(n),aa(n,n),bb(n))

    u = 0

    call leerMatriz(a, b)
    aa = a
    bb = b
    call lu(a,deter)
    do i = 1,n
        u(i) = a(i,i) !salvamos a diagonal de u
        a(i,i) = 1 !Poñemos 1 a diagonal de l
    end do

    !hai que facelo con sistul

    !call sistu(a, b, u)
    !call residuo(aa,bb,u,r)

    !do i=1,size(u)
     !   print*, r(i)
    !end do

    !do i=1,size(u)
    !    print*, u(i)
    !end do

    print *, 'Diagonal principal A:'

    do i=1,size(u)
        print*, a(i,i)
    end do

    print *, 'Determinante A' , deter

    if (allocated(a)) deallocate(a)

    if (allocated(b)) deallocate(b)

    if (allocated(u)) deallocate(u)

    if (allocated(r)) deallocate(r)

    if (allocated(aa)) deallocate(aa)

    if (allocated(bb)) deallocate(bb)

end program