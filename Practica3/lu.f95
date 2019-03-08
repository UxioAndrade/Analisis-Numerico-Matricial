subroutine lu(a,deter)

    implicit none 
    real, dimension(:,:), ALLOCATABLE :: u
    real, dimension(:,:) :: a
    real, dimension(:,:),allocatable :: l
    real :: deter
    INTEGER :: j,r,i,k,n

    n = SIZE(a(1,:))

    ALLOCATE(u(n,n),l(n,n))

    if(abs(a(1,1))<1.e-12) then
        print*, 'pivote 1 nulo'
        STOP
    end if

    do j=1,n
        u(1,j) = a(1,j)
    end do
    do r=2,n
        l(r,1) = a(r,1)/u(1,1)
    end do
    do i=2,n
        do j= i,n
            u(i,j) = a(i,j)
            do k=1,i-1
                u(i,j) = u(i,j) - l(i,k)*u(k,j)
            end do
        end do
        do r = i+1,n
            l(r,i) = a(r,i)
            do k = 1,i-1
                l(r,i) = l(r,i) - l(r,k)*u(k,i)
            end do
            l(r,i) = l(r,i)/u(i,i)
        end do
        if(abs(a(i,i))<1.e-12) then
            print*, 'pivote nulo en la etapa', i
            STOP
        end if
    end do

    deter = a(1,1)

    do i=2,n
        deter = deter*a(i,i)
    end do

    write(*,*) 'Matriz U'
    do i=1,n
        WRITE(*,*) u(i,:)
    end do
    write(*,*) 'Matriz L'
    do i=1,n
        write(*,*) l(i,:)
    end do

    if (allocated(u)) deallocate(u)

    if (allocated(l)) deallocate(l)

end subroutine