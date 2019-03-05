subroutine househ(a,b,deter)

    implicit none
  real, dimension(:,:),intent(inout) :: a !matriz del SEL
  real, dimension(:), intent(inout) :: b !termo independente do SEL
  real, intent(inout) :: deter
  real :: alpha, beta, q,p
  integer :: n,i,j,k

    n = size(b)

    do i=1,size(b)
        print*, a(i,i)
    end do

    do k=1,n-1
        alpha = 0
        do i = k,n
            alpha = alpha + a(i,k)*a(i,k)
        end do
        alpha = sqrt(alpha)
        if(a(k,k) > 0) then
            alpha = -1*alpha
        end if
        a(k,k) = a(k,k) - alpha
        do j = k+1,n
            q = 0
            do i = k,n
                q = q + a(i,k)*a(i,j)
            end do
            p = q/beta
            do i = k,n
                a(i,j) = a(i,j) - p*a(i,k)
            end do
        end do
        q = 0
        do i=k,n
            q = q + a(i,k)*b(i)
        end do
        p = q/beta
        do i=k,n
            b(i) = b(i) - p*a(i,k)
        end do
    end do

    deter = 1

    do i = 1,n
        deter = deter*a(i,i)
    end do

end subroutine
