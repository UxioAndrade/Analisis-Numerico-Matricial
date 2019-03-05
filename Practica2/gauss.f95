subroutine gauss(a,b,deter)

  implicit none
  real, dimension(:,:),intent(inout) :: a !matriz del SEL
  real, dimension(:), intent(inout) :: b !termo independente do SEL
  real, intent(inout) :: deter
  real :: z
  integer :: n,i,j,k

  n = size(b)

  do k =1,n-1
    do i = k+1,n
      z = a(i,k)/a(k,k)
      a(i,k) = 0
      do j = k+1, n
        a(i,j) = a(i,j) - z * a(k,j)
      end do
    b(i) = b(i) - z*b(k)
    end do
  end do

  deter = 1

    do i = 1,n
        deter = deter*a(i,i)
    end do

end subroutine
