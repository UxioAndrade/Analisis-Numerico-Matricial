subroutine sistub(a,b,u)

  implicit none

  real, dimension(:,:), intent(in) :: a !matriz do SEL
  real, dimension(:), intent(in) :: b !termo independiente do SEL
  real, dimension(:),intent(out) :: u !solucion do SEL

  integer :: n,i,j

  u=b

  n = size(b)

  do i=n,1,-1
    u(i)=u(i)/a(i,i)
    u(1:i-1)=u(1:i-1)-a(1:i-1,i)*u(i)
  end do

  print*,u

end subroutine
