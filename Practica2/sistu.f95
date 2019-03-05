subroutine sistu(a,b,u)

  integer :: n,i,j
  real, dimension(:,:), intent(in) :: a !matriz do SEL
  real, dimension(:), intent(in) :: b !termo independiente do SEL
  real, dimension(:),intent(out) :: u !solucion do SEL

  n = size(u)

  u(n) = b(n)/a(n,n)

  do i = n-1, 1, -1
    u(i) = b(i)
    do j = i+1,n
      u(i) = u(i) - a(i,j) * u(j)
    end do
    u(i) = u(i)/a(i,i)
  end do

end subroutine
