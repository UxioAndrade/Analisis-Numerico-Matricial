subroutine residuo(a,b,u,r)
  implicit none
  real, dimension(:,:), intent(in) :: a !matriz do SEL
  real, dimension(:), intent(in) :: b !termo independiente do SEL
  real, dimension(:),intent(in) :: u !solucion do SEL
  real, dimension(:), intent(out) :: r !residuo do SEL

  real, dimension(size(b)) :: aux
  integer :: i

  aux = MATMUL(a,u)

  do i = 1, size(b)
    r(i) = aux(i) - b(i)
  end do

end subroutine
