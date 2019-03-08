subroutine leerMatriz(a,b)
  implicit none
  real, dimension(:,:), intent(out) :: a !matriz do SEL
  real, dimension(:), intent(out) :: b !termo independiente do SEL

  integer :: n,i

  open(10,file="inputMatrix.in")

  n = size(b)

  do i = 1,n
    read(10,*) a(i,1:n)
  end do

  read(10,*) b

  close(10)

end subroutine
