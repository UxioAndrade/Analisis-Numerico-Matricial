program sistu_ppal

  use sistu_interface
  use leerMatriz_interface
  use sistub_interface

  implicit none

  real, dimension(:,:), allocatable ::  a
  real, dimension(:), allocatable :: b
  real, dimension(:), allocatable :: u

  integer :: n,i

  write(*,*) "Introduce a orde do sistema a tratar:"
  read(*,*) n
  write(*,*) "O sistema é de orde ", n

  allocate(a(n,n),b(n),u(n))

  u = 0

  call leerMatriz(a, b)
  call sistub(a, b, u)

  do i=1,size(u)
    print*, u(i)
  end do

  if (allocated(a)) deallocate(a)

  if (allocated(b)) deallocate(b)

  if (allocated(u)) deallocate(u)

end program
