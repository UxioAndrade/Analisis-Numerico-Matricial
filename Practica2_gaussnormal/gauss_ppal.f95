program gauss_ppal

use gaussb_interface
use sistub_interface
use datasissim_interface
use residuo_interface

implicit none

real, dimension(:,:), allocatable :: a
real, dimension(:,:), allocatable :: a_copia
real, dimension(:), allocatable :: b
real, dimension(:), allocatable :: b_copia
real, dimension(:), allocatable :: u
real, dimension(:), allocatable :: r

real :: deter

integer :: n, i, j

print*
print*, 'Introduce o orde do sistema: '
read*, n
print*

print*
print*, 'O orde introduce e: ', n
print*

allocate(a(n,n), a_copia(n,n), b(n), b_copia(n), u(n), r(n))

u = 0

call datasissim(a, b)

a_copia = a

b_copia = b

call gaussb(a, b, deter)

write(*,*) 'Matriz A:'

do i = 1,n
  print*, a(i,:)
end do

call sistub(a, b, u)

call residuo(a_copia, b_copia, u, r)

print*,
print*, 'Vector u (Solucion): '
print*,

print*,
print*, u
print*,

if (allocated(a)) deallocate(a)

if (allocated(a_copia)) deallocate(a_copia)

if (allocated(b)) deallocate(b)

if (allocated(b_copia)) deallocate(b_copia)

if (allocated(u)) deallocate(u)

if(allocated(r)) deallocate(r)

end program
