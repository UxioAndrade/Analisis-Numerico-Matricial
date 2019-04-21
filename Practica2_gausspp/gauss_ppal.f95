program gauss_ppal

use gausspp_interface
use sistupf_interface
use datasissim_interface
use residuo_interface

implicit none

real, dimension(:,:), allocatable :: a
real, dimension(:,:), allocatable :: a_copia
real, dimension(:), allocatable :: b
real, dimension(:), allocatable :: b_copia
real, dimension(:), allocatable :: u
real, dimension(:), allocatable :: r

integer, dimension(:), allocatable :: l

real :: deter

integer :: n, i, j

print*
print*, 'Introduce o orde do sistema: '
read*, n
print*

n = 6

print*
print*, 'O orde introduce e: ', n
print*

allocate(a(n,n), a_copia(n,n), b(n), b_copia(n), u(n), r(n), l(n))

u = 0

do i=1,n
  do j =1,n
    if(i == j) then
      a(i,i) = 2
    else
      a(i,j) = j - i
    end if
  end do
  print *, a(i,:)
end do

do i =1,n
  b(i) = 2*i
end do

a_copia = a

b_copia = b

call gausspp(a, b, l, deter)

print*, 'Matriz A:'
do i=1,n
  print*, a(i,:)
end do

print *, 'Vector l:'
print*, l(:)

print *, 'B é:'
print*, b(:)

print *, 'Determinante :)'
print *, deter

call sistupf(a, b, l, u)
call residuo(a_copia, b_copia, u, r)

print*,
print*, 'Vector u (Solucion): '
print*,

print*,
print*, u
print*,

if (allocated(a)) deallocate(a)

if (allocated(b)) deallocate(b)

if (allocated(u)) deallocate(u)

if(allocated(r)) deallocate(r)

if(allocated(a_copia)) deallocate(a_copia)

end program
