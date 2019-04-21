program residuo_ppal

use datasissim_interface
use residuo_interface

implicit none

real, dimension(:,:), allocatable :: a
real, dimension(:), allocatable :: b
real, dimension(:), allocatable :: u
real, dimension(:), allocatable :: r

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

allocate(a(n,n), b(n), u(n), r(n))

!call datasissim(a, b, u)

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
print *, b(:)
u(1) = -1
u(2) = -1
u(3) = -0.5
u(4) = 0.5
u(5) = 0.5
u(6) = 1

call residuo(a, b, u, r)

if (allocated(a)) deallocate(a)

if (allocated(b)) deallocate(b)

if (allocated(u)) deallocate(u)

if(allocated(r)) deallocate(r)

end program
