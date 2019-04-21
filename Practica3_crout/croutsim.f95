subroutine croutsim(a, deter)
  real, dimension(:,:), intent(inout) :: a
  real, intent(out) :: deter

  integer :: n, i, j, k

  n = size(a(:,1))

  do i=2,n
    a(i,1) = a(i,1) / a(1,1)
  end do

  deter = a(1,1)

  do j = 2, n
    do k=1, j-1
      a(j,j) = a(j,j) - a(j,k)*a(j,k)*a(k,k)
    end do

    deter = deter*a(j,j)

    do i= j+1, n
      do k=1, j-1
        a(i,j) = a(i,j) - a(i,k)*a(k,k)*a(j,k)
      end do

      a(i,j) = a(i,j)/a(j,j)
    end do
  end do

deter = a(1,1)

do i=2,n
	deter = deter*a(i,i)
end do


end subroutine
