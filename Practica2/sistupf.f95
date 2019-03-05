subroutine sistupf
  u(n)=b(l(n))/a(l(n),n)
   do i=n-1,1,-1
     u(i)=b(l(i))
    do j=i+1,n
     u(i)=u(i)-a(l(i),j)*u(j)
    end do
   u(i)=u(i)/a(l(i),i)
  enddo
end subroutine
