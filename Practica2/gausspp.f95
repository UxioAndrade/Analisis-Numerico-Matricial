subroutine gausspp(a,b,ip,deter)
  integer :: i,k
  deter=1.
  !inicializacion da permutacion de filas
  l=(/(i,i=1,n)/)
!etapa k-esima da eliminacion
  do k=1,n-1
    !busqueda do pivote e
  !da fila na que se atopa
  piv=a(l(k),k)
  p=k
  do i=k+1,n
    if(abs(piv)<abs(a(l(i),k)))then
      piv=a(l(i),k)
      p=i
    end if
  end do
  !comprobacion de que o
  !k-esimo pivote non e nulo
  if(abs(piv)<1.e-12) then
    print*,’pivote nulo na etapa: ’,k
    print*,’A matriz  e singular!’
    stop
  end if
  !posta ao dia da permutacion
  deter=deter*piv
  if (p /= k) then
    deter = -deter
    m=l(k)
    l(k)=l(p)
    l(p)=m
  endif
  piv=a(l(k),k)
  lk=l(k)
  !Eliminacio ́n
  do i=k+1,n
    li=l(i)
    z=a(li,k)/piv
    do j=k+1,n
      a(li,j)=a(li,j)-z*a(lk,j)
    end do
    b(li)=b(li)-z*b(lk)
    end do
  end do
  !Remate do calculo do determinante.
  piv=a(l(n),n)
  deter=deter*piv
  if(abs(piv)<1.e-12) then
    print*,’pivote nulo na etapa: ’,n
    print*,’A matriz e singular!’
    stop
  end if
