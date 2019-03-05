module residuo_interface
  interface
    subroutine residuo(a,b,u,r)
      implicit none
      real, dimension(:,:), intent(in) :: a !matriz do SEL
      real, dimension(:), intent(in) :: b !termo independiente do SEL
      real, dimension(:),intent(in) :: u !solucion do SEL
      real, dimension(:), intent(out) :: r !residuo do SEL
    end subroutine
  end interface
end module
