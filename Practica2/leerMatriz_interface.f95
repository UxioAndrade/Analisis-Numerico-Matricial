module leerMatriz_interface
  interface
    subroutine leerMatriz(a,b)
      implicit none
      real, dimension(:,:),intent(out) :: a !matriz del SEL
      real, dimension(:), intent(out) :: b !termo independente do SEL
    end subroutine leerMatriz
  end interface
end module
