module gauss_interface
  interface
    subroutine gauss(a,b,deter)
      implicit none
      real, dimension(:,:),intent(inout) :: a !matriz del SEL
      real, dimension(:), intent(inout) :: b !termo independente do SEL
      real, intent(inout) :: deter
    end subroutine gauss
  end interface
end module
