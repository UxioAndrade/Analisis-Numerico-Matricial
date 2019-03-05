module househ_interface
    interface
      subroutine househ(a,b,deter)
        implicit none
        real, dimension(:,:),intent(inout) :: a !matriz del SEL
        real, dimension(:), intent(inout) :: b !termo independente do SEL
        real, intent(inout) :: deter
      end subroutine househ
    end interface
  end module
  