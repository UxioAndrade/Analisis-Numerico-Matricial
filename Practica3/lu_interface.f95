module lu_interface
  interface
    subroutine lu(a,deter)
      implicit none
      real, dimension(:,:),intent(inout) :: a !matriz del SEL
      real, intent(inout) :: deter
    end subroutine lu
  end interface
end module
