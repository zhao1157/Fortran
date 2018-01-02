!How to merge the module with the main program? gfortran -c ran_mod.f95; gfortran -c fortran_learning.f95;
!gfortran ran_mod.o fortran_learning.o -o exe; ./exe

module ou !In the main program, at the very beginning, use ou instead of use ran_mod.
    contains

    integer function int_mod(i)
        implicit none
        
        integer, intent(in) :: i
        
        int_mod=i
        
    end function int_mod
    
    subroutine retu_arr(arr)
        implicit none
        
        integer, dimension(3), intent(out):: arr 
        integer :: i
        
        do i=1, 3
            arr(i)=i
        end do
    end subroutine retu_arr
end module ou
