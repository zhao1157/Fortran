program PI_RATE_try
    implicit none
    
    character*20 ::name_xsectn
    integer :: nline, i
    integer(kind=16), allocatable, dimension(:) :: line
    real(kind=16) :: ion_dens, temp
    
    open(unit=1, file='PI_sta_line_in')
    read(1,*), name_xsectn
    read(1,*), nline
    allocate(line(nline))
    
    
    read(1,*), ion_dens
    read(1,*), temp
    
    do i = 1, nline
        read(1, *), line(i)
        call PI_rate_x(name_xsectn, line(i), ion_dens, temp)
    end do
    
    close(unit=1)
    
    deallocate(line)
end program PI_RATE_try

subroutine PI_rate_x(name_xsectn, linei, ion_dens, temp)
    implicit none
    
    integer(kind=16), intent(in) :: linei
    character*20, intent(in) :: name_xsectn
    real(kind=16), intent(in) :: ion_dens, temp
    
    integer :: nreg, sum_nmesh
    integer(kind=16) :: i
    integer, allocatable, dimension(:) :: nmesh
    real(kind=16), allocatable, dimension(:) :: mesh, energy, xsectn, intgd, pi_rate_reg
    real(kind=16) :: pi_rate_total = 0.0
    character*30 :: head_out
    
    
    open(unit=2, file='PI_mesh_in')
    read(2,*), nreg
    allocate(nmesh(nreg), mesh(nreg), pi_rate_reg(nreg))
    
    
    do i = 1, nreg
        read(2, *), nmesh(i), mesh(i)
    end do
    close(unit=2)
    
    sum_nmesh=sum(nmesh)
    allocate(energy(sum_nmesh+nreg), xsectn(sum_nmesh+nreg), intgd(sum_nmesh+nreg))
    
    open(unit=3, file=name_xsectn)
    do i=1, linei-1
        read(3,*)
    end do

    read(unit=3, fmt='(a)'), head_out    

    do i = 1, sum_nmesh+nreg
        read(unit=3, fmt='(2x, 1p, e15.9, 21x, 1p, e9.3)'), energy(i), xsectn(i)
    end do
    
    close(unit=3)
!finished collecting energy and cross section, going to calculate the array of integrand, intgd.
    call integrand(energy, xsectn, sum_nmesh, nreg, ion_dens, temp, intgd)
!intgd is done!

    do i = 1, nreg
        call reg_simpson( intgd( sum(nmesh(1:i-1))+i : sum(nmesh(1:i))+i ), mesh(i), nmesh(i)+1, pi_rate_reg(i) )
        pi_rate_total = pi_rate_total + pi_rate_reg(i)        
    end do 
    
    do i = 1, nreg-1
        pi_rate_total = pi_rate_total + sum( intgd( sum(nmesh(1:i))+i : sum(nmesh(1:i))+i+1 ) )&
        &/2*( energy(sum(nmesh(1:i))+i+1) - energy(sum(nmesh(1:i))+i) )
    end do
    
    open(unit=5, file='PI_rate_out', position='append')
    write(unit=5, fmt='(a25, 4x, 1p, e19.13)'), head_out, pi_rate_total
    close(unit=5)
    

    deallocate(nmesh, mesh, energy, xsectn, pi_rate_reg, intgd)
end subroutine PI_rate_x


subroutine reg_simpson(intgd_reg_i, mesh_reg_i, bins_reg_i, pi_rate_reg_i)
    implicit none
    
    real(kind=16),dimension(bins_reg_i), intent(in):: intgd_reg_i
    real(kind=16), intent(in) :: mesh_reg_i
    integer, intent(in) :: bins_reg_i
    real(kind=16), intent(out) :: pi_rate_reg_i
    
    real(kind=16) :: sum_intgd=0.0
    integer :: i
    
    if ( mod(bins_reg_i-1,2)==0 ) then
        sum_intgd = sum_intgd + 2*sum(intgd_reg_i) - intgd_reg_i(1) - intgd_reg_i(bins_reg_i)
        do i = 2, bins_reg_i-1, 2
            sum_intgd = sum_intgd + 2*intgd_reg_i(i)
        end do
        pi_rate_reg_i = mesh_reg_i/3*sum_intgd
    else
        sum_intgd = sum_intgd + 2*sum( intgd_reg_i(1:bins_reg_i-1) ) - intgd_reg_i(1) - intgd_reg_i(bins_reg_i-1)
        do i = 2, bins_reg_i-2, 2
            sum_intgd = sum_intgd + 2*intgd_reg_i(i)
        end do
        pi_rate_reg_i = mesh_reg_i/3*sum_intgd + mesh_reg_i/2*sum( intgd_reg_i(bins_reg_i-1 : bins_reg_i) )
    end if   
end subroutine reg_simpson



subroutine integrand(energy, xsectn, sum_nmesh, nreg, ion_dens, temp, intgd)
    implicit none
    
    real(kind=16), intent(in) :: ion_dens, temp
    integer, intent(in) :: nreg, sum_nmesh
    real(kind=16),dimension(sum_nmesh+nreg), intent(in):: energy, xsectn
    real(kind=16),dimension(sum_nmesh+nreg), intent(out):: intgd
    
    double precision, parameter :: pi = 2*asin(1d+0), h=4.135667662d+0, kb=8.6173324d-05, ry=13.60569253d+0
    integer(kind=16), parameter:: c=299792458
    
    intgd = ( 2*(energy*ry/h)**2*xsectn*ion_dens )/( c**2*pi*( exp( energy*ry/(kb*temp) )-1 ) )
    
end subroutine integrand



