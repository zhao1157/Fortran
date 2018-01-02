!========================




!!=================
!!This program is to test the roundoff of float number.
!program he
!    implicit none
    
!    real :: a=2.3448
    
!    print '(f4.2,2x, f5.3)', a, a
!end program he


!!=======================
!program read_data
!    implicit none
    
!    integer, dimension(4,2) :: grd_data
!    integer :: i, r
    
!    open (unit=5, file='a40', status='old')
!    do i = 1, 3
!        read(5,*)
!    end do

!    do r = 1, 4
!        read (5, fmt='(t5, i2, t8, i1)') grd_data(r, 1), grd_data(r, 2)
!    end do

!    print *, grd_data
!end program read_data


!!=====================
!program he
!    implicit none
!    integer :: a=2
    
!    if (a) 10,20,30
    
!    30 write(*,*) 'positive'
!    a=a-1
    
!    if (a) 10,20,30
    
!    20 write(*,*) "zero"
!    a=a-1
    
!    if (a) 10,20,30
    
!    10 write(*,*) 'negative'

!end program he

!!===============================
!!This program is to test the .eq. and == operators.
!program he
!    implicit none
    
!    if ('a'.eq.'c') then
!        print *, "equal"
!    else
!        print *, "unequal"
!    end if
    
!end program he


!!===============================
!!This program is to test the return statement in 
!!a function when called
!program he
!    implicit none
    
!    call xyz
!    print *, "after xyz"

!end program he

!subroutine xyz

!    if (22 .eq. 2) return
!    print *, "In xyz"
    
!end subroutine xyz


!!==============================
!program he
!    implicit none
    
!    character, parameter :: a(2)=(/'h', 'e'/)
!    character*3 :: b(2)=(/ a(1)//'d', a(2)//'m' /)

!    print *, b(1), b(2)


!end program he


!!===============================
!program he
    
!    a=2
!    a=-a
    
!    print *, a

!end program he




!!================================
!program he
!    implicit none
!    integer :: i
    
!    open(2, file='he', status='unknown')
!    write(2,*) 2, 3, (i, i= 4, 8)
!    close(2, status='keep')

!end program he



!!===============================
!!This program is to test the execution of linux command in fortran.
!program he
!    call system('ls ~/Desktop')
!end program he


!!=================================================
!!This is used to test the comma separated data. 
!!It turns out the comma is just like the space between
!!two adjacent data values.
!program he
!    implicit none
    
!    integer :: a, b
    
!    read (*,*) a, b
    
!    print *, a, b
!end program he



!!==================================================
!!In a more than one character string, we can access random character by
!!a(n:n), a(1:1), a(3:3),or a sequence of characters, e.g. a(2:4).
!program he
!    implicit none
    
!    character*5 :: a='abcde'

!    print '(3(a, x))', a(2:2), a(4:4), a(2:4)

!end program he


!!=================================================
!!This program is to test the open statement without
!!explicitly opening the file in the program.
!!In standard fortran, unit=5 and unit=6 are reserved for
!!screen input and output respectively. But * can be used to 
!!replace these two unit numbers, so there is no need to remember
!!which unit number is for which purpose, reading from or writing to.
!!In ifort, the standard fortran restrictions are followed, but in 
!!gfortran these two unit numbers are not reserved.
!!So in linux command, we can set a file as the screen, from which or to 
!!which we can read or write data.

!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!!+run .exe < open_without > output: read the data in open_without in the+
!!+program without the explicit open statement, and write the output to  +
!!+output file without the explicit open statement.                      +
!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!program he
!    implicit none
!    integer :: a
    
!    read(2,*) a
!    close (2)
    
!    write(8,*) a
!    close(8)

!end program he


!!=================================================
!program he
!!This program is to test the concatenate statement.
!    integer :: a(2:4)
!    character*2 :: b='he', c='sd'
    
!    a=2
    
!    print *, a
    
!    print *, b//c

!end program he


!!=================================================
!program he
!    implicit none
    
!    integer :: valu(8), time(3), year, month(2)
!    call date_and_time(values=valu)
!    time=valu(5:7)
!    month=valu(2:3)
!    year=valu(1)
!    print '(6i5)', time, month, year
!end program he



!!======================
!program he
!    use omp_lib
    
!    implicit none
!    integer :: i, a(20), funit
    
!    open(unit=2, file='dat/he', status='replace')
!    write(2, '(i2/, i2/, i2/, i2)') 1, 3, 5, 7
!    close(2)

!    !$omp parallel do num_threads(20) private(funit)
!    do i = 1, 20
!        funit=100+omp_get_thread_num()
!        open(funit, file='dat/he', status='old', action='read')
!            !rewind 2
!        read(funit,'(i2)') a(i)
!    end do
!    !$omp end parallel do
    
!    print *, a
!end program he


!!=================================
!program he
!    implicit none
    
!    open (5, file='dat/he', status='replace')
!    write(5, '(a)', advance='no') new_line(' ')
!    write(5, '(a//)') 'hello'
!    write(5, '(a)', advance='no') 'Seth'
!    write(5, *) new_line(' ')
!    write(5, '(a)') '!'
!    close(5)
!end program he


!!====================================
!!This program is to test the direct access of a file.
!program he
!    implicit none
    
!    integer :: recs, lenrec
!    character * 40 :: ch
    
!    print *, 'Please enter the length of the record: '
!    read *, lenrec
    
!    open(unit=5, file='dat/he', status='replace', access='direct', &
!    &recl=lenrec, form='formatted')
    
!    write(5, '(a)', rec=10) 'Seth Zhao'
!    write(5, '(a)', rec=20) '04/02/1990'
!    write(5, '(a)', rec=30) 'On Earth'
    
!    write(*,'(a)', advance='no') 'which record would you like to see : '
!    read(*,'(i2)') recs
    
!    read(5, '(a)', rec=recs) ch
    
!    print *, ch

!end program he


!!======================================
!!This program is to test the IOLENGTH of an unformatted data.
!program  he
!    implicit none
    
!    integer :: flength(3), i=2
!    real :: j=2.3
!    logical :: a=.true.
!    character*3, dimension(3) :: b
!    print *, len(b), sizeof(.true.)
    
!!LEN() calculate the length of a string or an array of strings. sizeof()
!!finds the number of bytes of a variable.

!    inquire (iolength=flength(1)) i
!    inquire (iolength=flength(2)) j
!    inquire (iolength=flength(3)) a
!    print *, flength
!end program he

!!========================================
!program he
!    implicit none
    
!    integer :: i=1, a(4), j, frecl, frec(4)
    
!    open(unit=2, file='dat/he', status='replace', access='direct',&
!            &recl=2, form='formatted')
!        do i =  1, 4
!            write(2, '()', rec=i*3)
            
!            inquire(2, nextrec=frec(i))
!        end do
!    close(2)  
!!Nextrec is retrieve the number of record, but 1 more than the 
!!previous record number.
!    print *, frec
        
        
!!    iteration: do j = 1, 2
!!    writeinfile: do
!!        write(2, '(i2)', rec=i+j*9) i
!!        if (i==4) exit
!!        i=i+1
!!    end do writeinfile
!!    i= i -4
!!    end do iteration    
!end program he


!!=======================================
!!Just found it in the thick book saying the new program will never need
!!it.
!program he
!    implicit none
    
!    integer :: i, a(4)
!    character*10 :: ch
    
!    open(unit=2, file='dat/he', status='replace', blank='zero')
!    write(2, '(1x, i2, 2x i2)') 4, 5
!    inquire(2, blank=ch)
!    close(2)
    
!    open(3, file='dat/he', status='old', blank='zero')
!    read(3, *) a(1), a(2)
!    close(3)
    
!    print *, a(1), a(2)

!end program he


!!=====================
!!This program is to test the inquire statement.
!program he
!    implicit none
    
!    integer :: fopen(2), fpos(2)
!    logical :: fex, fop(2), fname(2)
!    character*10 :: fnames(2), faccess(2)*20, fdirect(2), fsequential(2),&
!                    &fstream(2), fform(2), funformatted(2), fposition(2),&
!                    &fencoding(2), faction(2), fblank(2)
    
!    open (unit=2, file='dat/he', status='replace', action='readwrite', position='rewind',&
!        &blank='zero')
        
!        inquire(file='dat/he', exist=fex, opened=fop(1), number=fopen(1),&
!        &named=fname(1), name=fnames(1), access=faccess(1),&
!        & direct=fdirect(1), sequential=fsequential(1),stream=fstream(1),&
!        & form=fform(1), unformatted=funformatted(1), position=fposition(1),&
!        & pos=fpos(1), encoding=fencoding(1), action=faction(1), blank=fblank(1) )
        
!    !write(2, *) 2
!    close(2)
    
!    inquire(file='dat/eh', exist=fex, opened=fop(2), number=fopen(2),&
!            &named=fname(2), name=fnames(2), access=faccess(2),&
!            & direct=fdirect(2), sequential=fsequential(2), stream=fstream(2),&
!            & form=fform(2), unformatted=funformatted(2), position=fposition(2),&
!            &action=faction(2), blank=fblank(2))
    
!    print *, fex, fop, fopen, fname , fnames, faccess, fdirect, fsequential,&
!            &fstream, fform, funformatted, fposition, fpos(1), fencoding(1),&
!            &faction, fblank
!!To get the unit number of a file, use number=int_var to get the unit,
!!which is positive for a opened file, and undefined (-1) for an unopened
!!file.
!end program he


!!=============================
!program he
!    implicit none
    
!    character *10 :: cha
!!If unformatted file is desired, make sure it is specified explicitly
!!in the open statement.    
!    open(unit=2, file='dat/he', form='unformatted', status='replace')
!    write(2) 2
!    inquire(2, form=cha) !This gives the unformatted form.
!    close(2)
!    !inquire(file='dat/he', form=cha) !This gives undefined form.
!    print *, cha
!!Unformatted is used so the content in the file is binary, whicle 
!!formatted is used so the content is readable for humans.
!end program he



!!========================
!program he
!    implicit none
    
!    integer :: i, a(4)
!    logical :: cha
    
!    open(unit=3, file='dat/he', status='replace')
!    !do i = 1, 4
!    write(3, '(i2)') 1
!    !end do
!    close(3)
    
!    open(unit=3, file='dat/he', status='old', position='append')
!    !do i = 1, 4
!    write(3, '(i2)') 2
    
!    !end do
!    close(3)
    
!    inquire(file='dat/he', exist=cha)
!!    open(3,file='dat/he', access='direct', recl=2, form='formatted')
!!    read(3, '(i2)', rec=2) a(1)
!!    close(3)
    
!!    print *, a
!    print *, cha
!end program he


!!==========================
!program he
!    implicit none
    
!    integer :: i, a(4)=9
    
!    open(unit=2, file='dat/he', status='replace', access='direct', recl=2, form='formatted')
!    do i = 1, 4
!        write(2, '(i2)', rec=2*i) i
!    end do
!    close(2)
    
!    open(5, file='dat/he')
!    !do i = 1, 4
!        read(5, '(2x, i2)') a(1)
!        !read(5, '(3x, i2)') a(2)
!        !read(5, '(10x, i2)') a(3)
!        !read(5, '(14x, i2)') a(4)
!    !end do
!    close(5)
!!For a file that is written using direct access, can be read just one 
!!value in a open-close circle, can not be read more than once.    
!    print *, a
!end program he




!!=====================================
!program he
!    implicit none
    
!    integer :: i, a(4)
    
!    open(2, file='dat/rec', access='direct', recl=8,form='formatted', status='replace')
!!    do i = 1, 4
!!        write(2,'(4i2)', rec=3) i
!!    end do
    
!    write(2, '(4i2)', rec=3), (i, i= 1,4)
    
!    close(2)

!    open(5, file='dat/rec', access='direct', recl=8, form='formatted', status='old')
!    read(5, '(4i2)', rec=3) a
!    close(5)
    
!    print *, a
!end program he


!!=====================================
!program he
!    implicit none
    
!    integer :: i
    
!    do 100 i = 1, 4
!        if (i==2) go to 100
!        print *, "inside", i
! 100 continue   
!!The above go to statement is to make
!    print *, "outside", i
    
!end program he



!=====================================
!program he
!    use omp_lib
    
!    implicit none
    
!    integer :: i, a(3), ios, b
    
!    common /dat/ a
!    !$omp threadprivate(/dat/, b)
!    save
    
!    open(2, file='he', status='replace')
!    write(2, '(i2/, i2/, i2)') 2, 3, 4
!    close(2)
!    open(5, file='he', status='old', iostat=ios)
!    !$omp parallel do
!    do i = 1, 3
!        !$omp single
!        read(5,*) a(i)
!        print *, "i= ", i, a(i), omp_get_thread_num()
!        !$omp end single copyprivate(a)
        
!        print *, "thread: ", omp_get_thread_num(), a(i)
!    end do
!    !$omp end parallel do
!    close(5)
    
!    !print *, a, ios

!end program he


!!======================
!program he
!    use omp_lib
    
!    implicit none
    
!    integer :: i, a(6)
    
!    open(unit=3, file='dat/rewind', status='replace', action='write')
!    write(3, fmt='(i2)') (i, i=1, 6)
!    endfile 3
!    close(3)
!!This is weird, gfortran does not support reading one file in different
!!unit number sulmultneously, while ifort does.
!    open(5, file='dat/rewind', position='rewind')
!    open(6, file='dat/rewind', position='rewind')
!    backspace 6
!    read(5, *) a(1)
!    read(5, *) a(2)
!    read(6, *) a(6)
!    close(5)
!    close(6)
!    print *, a
!end program he




!!======================
!program he
!    use omp_lib
    
!    integer :: i, a(6)
    
!    open(unit=2, file='dat/share', status = 'replace', action='write', &
!    & recl=1, form='formatted', access='direct')
!    do i = 1, 6
!    write(2, fmt='(i1)', rec=i) i
!    end do
!    !write(2, fmt='(i2)', advance='no') (i, i=1,4) does not write the 
!    !data in a row, but in column.
!    close(2)

!    open(unit=5, file='dat/share', access='direct', recl=2, form='formatted')
!    !do i = 1, 6
!        read(5, fmt='(i3)', rec=1), a(3)
!    !end do
!    close(5)
    
!    print *, a(3)
!end program he


!!========================
!!This program is to prove that the condition that the do loop is
!!running is essentially the while loop, checking the iteration number
!!with the maximum.
!program he
!    implicit none
    
!    integer:: i
    
!    do i = 1 ,5, 2
!    end do
    
!    print *, i

!end program he



!!=======================
!!This is to test the scope of exit.
!program he
!    implicit none
    
!    integer :: i, j
    
!    do i = 1, 4
!        do j = 1, 1
!            if (j==i) then
!                !print*, "j=i=", j
!                exit
!            end if
!        end do
        
!        print *,"j=i= ", j
!    end do
!end program he


!!=====================
!program he  
!    implicit none
!    integer :: a
!    open(unit=4, file='dat/hell', status='replace', action='write')
!    write(4, *) 1, 2, 3
!    write(4, *) 4, 5, 6
!    write(4, *) 7, 8, 9
!    close(4)
    
!    open(5, file='dat/hell', status='old', action='read')
!    do  !Without fixed number of iteraiton, just like while loop which can be done by combining 
!        !do loop and if conditions.
!        read(5, *) a
!        !print *, a
!        if (a == 7) then
!            print *, "Found!"
!            exit
!        end if
!    end do
!    close(5)
!end program he



!!====================
!!when opening a file: status='new', 'old', 'scratch', 'unknown', 'replace'
!!when closinga file, we can specify to delete the file by close(2,status='delete')
!!Access = 'sequential' which is default, 'direct' which is??
!!iostat = ios, to verify whether there is a problem existing when opening
!!a file.
!!access='direct', recl=23, form='formatted' are required when accessing it
!!form = 'formatted' and 'unformatted' for sequential and direct access respecitvely.
!!
!program he
!    implicit none
!!***********Direct access
!    integer :: i=0
!    integer :: a, b, c
!    open(unit=1, file='dat/hel', status='replace', access='direct', recl=12,&
!    &form='formatted')
!    do i = 10, 12
!        write(1, fmt='(i2, 1x, i3, 1x, i3, a)', rec=i-9) i-12, i-22, i-33
        
!    end do
!    close(1)
    
!    open(2, file='dat/hel', status='old', access='direct', recl=12,&
!    & form='formatted')
    
!    do i = 1, 3
!    read(2, fmt='(i2, 1x, i3, 1x, i3, a)', rec=i) a, b, c
!    print *, a, b, c
!    end do
!    !backspace (2) !You can not use backspace since the file is written
!    !randomly, use rec to locate the record.
!    !read(2, fmt='(i2, 1x, i2, 1x, i2)', rec=19) a, b, c
!    close(2)
    
!    open(4, file='dat/hel', status='old', access = 'direct', recl=20, form='formatted', action='write')
!    write(4, rec=3, fmt='(i1,i1,i1)') 1, 1, 1
!    close(4)
!!    open(3, file='dat/hel', status='old')
!!    do
!!       read(3, *) a, b, c
!!       if (a==-1) then
!!            print *,"found"
!!            exit
!!       end if
!!    end do 
!!!It seems we can not read a file which is written by direct method.
!!***********Direct access    
    
!!!*********status='unknown'
!!    integer:: a
!!    character*10 :: c='misseth'
    
!!    open(2, file=c, status="replace")
!!    write(2,*) 2
!!    close(2)
    
!!    open(3, file=c, status='unknown')
!!    write(3,*) 3
!!    close(3)
!!!**********status='unknown'
!end program he



!!===================
!!This is to test the scratch files which can be used as a temperary file
!!to contain some temperary data.
!program he
!    implicit none
    
!    integer :: i, a(5)
!    character :: c*10
    
!    open(2, status='scratch')
    
!    write(2, fmt='(5(i2/))') (i, i=1,5)
!    !write(2, fmt='(i2)') 8
!    !backspace 2
!    backspace 2
!    do i = 1, 5
!       ! backspace 2
!        backspace 2
!        read(2, fmt='(i2)') a(6-i)
!        backspace 2
!    end do
    
!    close(2)
    
!    print *, a
    
!    open(3, file='hello', status='replace')
!    write(3, *), 'hello'
!    backspace 3
!    read(3,*) c
    
!    close(3, status='delete')
    
!    print *, c

!end program he

!!====================
!program he
!    implicit none
    
!    integer :: a(3)=(/1, 2, 3/), b(3) = (/4, 5, 6/), c(3)=(/7, 8, 9/), i, ios
!!Here the status=replace is to overwrite the existing file.
!    open(2, file='tes', status='replace')
!    write(2, fmt='(i2, i2, i2)') (a(i), b(i), c(i), i = 1, 3)
!    close(2)
    
!    open(2,file='tes', status='old', iostat=ios)
!    print *, "ios: ", ios
!    do i = 1, 3
!        read(2,*) a(i), b(i), c(i)
!        if ( a(i)==2 ) then
!            print *, "found"
!!Here the backspace is to move the pointer back to its previous position.
!            backspace 2
!            write(2, *) 0, b(i), c(i)
!!Here the write statement overwrites the rest of the content.
!            exit
!        end if
!    end do
!    print *, "exit, i = ", i
!    close(2)
!end program he



!!============
!program he
!    implicit none
!    integer :: a(2)
    
!    open(5, file='tes', form='unformatted')
!    write(5) 11, 12
!    close(5)
    
!    open(1, file='tes', form='unformatted')
!    read(1) a
!    close(1)
    
!    print *, a

!end program he


!!=================
!program he
!    implicit none
!    integer :: i
!    print *, "before stop"
    
!    do i = 1, 4
!        print *, i
!        if(i==2) stop
!    end do
    
!    print *, "after stop"

!end program he



!!==============================================
!program he

!    implicit none
    
!    integer :: ios, u=2
    
!    open(u, file='hell', access='read', iostat=ios)
!    write(u, *) 2
!    close(u)
    
!    print *, ios
!!If the file is opened successfully, ios is 0, otherwise some other values.

!end program he

!!=========
!!Test the unit number to be less than 100, which is wrong after a few tests
!program he
!    implicit none
    
!    integer :: i=2, a(4), j
    
!    open(i, file='test.dat', status='unknown')
!    !here "unit" can be omitted, but "file" can not.
!    write(i,fmt='(2x, i2/, i2/, i2/, i2)') 1, 2, 3, 4
!    close(i)
    
!    open(i, file='test.dat', action='read', blank='null')
!    !!$omp parallel
!        !do j=1, 4
!        read(i,*) (a(j), j=1,4)
!        !end do
!    !!$omp end parallel
!    close(i)
    
!    print *, a
    
!end program he


!!===========================================
!program he
!    use omp_lib
    
!    implicit none
    
!    integer :: i, a=0
!    !$omp parallel do
!    do i = 1, 4
!        call re_va(a, i)
!    end do
!    !$omp end parallel do
    
!    print *,"Mian, a: ", a, omp_get_thread_num()

!end program he

!subroutine re_va(b, j)
!    implicit none
!    integer, intent (in) :: j
!    integer :: b
    
!    b=j
!end subroutine 


!!==========================================
!program he
!    !use omp_lib    !use omp_lib is useful when variables in omp_lib are used, like omp_get_thread_num()
!                    !Otherwise, it is not required to use it.
!    implicit none
    
!    integer :: i, a=0, b=0
!    common /dat/ a, b
    
!    !$omp threadprivate(/dat/)
!    !$omp parallel do
!    do i =  1, 4
!        call prin(i)
!    end do
!    !$omp end parallel do
!    print *, sizeof(2d+2)
!end program he

!subroutine prin(i)
!    implicit none 
!    integer, intent (in) :: i
!    integer :: a
    
!    common /dat/ a
    
!    !$omp threadprivate(/dat/)
    
!    print *, a
!    a=i
!    print *, a
    
!end subroutine prin


!!==========================================
!program he
!    implicit none
    
!    integer :: i, a(1,2), j
    
!    open(unit=2, file='test')
!    write(2,*) 1, 2
!    close(2)
    
!    open(2,file='test')
!    read(2,*) ( (a(i, j), i = 1, 1), j= 1, 2 )
!    close(2)
    
!    print *, a

!end program he



!!========================
!module va
!    real, private :: a, b
!    real :: c
    
!    contains
    
!    subroutine inti_valu
!        implicit none
!        a = 1.0
!        b = 2.0
!    end subroutine inti_valu
    
!    subroutine printc
!        implicit none
!        call inti_valu
        
!        !c = a+b
!        print *, "In the module, c = ", c
    
!    end subroutine printc
!end module va

!program he
!    use va
    
!    implicit none
!    !call printc
!    c = 2.3
!    print *, "In the main program, c = ", c
!    call printc
!end program he


!!===============================
!!Test the private variables in the module
!module pri_var
!    implicit none
    
!!    private 
!!    public printf
!!The above two statements make the all the stuff in the module as 
!!private except printf.
    
!    private a, b
    
    
!    integer :: a, b, c
    
!    contains 
    
!    subroutine printf
    
!        print *, c
!    end subroutine printf

!end module pri_var

!program he
!    use pri_var
    
!    implicit none
    
!    c = 1
    
!    call printf
!end program he

!===============================
!!When variables are decleared in the module like the following,
!!they behave like common variables whose values can be shared
!!when called.
!module vable
!    integer :: a, b
!end module vable

!program he
!    use vable
    
!    implicit none
    
!    a = 3
!    b = 32
    
!    call prin
    
!    print *, a, b
!end program he

!subroutine prin
!    use vable
    
!    a = 0
!    b = 2
!end subroutine prin 


!!==============================
!program he
!    implicit none
    
!    integer :: i = 1
!    common /num/ i
    
!    call print_num
    
!    print *, i
    
!end program he

!subroutine print_num
!    implicit none
    
!    integer :: j
!    common /num/ j
    
!    j=2
    
!end subroutine print_num



!!=====================================
!!Test the reading
!program he
!    implicit none
    
!    integer :: a
    
!    open(unit=2, file='a.txt')
!    read(2, '(t10, i2)') a
!    close(2)
    
!    print *, a
!end program he


!!=====================================
!program he
!    implicit none
    
!    integer :: a(3), b(4), i
!    namelist /dat/ a, b
!    data a/1, 2, 3/, b/1, 2, 3, 4/, i/ 2/
    
    
!!    open(5, file='dat.txt')
!!    write(5, '("&dat"/, "a=", i2, i2, i2/, "b=", i2, i2, i2, i3/, "/")') &
!!    &(/(i , i = 1, 3)/), [(i**2, i = 1 ,4)]
!!    close(5)
    
!!    open(5, file='dat.txt')
!!    read(5, dat)
!!    close(5)
    
!    write(*, *) a, b, i
!end program he


!!======================================
!program he
!    implicit none
    
!    integer :: i
!    integer :: a (3) = [(i ,i = 1, 3)], b (3) = (/4, 5, 6/),&
!                & c(3) = (/(i**2, i = 1, 3)/)
!    integer :: d(2, 2) = reshape( (/(i, i=11, 14)/),(/2, 2/))
!    namelist /dat/ a, b, c, d
    
!    write(*, dat)

!end program he



!!========================================
!!More practice on reading using namelist.
!program he
!    character :: nam*13, dob*10, gender*4
!    integer :: valu(8), time(3)
!    namelist /me/ nam, dob, gender, time
    
!    call date_and_time(values=valu)
!    time=valu(5:7)
!    open (unit=4, file='information')
!    write(4, '( "&me"/, "nam=''Lianshui Zhao''"/, "dob=""04/02/1990"""/&
!    &, "gender=""male"""/, "time=", i4, i4, i4/, "/" )') time
!    close(4)

!    open(4, file='information')
!    read(4, me)
!    close(4)
    
!    write(*,me)
    
!end program he


!!=======================================
!program he
!    implicit none
    
!    integer :: i =7
!    namelist /num/ i
    
!    write(*, num)
    
!    open(2, file='num.txt')
!    write(2, '("&num"/,"i= ",i1/, "/"/)') 8
!    close(2)
    
!    open(5, file='num.txt')
!    read(5, num)
!    close(5)
    
!    write(*, num)
!!Okay, so the format of reading from a file using namelist is
!!&num
!!i=8 for numbers, and a="hello" for strings and characters.
!!/

!end program he



!!=====================================
!program he
!    implicit none
    
!    character :: nam*13='Seth Zhao', DOB*10 = '1990/04/02',&
!                GENDER*4 = 'MALE'
!    integer :: valu(8), time(3)
!    namelist /me/ nam, dob, gender, time
    
!    call date_and_time(values=valu)
!    time = valu(5:7)
    
!    write(*,nml=me) !or write(*,me)
    
!    open(unit=2, file='information')
!    write(2, fmt='("nam=""Lianshui_Zhao"""/, "dob=""04/02/1990"""/, "gender=""Male"""/)')
!    close(2)
    
!end program he


!!====================================
!program he
!    implicit none
    
!    integer :: i
!    character :: a(5)
    
!    open(unit=1, file='he.txt')
!    do i = 1, 5
!        write(1, '(i2)', advance='no'), i
!    end do
!    close(1)
    
!    open(1, file='he.txt')
!    do i = 1, 5
!        read(1, '(1x, a1)', advance='no'), a(i) !Or you can put (t2, a1, t4, a1, and the rest)
!    end do
!    close(1)
    
!    print '(5(1x, a1))', a
!    print '(t2, a1, t4, a1, t6, a1, t8, a1, t10, a1)', a
!!tn is to write at column n.
!    print '(t2, i4)', 232
!end program he


!!====================================
!module prin
!    contains
    
!    subroutine prin_arr(a, n)
!        implicit none
        
!        integer, intent(in) :: n
!        integer, intent(in) :: a(n, n)
!        integer :: i, j
        
!        do i = 1, n
!            do j = 1, n
!                write(*, '(i2)', advance='no') a(i, j)
!            end do
!            write(*, *)
!        end do
    
!    end subroutine prin_arr

!end module prin

!program foal
!    use prin
!    implicit none
    
!    integer :: i, j, a(3, 3)=1, b(3, 3), c(3, 3)
!    logical :: d(3,3)=.false.
!    b = a
!    call prin_arr(a, 3)
!    write(*,'(10(1H+))')
    
!    do i = 1, 3
!        do j =1, 3
!            c(i, j) = a(i, i)+a(j, j)
!        end do
!    end do
!    call prin_arr(c, 3)
!    write(*,'(10(1H+))')
    
!    do i = 1, 3
!        do j =1, 3
!            a(i, j) = a(i, i)+a(j, j)
!        end do
!    end do
!    call prin_arr(a, 3)
!    write(*,'(10(1H+))')
    
!    forall (i = 1:3, j=1:3, b(2,3)==2 .or. i/=2) b(i, j) = b(i, i)+&
!    b(j, j)
!    call prin_arr(b, 3)
!    write(*,'(10(1H+))')
!!The forall statement does job parallel so the changes do not affect 
!!the result, while nested do lood affects the result because of the 
!!previous execution. Do difference is forall is parallel, while do
!!loop follows a specific order.
!end program foal




!!==================================
!program he
!    implicit none
    
!    integer :: i
!    integer :: b(2, 3)= reshape( (/(i, i = 1, 6)/),(/2, 3/) )
!    double precision :: a(2, 3)
!    logical :: c(2, 3) = .true.
    
!    where (c) a=1d0/real(b)
    
!    print *, b
!    print *, a

!end program he



!!=====================================
!program he
!    implicit none
    
!    complex(kind=4) :: a = (1.0, 1.0), b=(1.0, -2.0)
    
!    print *, a, int(real(a)), imag(a)
    
!    print *, "Enter complex number b: "
!    read '(2f10.3)', b
    
!    print *, a, b
!!When read complex number from keyboard, you have to specify the 
!!the format to be 2f22.3, something like that, and seperate the 
!!real and imaginary part by comma.
    
!!    print *, abs(a), conjg(a)
!!    print *, a*b, b/a
    
!!    print *, cmplx(1, 2)
!end program he

!!======================================
!program he
!    implicit none
!    integer :: i
!    integer :: a(2, 3) = reshape( (/(i, i = 1, 6)/), (/2, 3/) ),&
!               b(2, 3)
!    logical :: c(2, 3)
    
!    where (a .gt. 3)
!        b = 6
!        where (a*b .gt. 24)
!            c = .true.
!        else where
!            c = .false.
!        end where
!    else where
!        b = 1
!        where(a*b .gt. 2)
!            c = .true.
!        else where
!            c = .false.
!        end where
!    end where
    
!    print *, a
!    print *, b
!    print *, c
    
!    print *, size(a)
    
!end program he


!!=======================================
!program he
!    implicit none
    
!    integer :: i
!    integer :: a(2, 3) = reshape( [(i**2, i = 1, 6)],(/2, 3/) )
!    logical :: b(2, 3)
    
!    print *, a
    
!    where(a .lt. 18)
!        b = .false.
!    else where
!        b = .true.
!    end where
    
!    print *, b

!    if (a(2,2) /= 19) b(2,3) = .false.
!    print *, b
!end program he


!!========================================
!program he
!    implicit none
!    integer :: i
!    integer :: a(3) = (/(i, i = 1, 3)/), b(3) = [(i, i=1, 3)],&
!               c(3) = (/2, 3, 4/)
!    integer :: d(2, 3)=reshape( (/1, 2, 3, 4, 5, 6/), (/2, 3/) )

!    print *, a
!    print *, b
!    print *, c
!    print *, d
!end program he


!!========================================
!program rando
!    implicit none
    
!    real :: a(3, 3)
!    integer :: k
!    integer, dimension(12) :: seed_put, seed_get
!!If seed_put is initialized, then you will not get different 
!!random numbers each time you call it.
!    call random_seed(size=k)
!    print *, k
    
!    !call date_and_time(values=valu)

!    !print *, seed
    
!    call random_seed(put=seed_put)
!    call random_seed(get=seed_get)
    
!    !print *, seed_get
    
!    call random_number(a)
!    print *, a
!!Okay one way of generating an array of random numbers is to 
!!declare an uninitialized array, which is put into random_seed
!!, whose size should be the minimum size of the input array.
!!Then you will get an array through 'get', whose size is larger
!!than the size of the random_seed. When calling random_seed you 
!!get different random numbers each time you call it.
!end program rando   


!!=======================================
!!This is to test the date_and_time statement
!program he
!    implicit none
    
!    integer:: valu(8)
    
!    call date_and_time(values=valu)
!!values is some intrinsic variable. The order is also werid.     
!    print '(i5, i5, i5)', valu(5:7)
    
!!The output of the date_and_time is to output the year month day
!!The difference between the time in the current location and that
!! of UTC in minutes. The hour, minute, second and millisecond.

!end program he

!!=====================================
!program he
!    implicit none
    
!    integer, allocatable, dimension(:) :: a
!!For allocatable array, dimension(:) can not be replaced by dimension(*)

!end program he

!!======================================
!program he
!    implicit none
    
!    integer :: a(1:2, 1:2), i, j
    
!    do i = 1, 2
!        do j = 1, 2
!            write(*,'(i2)', advance='no') a(i, j)
!        end do
!        write(*,*)
!    end do
!!This is another way of 
!end program he


!!=======================================
!program he
!    implicit none
    
!    integer, parameter :: re_kind = selected_real_kind(32, 902)
!    real(kind=re_kind) :: pi = 3.11111111111111111111d10
!    double precision :: a= 3.1111111111111111111991d10
!    print *, re_kind, pi, a
!!I think selected_real_kind is not really useful at all. Though I set 
!!the number of precision as 32, it does not correctly read the digits 
!!beyond 6. I think double precision is more useful and accurate.
!end program he


!!=======================================
!!This program is just to practice module
!module statc
!    contains    
    
!    real function fmean(a, n)
!        implicit none
        
!        integer, intent(In):: n
!        integer, dimension(n), intent(in) :: a
        
!        fmean = real(sum(a))/n
    
!    end function fmean
    
!    real function fvar(a,n)
!        implicit none
        
!        integer, intent(in) :: n
!        integer, dimension(n), intent(in) :: a
!        integer :: i
        
!        fvar = fmean( [(a(i)**2, i = 1, n)], n ) - fmean(a,n)**2
        
!    end function fvar
    
!    real function fco_var(a,n)
!        implicit none
        
!        integer, intent(in) :: n
!        integer, dimension(n), intent(in) :: a
        
!        fco_var = fvar(a, n)**0.5
!    end function fco_var

!end module statc

!program he
!    use statc
    
!    implicit none
    
!    integer :: i
!    integer, dimension (4) :: a = [(i, i =1,4)]
!    real:: mean, var, co_var
    
!    mean = fmean(a, 4)
!    var = fvar(a, 4)
!    co_var = fco_var(a, 4)
    
!    print *, mean, var, co_var

!end program he

!!=========================================
!!This program is to test the assignment of an array.
!program he
!    implicit none
    
!    integer, dimension(4) :: a= (/1, 2 , 3, 4/), b
!    integer :: i
    
!    b = [(i**3, i = 1, 4)]
!    !a = (/1, 2, 3, 4/)
    
!    print *, a, sum( (/1, 2, 3/) )
!    print*, b
    
!!Okay, so there are a few ways of assigning values to an array. One is 
!!to use (/a, b, .../), or [(i**2, i = 1, 4)]. Or even read from the 
!!keyboard, a file .... Or use do loop do i = 1, 4 a(i)=i end do.
!end program he

!!========================================
!!This program is test the string that can be printed from nth character
!!to mth character.
!program he
!    implicit none
    
!    character*19 :: b = ' hello sd '
!    character*9, dimension(3):: a='he sd'
    
!    print '(a3, 1x, a4)', a(1)(2:4), b(3:6)

!end program he


!!=============================
!!This program is to test the statements trim, adjustl, and concatenate.
!program he
!    implicit none
    
!    character*20 :: a = '  hell    ', b = '  eh ', c*40
    
    
!    print *, len(a), len(trim(a)), len(adjustl(a))
!    print '(a10, a10)', trim(a), adjustl(a)
    
!    c = a//b
!    print *, len(trim(a)//trim(b))
!    print *, len(trim(adjustl(trim(a)//trim(b))))
!    print '(a8)', trim(adjustl(trim(a)//trim(b)))
    
!    print *, len(trim(a)//trim(b))
    
!!    print '(a10)', 'he'
    
!!    print '(a6, i2)', adjustl(trim(a)), len( adjustl(trim(a)) )
    
!!trim cuts the length of the string, while adjustl maintain the length 
!!of the string. trim deletes the blancks at the end of the string, while
!!adjustl removes the spaces at the beginning of the string. // is to 
!!concatenate the strings, with the total length as the sum of the 
!!two strings.

!end program he


!!============================
!program he
!    implicit none
    
!    integer, dimension(2, 3) :: a = 1, b = 2
!    integer, dimension(2) :: c = 1, d = 2
!    integer, dimension(3, 2) :: e = 2
!    integer, dimension(2, 2) :: f
    
!    print *, "sum a: ", sum(a), "sum b: ", sum(b)
!    print *, "product a: ", product(a), "product b: ", product(b)
    
!    print *, "dot_product cd: ", dot_product(c, d)
!    f=matmul(a, e)
!    print '(a, i2, i2/, 11x, i2, i2)', "matmul ae: ", f(1, 1), f(1, 2)&
!    &, f(2, 1), f(2, 2)
    
!    print *, "sum of f: ", sum(f)
!end program he


!!===========================
!program he
!    implicit none
    
!    integer :: i, j
!    integer, dimension (4, 3) :: a = 1
    
!    write(*,'(10(1HM))')
!    do i = 1, 4
!        write(*,'("|")', advance='no')
!        do j = 1, 3
!            write(*,'(i2)', advance='no'), a(i,j)
!        end do
!        write(*,'("|")')
!    end do 
!    write(*, '(10(1H+))')
!    do i = 1, 4
!        write(*, '("|", i2, i2, i2, "|")'), a(i, :)
!    end do
!    write(*,'(10(1H=))')
!end program he

!!==========================
!program he
!    use sum_sub
!    implicit none
    
!    integer:: a = 21, b = 3, c, d

!    c = add(a, b)
!    d = sub(a, b)
    
!    print *, c, d
!end program he

!module sum_sub
!    contains
    
!    integer function add(a, b)
!        implicit none
!        integer, intent (in) :: a, b
!        add = a+b
!    end function add
    
!    integer function sub(a, b)
!        implicit none 
!        integer, intent (in) :: a, b
!        sub = a-b
!    end function sub    

!end module sum_sub

!!=============================================
!program he
!    implicit none
    
!    integer :: i
!    integer, dimension(10) :: a
    
!    open (unit=5, file='test.txt')
!    !$omp parallel do ordered
!    do i = 1, 5
!    !$omp ordered
!        write(unit=5, fmt='(i2)'), i
!    !$omp end ordered
!    end do
!    !$omp end parallel do
!    close(unit=5)
    

!    open (unit=5, file='test.txt', position='append')
!    !$omp parallel do 
!    do i = 1, 5
    
!        write(unit=5, fmt='(i2)'), i
    
!    end do
!    !$omp end parallel do
!    close(unit=5)
    
!    open (unit=2, file='test.txt')
!    do i = 1, 10
!        read(unit=2, fmt='(i2)'), a(i)
!    end do
!    close(unit=2)
    
!    print *, a

!end program


!!====================================
!!This is to test the reshape and pack statements
!program test1
!    implicit none
    
!    integer :: i, j
!    integer, dimension(2,3) :: a
!    integer, dimension(2,2) :: c
!    logical, dimension(2,3) :: b
!    b = .false.
!    a = reshape((/1,2,3,4,5,6/),(/2,3/))
!    b(:,1:2) = .true.

!    c=reshape(pack(a,b), (/2,2/))
    
!    do i = 1, 2
!        do j = 1, 2
!            write(*,'(i2)', advance='no') c(j, i)
!        end do
!        !print *, new_line(' ')
!        write(*,*) new_line('a')
!    end do
    

!end program test1


!!====================================================
!!This is to test the arrary delcaration.
!program arr 

!    implicit none
    
!    character*3 :: a*1
    
!    integer, dimension(2, 2) :: b
!    data  b/4*2/
    
!    a = 'helsedfr'
    
!    print *, a, b
!    print '(23(1H+)/23(1H*)//2(1HM))'

!end program arr
 

!!==================================
!!This program is to test the function which returns an array.

!program fun_array
    
!    use ou    !Attention, use has to be in front of all other lines.
!              !Use the name of the module, not the name of the file which contains the module.
    
!    implicit none
    
!    integer, dimension(3) :: arr 
    
!    call retu_arr(arr)
    
!    print '(3(t2, i1), 3i3, i4)', arr, arr, int_mod(30)
    
!end program fun_array
 



!!=========================
!!Test integer format
!program int_for
!    implicit none
    
!    integer :: a=23232
!    !real(kind=8) :: b = -23.2348888d+0
!    double precision :: b = -23.2348888d+2
!    logical :: t=.true., f=.false.
!    character*23 :: str='hello w!'
!    write(*, 200), a
!    write(*, 300) a
!    write(*, 400) a, a
!    200 format (i6)
!    300 format(i5)
!    400 format(2i9.8)       !At least 8 digits, if more than the digits of the original value, 0 in front.
    
!    print '(2f12.6, 3x, 1p, e19.5)', b, b, b
!    write(*,"(es19.9)"), b
!    write(*,'(es19.9)'), b
    
!    print '(2l4)', t, f
!    write(*, "(2l5)"), t, f
    
!    print '(a)', str
!    write(*,'(a20)'), str
!    write(*, 500), str
!    500 format(a2)
    
!    print '(t2, i1, t3, i2)', 2, 7        !t3: go to column 3.
    
!    print '("hello, "3i4.3)', 1, 2, 323
!    write(*,'("hello, ", 2(i2, i2))'), 1, 2, 3 ,4
!    print '( 3(i2, i2) )', 1, 2, 3, 4, 5, 6
!    print "('hello, ', a)", 's', 'e', 't', 'h'
    
!    open(unit=2, file='int_for.txt')
!    write(2, '("hello, ", i1)'), 1, 2, 3, 4
!    write(2, '("new line"/, "next new line"/, "last line")')
!    close(unit=2)
    
!    write(*, '("hello"/, t2, i3//, t3, i2)'), 20, 30
!    print '(i1/, t2, i1/, t3, i1)', 1, 2, 3
    
!end program int_for



!!===========================
!!This is to test printer
!program printer
!    implicit none
    
!    write(*,100)
!    100 format('1   ', "This is at the top of a new page.")
!    write(*,200)
!    200 format('   ', "1      3      ")
!    write(*, 300)
!    300 format('   0', "2     4")
!end program printer


!!==========================
!program test
!    implicit none
!    character*12:: str='hehe'
    
!    print '(a2)', str
    
    
!    open(unit=2, file='(hell_txt)')  !
!    write(2, '(a)'), "helloworld"     !write(unit=2, fmt=str), "hellowrold"
!    close(2)
!    print *, str
!    open(unit=2, file='(hell_txt)')
!    read(2, '(a)'), str
!    close(2)
    
!    print '(a)', str

!end program test    


!!========================================================
!!This program is to test the comma between write/read and the contents
!program comma_w_r
!    implicit none
!    integer :: str=3
    
!    write(*,*), "hello (with comma)"
!    write(*,*) "world (without comma)"
    
!    print 100
!    100 format ("Test the format number 100, hope it works.")
    
!    print '("he", "''", ", """, a)'
!    write(*, '("as")')
!    write(*, 100)

    
!    write(*, '(i3)'), str
    
!end program comma_w_r
