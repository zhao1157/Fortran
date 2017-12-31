program test_write
implicit none
	
2 format (2I4)
	
write(*, 2) 2, 3, 4, 5



end program test_write







!!=========================================================
!!This program is to test intent(out)
!program intent_out
!    implicit none
    
!    integer:: a=2, b
    
!    call add_a_b(a,b)
    
!    print *, a+2, b
!end program intent_out

!subroutine add_a_b(a,b)
!    implicit none
    
!    integer, intent(inout):: a
!    integer, intent(out):: b
    
!    b = a+1
!    a = b-a

!end subroutine add_a_b

!!========================================================
!!This program is to test 3-D matrix
!program matrix_3d
!    implicit none
!    integer:: c, i, j, k
!    integer, allocatable, dimension( :, :, :):: mat
!    integer, allocatable, dimension(:):: mat_insert
    
!    print *, "Please enter the number of columns: "
!    read *, c
!    allocate(mat(c, c, c+1), mat_insert(c))
    
!    print *, "Please enter the fundamental matrix (integer): "
    
!    print *, "Please enter mat(:,:,1) here: "
!    read *, mat( :, :, 1)
            
!    do i = 1, c
!        print '("Please enter the ", i1, "th element of the inserted& 
!        &column")', i
!        read *, mat_insert(i)
!    end do
    
!    do k = 1, c+1
!        print '("The (:,:,", i1, ") element is ")', k
!        do i = 1, c
!            do j = 1, c
!                write(*, '(i2)', advance= 'no'), mat(i,j,k)
!            end do
!            write(*,*)
!        end do
!        print*, new_line('a')
!    end do
    
!    print *, "The inserted column is: "
!    do i = 1, c 
!        write(*, '(i2)', advance='yes'), mat_insert(i)
!        !print *, new_line('a')
!        !print *, mat_insert(i,1)
!    end do
!    !write(*,*)
    
!    call mat_new_fun(mat, c, mat_insert)
    
!    do k = 1, c+1
!        print '("The NEW (:,:,", i1, ") element is ")', k
!        do i = 1, c
!            do j = 1, c
!                write(*, '(i2)', advance= 'no'), mat(i,j,k)
!            end do
!            write(*,*)
!        end do
!        print*, new_line('a')
!    end do

!end program matrix_3d

!subroutine mat_new_fun(mat, c, mat_insert)
!    implicit none
    
!    integer, dimension(c,c,c+1), intent(inout):: mat
!    integer, dimension(c), intent(in):: mat_insert
!    integer, intent(in):: c
!    integer:: i
    
!    do i =1, c
!        mat(:,i,i+1) = mat_insert
!    end do 


!end subroutine mat_new_fun


!!===============================
!program matrix_input
!    implicit none
    
!    integer:: i, j
!    integer, dimension(2,2):: mat
    
!    do i = 1, 2
!        print *, "Please enter an element "
!        read *, mat(i, :)  !Assign value by row
!        !read *, mat(:, i) !! Assign value by column
!    end do
    
!    do i = 1, 2
!        do j = 1, 1
!            write (*, '(i2)', advance= 'no'), mat(i,j)
!        end do
!        write (*,*)
!    end do

!end program matrix_input


!!=======================================================
!!This program is to use various techniques to implement terminant.
!program terminant
!    implicit none
    
!    integer:: c, i, j
!    real:: det
!    real, allocatable, dimension(:, :):: mat
!    real, external:: detfun
    
!    print *, "Please enter the number of columns of the square &
!    & matrix: "
!    !read '(i2, i2)', r, c
!    read *, c
    
!    allocate(mat(c, c))
    
!    do i= 1, c
!        do j = 1, c
!            print '("Please enter real mat( ", i1, ", ", i1, "): ")',&
!            &i, j
!            read *, mat(i, j)
!        end do
!    end do
    
!    do i= 1, c
!        do j = 1, c
!            write(*, '(f5.1)', advance='no'), mat( i, j)
!        end do
!        write (*, *)
!    end do
    
!    det = detfun(mat, c)
    
!    print '("The determinant of the above matrix is: ", f7.2)', det

!end program terminant

!recursive real function detfun(mat, c) result(det)
!    implicit none
    
!    integer, intent(in):: c
!    integer :: i
!    real, dimension(c, c), intent(in):: mat
!    real, dimension(c-1, c-1):: new_mat     !Use another function?
    
!    do i = 1, c
!        if (c == 1) then
!            det = 1
!            return
!        else
!            call new_mat_fun(mat, c, i, new_mat)
           
!            det = det + (-1)**(i+1)* mat(1, i)*detfun(new_mat, c-1)
!        end if
!    end do
!    return
!end function detfun

!subroutine new_mat_fun(mat, c, i, new_mat)
!    implicit none
!    integer, intent(in):: c, i
!    real, dimension(c, c), intent(in):: mat
!    real, dimension(c-1, c-1), intent(out):: new_mat
!    logical, dimension(c, c):: tf_mat
    
!    tf_mat = .true.
!    tf_mat(1, :) = .false.
!    tf_mat(:, i) = .false.
    
!    new_mat = reshape(pack(mat, tf_mat), (/c-1, c-1/))
!end subroutine new_mat_fun


!!======================================================
!!This program is to test the allocation of 2D array
!program allo_2d
!    implicit none
    
!    integer:: i, j, r, c
!    integer, allocatable, dimension(:, :):: new_mat
!    integer, dimension(3, 3):: mat
    
!    print *, "Please enter the numbers (less than 3))of row and column of the&
!    & matrix"
!    read *, i, j
    
!    allocate (new_mat(i,j))
    
!    mat = reshape((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
!    print *, "The orginal matrix is: "
!    do r = 1, 3
!        do c = 1, 3
!            write (*, '(i1)', advance='no'), mat(r, c)
!        end do
!        write (*, *)
!    end do 
!    print *, "The new matrix is: "
!    do r = 1, i
!        do c = 1, j
!            new_mat(r, c)= mat(r, c)
!            write(*, '(i2)', advance='no'), new_mat(r, c)
!        end do
!        write(*, *)
!    end do
    
!    print *, new_mat
!end program allo_2d


!!=======================================================
!!This program is to practice the write functino
!program write_test
!    implicit none
    
!    integer:: i, j
!    real, dimension(3, 4):: mat
!    real, dimension(2, 3):: new_mat
!    logical, dimension(3, 4):: tf_mat
    
!    mat = reshape((/1.0, 2.2, 2.3, 4.2, 5.1, 6.1, 7.1, 8.2, 9.2, 10.1,&
!    &11.1, 12.1/), (/3, 4/))
!    tf_mat = .true.
!    tf_mat(3, :) = .false.
!    tf_mat(:, 4) = .false.
    
!    new_mat = reshape(pack(mat, tf_mat), (/2, 3/))
    
!    print *, "The old matrix is the following: "
!    do i= 1, 3
!        do j = 1, 4
!            write (*, '(f5.1, 2x)', advance='no'), mat(i, j)
!        end do
!        write(*, *)
!        !print *, new_line('a')
!    end do
!    print *, new_line(' ')
!    print '("The new matrix is the following: ")'
!    do i = 1, 2
!        write(*, '(a, i1, 2x)', advance='no'), "Row ", i
!        do j = 1, 3
!            write (*, '(f3.1, 2x)', advance = 'no'), new_mat(i, j)
!        end do
!        !write(*, *)
!        !print *, new_line('a')
!        !print *
!    end do
   

!end program write_test

!!======================================================
!!This program is to test the pack function
!program pack_test
!    implicit none
    
!    integer:: i
!    integer, dimension(2, 4):: mat
!    integer, dimension(2, 1):: new_mat
!    logical, dimension(2, 4):: tf_mat
    
!    mat = reshape((/1, 2, 3, 4, 5, 6, 7, 8/), (/2, 4/))
    
!    tf_mat = .false.
!    tf_mat (:, 1) = .true.
    
!    new_mat = reshape(pack(mat, tf_mat), (/2, 1/))
    
!    do i = 1, 2
!        print '(i1, 2X, i1, 2X, i1)', new_mat(i, 1)!, new_mat(i, 2), new_mat(i,& 
!        !&3)
!    end do
    
!end program pack_test

!!=====================================================
!!This program is to test the reshape function to transform 1D to 2D.
!program reshape_test
!    implicit none
!    integer :: i, j
!    integer, dimension(4) :: str
    
!    integer, dimension(2, 4) :: mat
    
!    mat = reshape((/1, 20, 36, 47, 5, 6, 7, 8/),(/2, 4/)) !The values are 
!                                                    !written in column.
    
!    do i = 1, 2
!        do j = 1, 4
!            print '("mat(", i1, "," i1, ") is ", i2)', i, j, mat(i, j)
!        end do 
!    end do
    
!    open(unit=1, file="reshape_mat")
!        do i = 1, 2
!            write(unit=1, fmt='(i2, i2, i2, i2)'), mat(i, 1), mat(i,& 
!            &2), mat(i, 3), mat(i, 4)
!            !read (unit=1, fmt='(A)'), str
!            !print *, str
!        end do
!    close(unit=1)
    
!    open(unit=1, file="reshape_mat")
!        do i = 1, 2
!            read (unit=1, fmt='(1X, i1, 1X, i1)'), str(1), str(2)
!            print '(i2, i2)', str(1), str(2)
            
!!            read (unit=1, fmt='(i2, i2, i2, i2)'), str(1), &
!!            & str(2), str(3), str(4)
!!            print '(i2, 2X, i2, i2, i2)', str(1)+1, str(2)+1, &
!!            & str(3)+1, str(4)+1
            
!!            read (unit=1, fmt='(A2, A2, A2, A2)'), str(1), &
!!            & str(2), str(3), str(4)
!!            print '(A2, 2X, A2, A2, A2)', str(1), str(2), &
!!            & str(3), str(4)
!        end do
!    close(unit=1)
    
!end program reshape_test

!!===============================::======================
!!This program is to test the subrountine
!program sub_test
!    implicit none
!    print *, "We are in the main program now and goint to A."
!    call A !The external procedures can not access to the inside ones.
    
!    !module xy
!    contains 
    
!    subroutine A 
!    implicit none
!    print *, "We are in the subroutine A now and goint to B."
!    call B
!    print *, "We are back to subroutine A."
!    end subroutine A
    
!    !end module xy
!    !print *, "We are back to the main program. Yea!" !!Limit the scope 
!                                                      !!of contains
!end program sub_test

!subroutine A 
!    implicit none
!    print *, "We are in the subroutine A now and goint to B."
!    call B
!    print *, "We are back to subroutine A."
!end subroutine A

!subroutine B
!    implicit none
!    print *, "We are in the subroutine B now and goint to C."
!    call C
!    print *, "We are back to subroutine B."
!end subroutine B

!subroutine C
!    implicit none
!    print *, "We are at the top and going back now"

!end subroutine C

!!====================================================
!!Just came up with an idea of how to fetch a character using function
!program str_fun
!    implicit none
!    integer :: n
!    character*30, external:: fetch_str
!    print *, "Please enter an integer: "
!    read *, n
!    print '("The number you entered is ", A)', fetch_str(n)
!end program str_fun

!character*30 function fetch_str(n)
!    implicit none
!    integer, intent(in) :: n
!    if (mod(n, 2) == 0) then
!        fetch_str = 'even'
!    else if (mod(n,2) /= 0) then
!        fetch_str = 'odd'
!    else
!        fetch_str = 'neither even or odd'
!    end if
!end function fetch_str

!!===================================================
!!This program is to test the recursive function inside and outside.
!program recur_function
!    implicit none
!    integer :: n
!    integer(kind=16), external:: mul
    
!    print '("Please enter a positive integer: ")'
!    read *, n
   
!    print '("The factorial and summation of n integers are ", i8,& 
!    &i8)', mul(n), sum_n(n)
    
!    contains
!    recursive integer function sum_n(n) result(s)
!        implicit none
!        integer, intent(in) :: n
        
!        if (n == 1) then
!            s = 1
!        else
!            s = n + sum_n (n-1)
!        end if
!    end function sum_n
!end program recur_function

!recursive integer(kind=16) function mul(n) result(m)
!    implicit none
!    integer, intent(in) :: n
    
!    if (n == 1) then
!        m = 1
!    else 
!        m = n * mul(n-1)
!    end if
!end function mul


!!================================================
!!This program is to test the function written inside the main program
!program area_rec_sq
!    implicit none
!    real :: a, b, area
!    character :: sh
    
!  do
!    print *, "Please enter r for rectanular or s for square: "
!    read '(A1)', sh
!    if (sh == 'r' .or. sh =='R') then
!        print *, "Please enter the two side length of the rectangular: "
!        read *, a, b
!        area = area_rec(a, b)
!        print *, "The area of the rectangular is ", area 
!        exit 
!    else if (sh == 's' .or. sh =='S') then
!        print *, "Please enter the side length of the square: "
!        read *, a
!        area = area_sq(a)
!        print *, "The area of the square is ", area
!        exit 
!    else 
!        print *, "It's not a rectangular or a square."
!        cycle
!    end if
!  end do
  
!    contains      !This is huge important, don't forget "contains"
    
!    real function area_rec(a, b)
!        implicit none
!        real, intent(in) :: a, b
!        area_rec = a*b
!    end function area_rec 
    
!    real function area_sq(a)
!        implicit none
!        real, intent(in):: a
        
!        area_sq = a**2
!    end function area_sq
       
!end program area_rec_sq

!!=======================================================================
!!This program is to study external funcitons
!program area_sq_tri
!    implicit none
!    real :: a, b, c, area 
!    real, external:: area_tri, area_sq
!    character :: sh

!    print *, "Please enter s for square and t for triangle: "
!    read '(A1)', sh

!    if (sh == 't' .or. sh == 'T') then
!        print *, "Please enter the three sides of the triangle: "
!        read *, a, b, c

!        area = area_tri(a, b, c)
!        print 5, a, b, c, area
!        5 format ("The triangel with sides of ", f4.2,2X, f5.2, 2X, &
!        & f4.2, &
!        &" has an area of ", f8.3)
!    else if (sh == 's' .or. sh =='S') then
!        print *, "Please enter the side length of the square: "
!        read *, a

!        area = area_sq(a)
!        print 10, a, area
!        10 format ("The square with side length of ", f4.2, " has an& 
!        &area of ", f4.2)
!    end if
!end program area_sq_tri

!real function area_tri(a, b, c)
!    real, intent(in) :: a, b, c
!    real :: s
!    s = 1.0/2*(a+b+c)
    
!    area_tri = (s*(s-a)*(s-b)*(s-c))**(1.0/2)

!end function area_tri

!!real function area_sq(a)       !!Functions can also be written in 
!!    implicit none              !!another script. gfotran -c x.f95; 
!!    real, intent(in) :: a      !!gfortran -c y.f95;
!                                !!gfortran x.o y.o -o xy.exe
!!    area_sq = a**2
    
!!end function area_sq

!!================================================================
!!The following program is to tell you the temerature and humidity of 
!!the day. Check this out!
!program temp_hum
!   implicit none
!   integer :: i, n
!   integer*8, allocatable, dimension(:) :: temp, rh
!   character*30 :: t, r, tr

!   print *, "Please enter the number of days: "
!   read *, n
!   allocate(temp(n), rh(n))

!   do i=1, n
!       print *, "Please enter the temperatue and realtive hunidity of& 
!        day n: " 
!        read *, temp(i), rh(i)
!   end do
    
!   open(unit=1, file="temp_rh")
!   do i = 1, n
!       select case (temp(i))
!           case (:20)
!               t = "cold"
!           case (21: 50)
!               t = "cool"
!           case (51: 80)
!               t = "comfortable"
!           case (81:)
!               t = "hot"
!           case default
!               t = "error"
!       end select
        
        
!       select case (rh(i))
!           case (:10)
!               r = "dry"
!           case (11: 30)
!               r = "sweaty"
!           case (31:)
!               r = "wet"
!           case default
!               r = "error"
!       end select 
!       !print '("Day ", i2, " is ", A12, A12)', i, t, r
        
!       write(unit=1, fmt='("Day ", i2, " is ", A, A)'), i, t, r
!   end do
!   close(unit=1)
    
!   open (unit=2, file="temp_rh")
!   do i = 1, n-1
!       read (unit = 2, fmt = '(A)'), tr
!       print *, tr
!   end do
!   close (unit =2)
!end program temp_hum


!!========================================
!!This program is to study the select case and 
!program select_case
!   implicit none
    
!   integer :: n
!   character*7 :: str
    
!   print *, "Please enter an integer: "
!   read *, n
    
!   !character*n :: str
    
!   select case (mod(n, 2))
!       case (0)
!           str = "even"
!           print *, "The number you just entered is ", str
!       case (1, -1)
!           str = "odd"
!           print *, "The number you just entered is ", str
!       case default
!           str = "unknown"
!           print *, "The number you entered is ", str
!   end select 
!end program select_case

!!========================================================
!!This program is to test the string array
!program string_array
!   implicit none
!   character*3, dimension(3):: a
!   print *, "Please enter 3 3-character strings below: "
!   read *, a(1), a(2), a(3)

!   print '(A3,2X,A3,2X,A3)', a(1), a(2), a(3)
!end program string_array

!!=========================================================
!!This program is to test the reading of more than on number
!program two_read
!   implicit none
    
!   integer, dimension(2):: a
!   print *, "Please enter two numbers here: "
!   read *, a(1), a(2)
    
!   print *, "The numbers you entered are: ", a(1), a(2)
!end program two_read

!!================================================================
!!This program is to test the the exit and cycle command in the codes.
!program exit_cycle
!   implicit none
!   integer :: n, i, temp, sum_pos
    
!   print '("Please enter the number of the integers you want to try: ")'
!   read '(i1)', n
    
!!  print *, "Please enter a positive integer here: "
!!  read *, temp
    
!   sum_pos = 0
!   !i = 1
    
!   do  i =1, n
    
!       print *, "Please enter an integer: "
!       read *, temp
!       if (temp<=0) then 
!           cycle          !cycle is from print to cycle
!       else
!           sum_pos = sum_pos +1
!       end if
!   end do
    
!   print '(i2, " out of ", i2, " integers you entered are & 
!   positive.")',sum_pos, n
    
!!  do 
!!      if (i > n) then
!!          exit         !is to break from the do loop
!!      else 
!!          i = i+1
            
!!          print *, "Enter an integer: "
!!          read *, temp
            
!!          if (temp > 0) then
!!              sum_pos = sum_pos+1
!!          end if
!!      end if
!!  end do
    
!!  print '(i2, " out of ", i2, " integers you entered are & 
!!  positive.")',sum_pos, n
    
!!  do 
!!      if (temp > 0) then
!!          print *, "The number is : ", temp, ", and the sum of the &
!!          digits is: ", sum_dig
!!          sum_dig = sum_dig + mod(temp, 10)
!!          temp = temp/10
!!      else 
!!          exit
!!      end if
!!  end do
!!  print '("The number is : ", i3, ", and the sum of the &
!!  digits is: ", i3)', temp, sum_dig

!end program exit_cycle

!!==============================================
!!This program is to read a file
!program readfile
!    implicit none
!    real, dimension(3) :: a, b
!    integer :: i
    
    
!    open(unit=1, file="real.txt")
!    do i=1,3
!        read(unit=1,fmt='(f5.2, f4.2)'),a(i),b(i)
!        print '(f7.3, 2X, f4.2)', a(i), b(i)
!    end do
!    close(unit=1)


!end program readfile


!!========================================================================
!program read_write
!    implicit none
    
!    real, allocatable, dimension(:) :: a
!    integer :: n, i
!    read '(i3)', n
    
!    allocate (a(n))
    
!    do i = 1, n
!        print '("Please enter a real number ")'
!        read '(f4.2)', a(i)
    
!    end do
    
!    open(unit=1, file="real.txt")
    
!    do i=1, n
!        write(unit=1, fmt='(f4.2, f4.2, f4.2)'), a(i)
!    end do

!    close (unit=1)
    
!    open(unit=2, file="real.txt")
!:
!end program read_write


!!=========================================================================
!!The program is to write some random things in a file

!program write_to_file
!    implicit none
!    character*5, dimension(3) :: a, b
!    read *, a, b
    
!    open(unit=1, file="textfile.txt")
!    write(unit=1, fmt='(A, A, A, A, A, A)'), b, a
!    close(unit=1)
    

!end program write_to_file


!!============================================================================
!!The following program is to practice triagnometric function
!program trig
!    implicit none
    
!    real, parameter :: pi=4*atan(1.0)
    
!    !integer, parameter :: n=50
!    integer :: n
    
!    real (kind=8), allocatable, dimension(:) :: x, sinx, cosx, readvalue
!    integer :: i
    
!    print '("Please enter an integer: ")'
!    read *, n
    
!    allocate(x(n),sinx(n),cosx(n),readvalue(n))
        
!    do i=0,n
!        x(i)=i * 2*pi / n
!    end do
    
!    sinx=sin(x)
!    cosx=cos(x)
    
!    !do i=0,n
!        !if (mod(i,10)==1) then
!            !print '(i3, 2X, f7.3, 2X, f6.4)', i, x(i)*180/pi, sinx(i)**2+cosx(i)**2
!!        else if (mod(i,10)==2) then
!!            print '(i4, "nd: ", "Sin of ", f5.1," degrees is ", f5.3)', i, x(i)*180/pi, sinx(i)
!!        else if (mod(i,10)==3) then
!!            print '(i4, "rd: ", "Sin of ", f5.1," degrees is ", f5.3)', i, x(i)*180/pi, sinx(i)
!!        else
!!            print '(i4, "th: ", "Sin of ", f5.1," degrees is ", f5.3)', i, x(i)*180/pi, sinx(i)
!!        end if 
!    !end do
!    5 format(f5.3, 3X, f6.4)
!    open(unit=1, file='tri.txt')
!    do i=0,n
!        write(unit=1, fmt=5), i*2*pi/n, sinx(i)**2
!    end do
!    close(unit=1)
    
!!    do i=0,n
!!    open(unit=2, file='tri.txt')
!!    read(unit=2, fmt='(f6.3)'), readvalue(i)
!!    print'("The ",i4,"th value of readvalue is ", f6.3)', i, readvalue(i)
!!    end do
!!    close(unit=2)
!end program trig


!!==================================================================
!!This program is to text reading text from a file
!program read_text
!    implicit none
!    character*3, dimension(3) :: ac, bc
!    integer :: i
    
!    open(unit=1, file='string.txt')
!    do i = 1, 3
!        print '("Please enter the ",i1, "th string: ")', i
!        read '(A3)', ac(i)
!        write(unit=1, fmt='(A3)'), ac(i)
!    end do
!    close(unit=1)
    
!    open (unit=2, file='string.txt')
!    do i = 1, 3
!        read (unit=2, fmt='(A2)'),bc(i)
!        print *, bc(i)
!    end do
!    close(unit=2)
    
!    print *,bc(1),bc(2),bc(3)
!end program read_text


!!===================================================================
!!This program is to practice the read and write commands.
!program read_write
!    implicit none
!    integer :: n, i
!    integer(kind=16), allocatable, dimension(:):: vals, ind, val_ind
!    print '("Please enter the maximum index: ")'
!    read *, n
!    allocate(vals(n), ind(n), val_ind(n))
    
!    vals(1)=0
!    vals(2)=1
    
!    do i = 3, n
!        vals(i) = vals(i-1)+vals(i-2)
!    end do
    
!    open(unit=1, file='fabonacci.txt')
    
!    do i = 1, n
!        write(unit=1,fmt='(i3, i30)'), i, vals(i) !How to read string?
!!        write(unit=1, fmt='("The ", i3,"th value of fabonacci is &
!!        &",i30)'), i, vals(i) !& usage is very important.
!    end do
    
!    close(unit=1)
    
!    open(unit=1, file='fabonacci.txt')
!    do i = 1, n
!        read(unit=1, fmt='(i3, i30)'), ind(i), val_ind(i)
!        !print '("The index and its fabonacci value are: ", i3, i30)', ind(i), val_ind(i)
!        print *, "The values are ", ind(i), val_ind(i)
!    end do
    
!    close(unit=1)
!!    integer :: a
!!    print '("Pleanse enter an integer: ")'
!!    read *,a
!!    open(unit=1, file='word1.txt')  !!Can I use "" instead of ''?
!!    write(unit=1,fmt = '("The number you entered is ", i3)'), a
!!    close(unit=1)
!    deallocate(vals, ind, val_ind)

!end program read_write

!====================================================================
!!This program is to test the format
!program test
!    implicit none
!    real :: a
    
!    print '("ENTER: ")'
!    read *,a
!    print '("The integer you entered is ", e9.2)',a

!end program test

!!===================================================================
!! This program will be used to calculate the Fabnacii values

!program fabnacci
!    implicit none
!    integer:: i, n
!    integer(kind = 16), allocatable, dimension(:)::vals
    
!    read *,n
!    allocate(vals(n))
    
!    vals(1)=1
!    vals(2)=1
    
!    do i = 3,n
!        vals(i) = vals(i-1)+vals(i-2)
!    end do
    
!    do i = 1, n
!        print '("fabanacci value of",i3 ," is ", i25)',i ,vals(i)
!    end do

!end program fabnacci

!!=====================================================================
!!This progrma is to test the the reading and writing of exponential values
!program exp_test
!    implicit none   
!    integer :: a
    
!    print *,"Please enter two integer here: "
!    read *, a
!    5 format (i0.9, 3X, e9.3) !!i0.9 means that all the vacant blocks 
!                              !!will be filled with zero's, i.e. 23 will 
!                              !!be 000000023.
!    print 5, a

!end program exp_test

!!=======================================================================
!program formatting
!    implicit none
    
!    integer :: a
!    print *,"Please enter an integer here: "
!    read '(e12.3)',a
!    print *,a
!!    character*2 :: ac
    
!!    print *, "Please enter a 3-character string: "
!!    read '(A5)', ac
    
!!    print '(A4)',ac
!!    integer :: a
!!    real :: ar, br
    
!!    print *,"Please enter an integer and a real number here : "
!!    read '(i3,f5.2,e10.3)', a, ar,br
!! !   19 format(i3, 3X, f5.2,3X, e10.3)
    
!!    print '(i3,f5.2,e10.3)', a, ar,br
!end program formatting


!!===========================================================================
!! This program is developed to find the day of a date between year 1901 and 2000 using a secret calculation

!program day_data

!    implicit none
    
!    integer :: year, month, date, y1, y14, m1, d
!    character (12), dimension(12) :: months, days
!    character*12 :: day
    
!    months(1) = "January"
!    months(2) = "February"
!    months(3) = "March"
!    months(4) = "April"
!    months(5) = "May"
!    months(6) = "June"
!    months(7) = "July"
!    months(8) = "August"
!    months(9) = "September"
!    months(10) = "October"
!    months(11) = "November"
!    months(12) = "December"
    
!    days(2) = "Monday"
!    days(3) = "Tuesday"
!    days(4) = "Wednesday"
!    days(5) = "Thursday"
!    days(6) = "Friday"
!    days(7) = "Saturday"
!    days(1) = "Sunday"
    
!    print *, "Please enter year, month and date : "
!    read *, year, month, date
!    y1 = mod(year, 1900)
!    y14 = y1/4
    
!! Leap year:     034 025 036 146 for m1 values
!! Non-leap year: 144 025 036 146 for m1 values   
!!    print *, months(7), days(7)
    
!    select case (month)
!        case (4, 7)
!            m1 = 0
!        case (10)
!            m1 = 1
!        case (5)
!            m1 = 2
!        case (8)
!            m1 = 3
!        case (3, 11)
!            m1 = 4
!        case (6)
!            m1 = 5
!        case (9, 12)
!            m1 = 6
!        case (1)
!            if (mod(year,4)==0) then
!                m1 = 0
!            else
!                m1 = 1
!            end if
!        case (2)
!            if (mod(year, 4)==0) then
!                m1 = 3
!            else
!                m1 = 4
!            end if
!        !   case (1)
!        !       select case (mod(year, 4)==0)
!        !           case (.true.)
!        !               m1 = 0
!        !           case (.false.)
!        !               m1 = 1
!        !       end select
!        !   case (2)
!        !       select case (mod(year, r)==0)
!        !           case (.true.)
!        !               m1 = 3
!        !           case (.false.)
!        !               m1 = 4
!        !       end select
        
!    end select
    
!    d = mod(y1+y14+m1+date, 4)
    
!    select case (d)
!        case (0)
!            day = days(7)
!        case (1)
!            day = days(1)
!        case (2)
!            day = days(2)
!        case (3)
!            day = days(3)
!        case (4)
!            day = days(4)
!        case (5)
!            day = days(5)
!        case (6)
!            day = days(6)
!        case default
!            print *, "error!"
!    end select
    
    
!    print *, "Year: ", year, " month: ", months(month), " day: ", day
!end program day_data
 

!!========================================================================

!!This program is used to explore the select case structure.

!program select_case
!    implicit none
!!    integer :: i
!!    character(5), dimension(4) :: str
    
!!    print *, "Please enter the index of the element you are interested in i: "
!!    read *,i
    
!!    str(1) = "he"
!!    str(2) = "ll"
!!    str(3) = "o"
    
!!    print *, "The element of ", i, " is ",str(i)
!    integer :: ind
!    integer, dimension(3) :: b
!!    a(1) = 1
!!    a(2) = 2
!!    a(3) = 3
!    print *, "Please enter the index : "
!    read *, ind
!    select case (ind)
!        case (1)
!            b(1) = 4
!        case (2)
!            b(2) = 5
!        case (3)
!            b(3) = 6
!    end select
    
!    print *,b(ind)
!end program select_case

!========================================================================
!program calculation
!    implicit none
    
!    integer :: a
!    print *, "Please enter an odd integer a : "
!    read *,a
!    print *, a/2

!end program calculation

!========================================================================

!! This program is to practice labelling loops.
!program labelling_loop

!    implicit none
    
!    integer :: i
    
!    label : do i = 1, 5
    
!        print *, i
    
!    end do label
!end program labelling_loop

!!05.17.2016 11:04:21

!======================================================================

!!This program will be something deep into the do loop, i.e. nested looping.

!program nestedloop

!!    4implicit none
!    integer :: i, n, factorial
    
!    print *, "Please enter the maximum number n: "
!    read *, n
    
!    factorial = 1
    
!    do i = 1, n
    
!        if (i == 1) then
!            factorial = factorial * i
!            print *, i, "! = ", factorial
!        else if (i == 2) then
!            factorial = factorial * i
!            print *, i, "! = ", factorial
!        else if (i == 3) then
!            factorial = factorial * i
!            print *, i, "! = ", factorial
!        else if (i == 4) then
!            factorial = factorial * i
!            print *, i, "! = ", factorial
!        else if (i == 5) then
!            factorial = factorial * i
!            print *, i, "! = ", factorial
!        end if
    
!    end do       
    
    
!    do i = 1, n, 1
        
!        factorial = 1
        
!        do j = i, 1, -1
        
!            factorial = factorial * j
        
!        end do
        
!        print *, i,"! = ", factorial
    
!    end do
    
!    print *, "The last i value is ", i


!end program nestedloop


!================================================================

!! Let's play modulus

!program modulus

!    implicit none
    
!    integer :: a, b
!    real :: c, d
    
!    a = 40
!    b = 30
    
!    c = 43
!    d = 30
!    print *, a, b, a/b, mod(a,b)
!    print *, c, d, c/d, mod(c,d)

!end program modulus


!===================================================================

!!Let's play single and double quotes

!program quot

!    implicit none
    
!    print *, "Hello world, ''what the heck?'' I was born again!"

!end program quot


!==========================================================

!! Let's play characters and strings.

!program str

!    implicit none
    
!    character(6), dimension(4) :: str_
    
!    str_(1) = '12ddddddddddx'
!    str_(2) = ' '
!    str_(3) = "sdsd"
    
!    print *, str_(1), '', str_(3), 'hello'
    
    
!!    character*8 :: a
!!    a = '  2323ssdsdfsdf'
!!    print *, a
    
!!    character :: a, b
!!    a = '2'
!!    b = '#'
    
!!    print *, a,' ', b
    

!end program str


!================================================================

!program x
    
!    implicit none
!    character :: q

!    q = '3'
!    print *, q

!end program x

!================================================================

!! The following program is do practice the do command or loops in other languages

!program do_loop

!    implicit none
!    integer (kind = 8) :: i, f, j

!    print *, "Please enter the initial and final values: "
!    read *, i, f
    
    
!    do j = i, f
    
!        print *, "2^", 'j', "is : ", 2**j
        
!    end do

!end program do_loop



!========================================================

!The following codes deal with the logical and relational stuff

!program logicalandrelational

!    implicit none
    
!    integer a, b
    
!    a = 4
!    b = 5
!    logical :: m, n
    
!    m = .true.
!    n = .false.
    
!    print *, "a > b ?", a .gt. b
!    print *, "a = b ?", a .eq. b
!    print *, "a < b ?", a .lt. b
!    print *, "a >= b ?", a .ge. b
!    print *, "a <= b ?", a .le. b
!    print *, "a /= b ?", a /= b

!    print *, "a > b or a < b    ", a < b .or. 1 > 2      ! .and.
!    print *, ".not. a > b : ", .not. a > b
!    print *, ".true. .and. .false. ", .true. .and. .false.
!    print *, ".not. .true. ", .not. .true.
!    print *, "1 > 2 .eqv. 2 > 3 ", 1 > 2 .eqv. 2 > 3
!    print *, "1 > 2 .neqv. 2 > 3 ", 1 > 2 .neqv. 2 > 3
!    print *, "m .and. n ", m .and. n
!end program logicalandrelational

!========================================================

! I will play type truncating
!program type

!real :: a = 2.3
!integer :: b 

!b = a
!print *, a, b


!end program type

!================================================================

!In the following program, I will calculate the length of the hypotenuse of a triagle and ite angle.

!program triangle

!    implicit none

!    real :: x, y, l, a
!    integer :: q
!    real, parameter :: pi = 2 * (asin(1.0))
!    character :: q


!    read *, x, y

!    l = sqrt(x**2+y**2)
!    a = atan(y/x)*180/pi

!    print *, "The length of the hypotenuse is ", l
!    print *, "The angle is ", a
    
!    if (x>0 .and. y>0) then
!        q = '1'                           !print *, "You are in quadrant 1."
!    else if (x>0 .and. y<0) then
!        q = '4'                            !print *, "You are in quadrant 4."
!    else if (x<0 .and. y<0) then
!        q = '3'                           !print *, "You are in quadrant 3."
!    else if (x<0 .and. y>0) then
!        q = '2'                          !print *, "You are in duadrant 2."
!    end if
    
!    print *, "The triangle is in quadrant ", q
    
!    print *, "The character with ascii 74 is ", char(74)
!end program triangle


!!=========================================================


!The line is to test the auto line breaking feature in this program. 
!Look, what just happened, it broke the line to the next one, but it 
!does make it as comment.

!program area_triangle

!   implicit none
    
!   real :: a, b, c, p, s, area 
    
!   print *, "Please print the three sides of a triangle"
!   read *, a, b, c
    
!   p = a + b + c
!   s = p/2
    
!   area = (s*(s-a)*(s-b)*(s-c))**0.5
    
!   print *, "A = ",a,"B = ",b,"C = ",c
!   print *, "The area of the above triangle is ", area
!end program area_triangle
