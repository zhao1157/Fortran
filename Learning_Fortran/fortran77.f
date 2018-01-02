c===============================
      program he
        implicit none
        
  10    print *, "hi" 
        
        if (2)10,10,10
      
      
      
      end program he






c================================
cIn this program, it shows that the variable passed between 
csubroutines can be modified without specifying IN/OUT/INOUT.
c      program he
c        a=2
c        do i = 1, 3
c            call a1(a)
c            print *, "a1: ", a
c            call a2(a)
c            print *, "a2: ", a
c        end do
c      end program he
      
c      subroutine a1(a)
c        a=a*2
c      end subroutine a1

c      subroutine a2(a)
c        a=a+16
c      end subroutine a2


c!==================================
c      program he
c        use omp_lib
        
c        implicit none
        
c        integer :: a, b, c, d
c        common /dat1/a
c        common /dat2/b
c        common /dat3/c
c        common /dat4/d
cc$omp threadprivate(/dat1/,
cc$omp1/dat2/,/dat3/,
cc$omp2  /dat4/)
      
      
c      end program he



cc=======================
cc This is to test if statement
c      program he
c      use omp_lib
c      implicit none
   
c      if (2 .gt. 1) then
c      write(*,*) "ni" 
c      go to 100
c      end if
c      write(*,*) "no"
cc$100  write(*,*) "he"  
 
c      if (2 .gt. 1) write(*,*) "last"
      
      
c      end program he

cc===========================================
c      program he
c      use omp_lib
      
c      implicit none
      
c      integer :: i, a=0
cc$omp parallel do  default(private)    
c      do 100 i =1, 5
c        a=i
c        write(*,*) "i= ", i, "a= ", a, omp_get_thread_num()
c 100  continue    
cc$omp end parallel do      
c      end program he

cc==============================================
c       program he
c      use omp_lib

c      implicit none
      
c      integer :: i
cc$omp parallel do      
c      do i = 1, 9
cc$        write(*,*) "Before: ", omp_get_thread_num()
c      stop
cc$    write(*,*) "After: ", omp_get_thread_num()
      
c      end do
cc$omp end parallel do      
      
      
      
c      end program he





cc==================================================================
cc      program he
cc$      use omp_lib
cc        implicit none
        
cc        write(*,*) "Before the stop statement."
        
cc$        stop
        
cc        write(*,*) "After the stop statement."
cc      end program he





cc==================================================================
cc This program is to test the 6th column
c      program column_6th
      
c      implicit none
      
c      integer :: a = 1,
c     1b= 2,
c     2c=3,
c     *d=4,
c     xe=5
      
c      print *, a, b, c, d, e
c      end program column_6th

cc=================================================================
ccThis is to test go to statement.
c      program go_to
c        implicit none
        
c        print *, "Before go to: "
        
c        if (2 .gt. 1) then
c            print *, "a>b: "
c            go to 100
c        else
c            print *, "a<b"
c            go to 200
        
c        end if
        
c 100    print *, "label 100."
c      stop
c 200    print *, "label 200."
c      end program go_to


cc=================================================================
cc This program is to test parallel program when loops are nested
c      program nes_lo
cc$    use omp_lib
c      implicit none
      
c      integer :: i, j
c      integer, dimension(3,3) :: a
cc$omp parallel do  private(i, j) 
c      do 10 i = 1, 3
ccc$omp parallel do  private(i, j)
c        do 20 j = 1, 3

c          a(i, j)=i
cc$        print *, omp_get_thread_num(), i, j         
c 20     continue
ccc$omp end parallel do 
c 10   continue
cc$omp end parallel do
c      print *, a
c      end program nes_lo


cc===============================================================
cc This program is to test array in fortran77.
c      program arr
c      implicit none
      
c      integer, dimension(3) :: a
c      integer :: i
      
c      do 10 i = 1, 3
c 10      a(i)=i
      
cc10    continue   
c!10    continue    
      
c      write(*,*), a
      
      
      
c      end program arr



cc==============================================================
CC * can be used to comment contents behind it.
*      program hello
cc$    use omp_lib
            
c      implicit none
            
c      integer :: i,max_num_threads=1, thread_num = 0, count1=0, count2=0
c      integer :: count_rate, count_max
      
cc$    max_num_threads = omp_get_max_threads() 
           
c      write(*,'(a, i2)')"The max_num_threads is ", max_num_threads
      
c      call system_clock(count1, count_rate, count_max)
      
cc$omp parallel do ordered  default(private) schedule(static,1)     
c      do 100 i = 1, 90000 
cc$omp ordered      
cc$      thread_num = omp_get_thread_num()        
c        write(*,'(a, i2, i7)') "This is thread ", thread_num, i
c!$omp end ordered        
c100   continue
cc      end do
cc$omp end parallel do
c      call system_clock(count2, count_rate, count_max)
      
c      print '(a, 1pe12.6)', "count2 - count1 is "
c      print '(1p,e17.10)', real(count2-count1)/count_rate
c      end program hello
