!!==========================================
!program he
!    implicit none 
    
!    integer :: i, a(4)
    
!    open(unit=2, file='hell')
!    write(2, fmt='(i2/, i2/, i2/, i2/)') 2, 3, 4, 5
!    close(2)
    
!    open(3, file='hell')
    
!    read(3, fmt='(i2)') a(3)
!    close(3)
!    open(4, file='hell')
!    read(4, fmt='(i2)') a(4)
!    close(4

    
!!The file can not be read simultaneously by more than one thread??
    
!    print *, a
!end program he



!==========================================
program he
    use omp_lib
    
    implicit none
    integer :: a, i, j = 2
    common /dat/a
    !$omp threadprivate(/dat/)
    
    open(unit=2, file='read_thread')
    write(2,fmt='(i2, i2, i2, i2)') 2, 3, 4, 5
    close(2)
    
    open(unit=2, file='read_thread')
    
    do i = 1, 3
    !$omp parallel num_threads(4)
    !do i = 1, 1
        !$omp single
        read(2,fmt='(i2)', advance='no') a
        !$omp end single copyprivate(a)
        print *, a, omp_get_thread_num()
        
    !end do
    !$omp end parallel
    end do
    close(2)
    
    !$omp parallel
        print *, "j = ", j
    !$Omp end parallel
end program he


!!=============================================
!program he
!    use omp_lib
    
!    implicit none
    
!    integer :: a, b, i
!    common /dat/a, b
!    !$omp threadprivate(/dat/)
!    a = 0
!    b = 0
!    !$omp parallel do
!    do i = 1, 4
!        call a_b(i)
!        print *, "a, b: ", a, b, omp_get_thread_num()
!    end do
!    !$Omp end parallel do
!end program he

!subroutine a_b(i)
!    !use omp_lib      !It's okay to omit this line, but can not be
!                      !omitted in the main program.
!    implicit none
!    integer, intent(in) :: i
!    integer :: m, n
!    common /dat/ m, n
    
!    !$omp threadprivate(/dat/)
    
!    m = m + i 
!    n = n + i

!end subroutine a_b


!!======================================================
!program he
!    use omp_lib
    
!    implicit none
    
!    integer :: a, b, i
!    common /dat/ a, b
    
!    !$omp threadprivate (/dat/)
!    a = 0
!    b = 0
!    !$omp parallel do
!    do i = 1, 4
!        call a_b(a, b, i)
!    end do
!    !$omp end parallel do
!end program he
    
!subroutine a_b(a, b, i)
!    !use omp_lib
    
!    implicit none
    
!    integer, intent (inout) :: a, b, i
    
!    a = a + i 
!    b = b + i
    
!    print *, "a, b: ", a, b!, omp_get_thread_num()
!end subroutine a_b


!!=============================
!program he
!    use omp_lib
!    !Okay, the save statement works in the following positions.
!    implicit none
!    !save
!    integer a, b, i, c
!    !save
!    common /dat/ a, b
!    !save                            !Here save is to save the v
!    !$omp threadprivate(a, c)      
!                                     !Here you have to use the common
!                                     !block name if you want to set the
!                                     !variable in the block as private.
!    save
!    a=0
   
!    b=0
!    c=0
    
!    !save          !The save statement does not  work here.
!    !$omp parallel do 
!    do i = 1, 4
!        a = a + i
!        c = c + i
!        !$ print *, i, a-c, b, omp_get_thread_num()
!    end do
!    !$omp end parallel do
!end program he

!!================================
!program he
!    use omp_lib
    
!    implicit none
    
!    if (2 .gt.1) go to 100
!    write(*,*) "false"
!!$ 100 write(*,*) "true"

!end program he


!!===============
!program test2
!    use omp_lib
    
!    implicit none
    
!    integer :: i
!    !$omp parallel do default(private)
!    do i =1, 5
!        write(*,*) "i = ", i, omp_get_thread_num()
!    end do
!    !$omp end parallel do
!end program test2

!!=========================================
!       program he
!      use omp_lib

!      implicit none
      
!      integer :: i
!!$omp parallel   do   
!      do i = 1, 9
!!$        write(*,*) "Before: ", omp_get_thread_num()
!!$omp barrier
!      stop
!!$    write(*,*) "After: ", omp_get_thread_num()
      
!      end do
!!$omp end parallel     do
      
      
      
!      end program he


!!===========================================
!!This is to test the stop statement
!program he
!    !$ use omp_lib
!    implicit none
    
!    write(*,*) "Before stop statement."
    
!    !$ stop
    
!    write(*,*) "After the stop statement."
!    !$ stop

!end program he




!!==========================================
!!This program is to test something
!program test
!    !$ use omp_lib
    
!    implicit none
    
!    integer :: i, j
    
!    !$omp parallel do ordered
!    do i=1 ,9
!        !$omp ordered
!        !$ write(*,*) omp_get_thread_num()
!        call thr(i)
!        do j = 1, 1
!            write(*,*) "hel"
!        end do
!        !$omp end ordered
!    end do
!    !$omp end parallel do  
!end program test

!subroutine thr(i)
!    !$ use omp_lib
    
!    implicit none
    
!    integer, intent(in) :: i
    
!    write(*,*) i, "Inside subroutine, thread: ", omp_get_thread_num()

!end subroutine thr  


!!==================================================
!!This is to test the ampersand
!program he
!    implicit none
!    integer :: a=2, &
!    b=3
    
!    print *, a,b, "helll &
!    &hell"
!!In fortran 77, there is no such thing, that's why we have 72 columns limit.
!end program he


!!================================================
!!This is to test the omp_get_num_threads()
!program get_num_thread
!    !$ use omp_lib
    
!    implicit none
    
!    integer :: i
    
!    !$ write(*,'(a, i2)') "The maximum number of threads is",omp_get_max_threads(), &
!    !$ &", the number of processors is ", omp_get_num_procs()
    
!!    !$ call omp_set_num_threads(3)
    
!!    !$omp parallel do ordered default(private)
!!        !do i = 4, 1, -1
!!        do i = 1, omp_get_max_threads()
!!        !$omp ordered
!!        !Be aware that when line is truncated, you must add &&, otherwise error!
!!            write(*,'(a, i2, a, i2, a, i2)'), "The number of working threads is&
!!            &",omp_get_num_threads(), ", this&
!!            & is thread", omp_get_thread_num(),", i=", i
!!        !$omp end ordered
!!        end do
        
!!    !$omp end parallel do 
!    print *, "This is the end!"
!end program get_num_thread


!!========================================================
!!This is to test the function which can print omp_get_thread_num()
!program fun_omp
!    use omp_lib
    
!    implicit none
    
!    integer:: i, thread_num
!    integer, external:: omp
    
!    !$omp parallel do
!        do i=1, 8
!        !$ thread_num=omp_get_thread_num()
!        print *, omp(i, thread_num), thread_num
!        !call omp(i, thread_num)
!        end do
!    !$omp end parallel do
!end program fun_omp

!integer function omp(i, thread_num)
!    implicit none
    
!    integer, intent(in):: i, thread_num
    
!    !print *, "Iteration: ", i, "thread_num: ", thread_num   
!    omp=i
!end function omp


!!========================================================
!!This is to test the reduction and atomic
!program red_ato
!    use omp_lib
    
!    implicit none
    
!    integer:: i, summ2=0, mul2=0, summ1, mul1
!    integer, parameter:: n=340
!    integer, dimension(n):: a, b
    
!    a=[(i, i=1, n)]
!    b=a
    
!    summ1=sum(a)
!    mul1=dot_product(a, b)
!    print*, summ1, mul1
    
!    !$omp parallel do num_threads(8) reduction(+: summ2) reduction(-:mul2)
!        do i = 1, n
!        print *, omp_get_thread_num()
!        !!$omp atomic
!            summ2= summ2+a(i)
!        !!$omp atomic
!            mul2=mul2-a(i)*b(i)*(-1)
!        end do
!    !$omp end parallel do
    
!    print *, new_line(' '), summ1-summ2, mul1-mul2
    
!end program red_ato




!!=======================================================
!!This is to test ordered
!program ord
!    use omp_lib
    
!    implicit none
    
!    integer:: i
    
!    !$omp parallel do private(i) ordered
!        do i = 1, 8
!            !$omp ordered
!            print *, "i: ", i, " thread_num: ", omp_get_thread_num()
!            !$omp end ordered
            
!            print *, "i: ", i
!            print *, "2i: ", 2*i
!            write(*, *), "3i: ", 3*i
            
!        end do
!    !$omp end parallel do
!end program ord 

!!========================================================
!!This is to test num_threads()
!program para_num
!    use omp_lib
    
!    implicit none
    
!    integer:: a=1, n=7
    
!    !$omp parallel if(a<2) num_threads(n) firstprivate(n)
!    !$    print *, "Num_threads: ", n, omp_get_thread_num()
!    !$omp end parallel
!    !if() is not true, then the master thread execute the operation.
!    !$omp parallel if(a>=2) num_threads(a) firstprivate(a)
!    !$    print *, "Num_threads: ", a, omp_get_thread_num()
!    !$omp end parallel

!end program para_num


!!=======================================================
!!This is to test if 
!program pra_if
!    use omp_lib
    
!    implicit none
    
!    logical:: tf=.true.
!    integer:: i=2, a=3
    
!    !$omp parallel if(i==22 .and. tf .eqv. .true.) firstprivate(a)
!    !$    print *, a, "thead_num: ", omp_get_thread_num()
    
!    !$omp end parallel 

!end program pra_if


!!=======================================================
!!This is to test lastprivate
!program lapr
!    use omp_lib
    
!    implicit none
    
!    integer:: a=3, i
    
!    !$omp parallel do lastprivate(a)
!    !Never put anything  between parallel do ad do loop. last private () does not work wit parallel.
!        do  i = 1, 3
!            a =2
!            print*, a, omp_get_thread_num()
!        end do
!    !$omp end parallel do
!    print *, a
!end program lapr



!!=======================================================
!!This is to test master block.
!program  para_mas
!    use omp_lib
    
!    implicit none
    
!    integer :: i, b =34
!    integer, dimension(8):: a
    
!    print *, "default num_thread: ", omp_get_max_threads()
    
!    !$omp parallel firstprivate(b)
        
!        !$omp master
!            print *, "The master thread_num: ", omp_get_thread_num()
!            b=4
!        !$omp end master
        
!        !$omp sections
!            !$omp section 
!            print *, "thread_num: ", omp_get_thread_num()
!!private(a), a value will be a random value if not assigned in each thread, firstprivate() will enable us to use the value in front of the scope, but it can also be changed in each thread.          
!            do i = 1,2
!                a(i)=b
!            end do
!            b=23
!            print *, b
            
!            !$omp section 
!            print *, "thread_num: ", omp_get_thread_num()
!            do i = 3,4
!                a(i)=b
!            end do
            
!            !$omp section 
!            print *, "thread_num: ", omp_get_thread_num()
!            do i = 5, 6
!                a(i)=b
!            end do
            
!            !$omp section 
!            print *, "thread_num: ", omp_get_thread_num()
!            do i = 7, 8
!                a(i)=b
!            end do
            
!        !$omp end sections
!    !$omp end parallel 
!    print *, a
!end program para_mas



!!=======================================================
!!This is to test atomic
!program ato_para
!   ! use omp_lib
    
!    implicit none
    
!    integer:: i, count1, count2, count_rate, max_count
!    integer, dimension(4):: a=0 
!    real,dimension(4):: b=0.0
!    call system_clock(count1, count_rate, max_count)
!    !$omp  parallel do
!    do i = 1, 40
!    !$omp atomic
!        a(i) = i+a(i)
!    !$omp atomic    
!        b(i) = b(i)+exp(i*1.0)
!        !print *, i-omp_get_thread_num()
!    end do
    
!    !$omp end parallel do
!    call system_clock(count2, count_rate, max_count)
!    !print *, count2-count1
    
!    !$omp atomic
!    a(1)=a(1)-2
!    print *, a
!    print *, b
!end program ato_para






!!=======================================================
!!This is to test sch_gui
!program sch_gui
!    use omp_lib
    
!    implicit none
    
!    integer:: i, thread_num, num_thread=5
    
!    !!$ call omp_set_num_threads(num_thread)
    
!    !$omp parallel do schedule(guided) private(i, thread_num) ordered
!        do i=1, 13
!        !$omp ordered
!            thread_num=omp_get_thread_num()
!            print *, "i: ", i, "thread_num: ", thread_num
!        !$omp end ordered
!        !ordered can be used to eleminate race conditions.
!        end do
!!Static specifies    
!    !$omp end  parallel do
!end program sch_gui


!!=======================================================
!!This is to test schedule dynamic
!program sch_dyn
!    use omp_lib
    
!    implicit none
    
!    integer :: i, num_thread=4, thread_num
    
    
!    !$ call omp_set_num_threads(num_thread)
    
!    !$omp parallel do ordered schedule(dynamic,2)
!        do i = 1, 13
!        !$omp ordered
!            thread_num=omp_get_thread_num()
!            print *, "i: ", i, "thread_num: ", thread_num
!        !$omp end ordered
        
!        end do
    
!    !$omp end parallel do


!end program sch_dyn



!!=======================================================
!!This is to test section
!program sec_paral
!    use omp_lib
    
!    implicit none
    
!    integer::  num_thread=4
    
!    !$ call omp_set_num_threads(num_thread) 
!    !Never put sections behind parallel, it's not valid.
!    !$omp parallel 
!    !$omp sections
!        !$omp section
!            print *, "1: ", omp_get_thread_num()
        
!        !$omp section 
!            print *, "2: ", omp_get_thread_num()

!    !$omp end sections
    
!    print *, "t: ", omp_get_thread_num()
!    !$omp end parallel

!end program sec_paral


!!=======================================================
!!This program is to test schedule static 
!program sch_sta
!    use omp_lib
    
!    implicit none
    
!    integer :: i, num_thread=4, thread_num
    
!    !$ call omp_set_num_threads(num_thread)
    
!    !$omp parallel do  schedule(static,6) ordered 
    
!        do i = 1, 12
!        !$omp ordered
!            thread_num= omp_get_thread_num()
!            print *, "i: ", i, "thread_num: ", thread_num
!        !$omp end ordered
!        end do
!    !$omp end parallel do


!end program sch_sta





!!======================================================
!!This is to test workshare in do loop
!program wor_do
!    use omp_lib
    
!    implicit none
    
!    integer:: num_thread=4
!    integer, dimension(3):: a, b=1, c
    
!    !$ call omp_set_num_threads(num_thread)
    
!    !$omp parallel workshare
!!    !$omp parallel
!!        !$omp workshare
!        a=b
!        c=a+b
!        !It does not work for do loop between workshare.
        
!!        !$omp end workshare
!!    !$omp end parallel 
!    !$omp end parallel workshare
!    print *, c
!end program wor_do


!!=====================================================
!!This is to test workshare 
!program wor_pa
!    use omp_lib
    
!    implicit none
    
!    integer:: num_thread=4, i, count1, count_rate, max_count
!    integer, dimension(10):: a, b=1, c
    
!    !$ call omp_set_num_threads(num_thread)
    
!    !$omp parallel 
!        !$omp sections 
       
!            !$omp section 
!                print *, "section 1: "
!                do i =1, 3
!                print *, "i: ", i
!                end do
!                print *, "1done!"
!                print *, "thread_num: ", omp_get_thread_num()
        
!            !$omp section
!                print *, "section 2: "
!                print *, omp_get_thread_num()
        
!        !$omp end sections 
       
!        !$omp single
!            do i = 1, 3
!                print *, "i: ", i
!            end do
!            print *, "2done!"
!            print *, "thread_num: ", omp_get_thread_num()
!        !$omp end single
        
        
!        !$omp single
!            print *, "3done!"
!            print *, "thread_num: ", omp_get_thread_num()
!        !$omp end single
        
!        !$omp workshare
!            a=b
!            c=a+b
!        !$omp end workshare
        
!    !$omp end parallel
    
!    print *, c
    
!!    call system_clock(count1, count_rate, max_count)
!!    print *, real(max_count)/count_rate, count1, count_rate, max_count
    
!end program wor_pa


!!========================================================
!!This is to verify real()/real()
!program re_re
!    implicit none
!    real:: a=3.23
!    integer:: b=4
    
!    print *, int(a), int(a)/b, a/real(b), a/b

!end program re_re


!=========================================================
!program test_system_clock

!  integer :: count1, count2, count_rate, count_max, i, j
!  integer, dimension(100, 100):: a
  
!  call system_clock(count1, count_rate, count_max)
  
!  do i =1, 100
!    do j = 1, 100
!        a(i, j)=i+j
!    end do
!  end do
  
!  call system_clock(count2, count_rate, count_max)
  
!  print *, "Elapesed time: ", count1, count2, real(count2-count1)/real(count_rate)
  
!end program test_system_clock


!!========================================================
!!This program is to test the workshare block
!program wosh
!    use omp_lib
    
!    implicit none
    
!    integer:: thread_num, num_thread=4, i
!    integer, dimension(23):: a, b
    
    
!    !$ call omp_set_num_threads(num_thread)
    
!    !$omp parallel
!    !$omp  workshare
    
!    !$omp end workshare
!    !$omp end parallel  
    
    
!end program wosh





!!=========================================================
!!This is to test parallel do
!program para_do
!    use omp_lib
    
!    implicit none
    
!    integer:: i, thread_num, num_thread=4
    
!    !$ call omp_set_num_threads(num_thread)
    
!    !$omp parallel  
!        !!$omp critical
!        do i = 1, 23
!        !$omp critical
!        !!$ thread_num= omp_get_thread_num()
!        !!$omp ordered
!        !$ thread_num= omp_get_thread_num()
!        print *, i, thread_num
!        !!$omp end ordered
!        !$omp end critical
!        end do
!        !!$omp end critical
!    !$omp end parallel

!end program para_do



!!=========================================================
!!This program is to test single directive. There are two ways of making OMP 
!!reduce the actural running time. Parallel do and parallel(manually assign
!!workload to each thread.)
!program sing_para
!    use omp_lib 
    
!    implicit none
    
!    integer :: i, ista, iend, num_thread=4, thread_num, n=2000, ppt
!    real, parameter:: pi = 2*asin(1.0)
!    real:: resul1=0.0, resul2=0.0, dx
    
!    ppt=(n+num_thread-2)/num_thread
!    dx = pi/2/(n-1)
!    ista=1
!    iend=n-1
    
!    !$ call omp_set_num_threads(num_thread)
    
!    !$omp parallel default(none) private (thread_num, i, ista, iend, resul1)&
!    !$omp shared(n, dx, ppt, resul2)
    
!    !$ thread_num= omp_get_thread_num()
!    !$ ista=ppt*thread_num+1
!    !$ iend=min(n-1, ppt*(thread_num+1))  
    
!    !$omp critical 
!    do i=ista, iend
    
!        resul1=resul1+sin(dx*i)*dx
!!    !$    print *, "thread_num: ", omp_get_thread_num(), i
!!    !$    print *, thread_num, i
    
!    end do
!    !$omp end critical
!    !The above critical block makes the output in i order in each thread.
!    !$omp critical
!    resul2=resul2+resul1
!    print *, resul1, "resul2: " ,resul2, omp_get_thread_num()
!    !$omp end critical
    
!    !$omp barrier
    
!    !$omp single
!        print *, "The final value is: ", resul2, omp_get_thread_num()
!    !$omp end single
    
!    !$omp end parallel
    
  
!    print *, resul2
    
!!Just found that sin(double precision parameter) is valid while 
!!sin(double precision variable) is not allowed.
!end program sing_para



!!========================================================
!!This is to test the private clause in the parallelism. 
!program pri_p
!    use omp_lib
    
!    implicit none
    
!    integer:: num_thread=8, thread_num, i, j
    
!    write(*,*), "Before: ", i, j, thread_num, new_line(' ')
    
!    !$ call omp_set_num_threads(num_thread)
!    !$omp parallel do lastprivate(thread_num) lastprivate(j) private(i) ordered
!    do i = 1, 200
!        !$omp ordered
!        !$ thread_num = omp_get_thread_num()
!        !$ print *, "Middle: ", i, j, thread_num
!        !$omp end ordered
!    end do
!    !$omp end parallel do 
    
!    print *, new_line(' '), " End:    ", i, j, thread_num
    
!!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!!It seems that only the clauses should not be there in omp parallel. Only when 
!!do loop? 
!end program pri_p


!!=====================================================================
!!This program is to test the private clause
!program pri_para
!    use omp_lib
    
!    implicit none
    
!    integer:: num_thread=4, thread_num, i
    
!    !$ call omp_set_num_threads(num_thread)
    
!    print *, "Before scope, ", thread_num
!    !$omp parallel do private(thread_num) ordered
    
!        do i= 20, 1, -1
    
!    !$omp ordered   
!    !!$omp critical
!        !!$ print *, "inside the scope, ", thread_num, i
    
!        !$ print *, "omp_get_thread_num", i
!        !$ print *, omp_get_thread_num(), i
        
!    !!$omp end critical
!    !$omp end ordered
    
!        end do
        
!    !$omp end parallel do

!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!! !$omp parallel ...!$omp parallel only executes the operations for all threads
!! in a completely random order. !$omp parallel do ... !$omp end parallel do: it
!! reduces the executive time.

!! !$omp parallel do ordered, !$omp ordered ... !$omp end ordered: is to make the
!! output in the thread order.

!! !$omp critical ... !$omp end critical: is to make the operations in between
!! execute in serial.

!end program pri_para




!!========================================================
!!This is to test the section parallel 
!program sec_par
!    use omp_lib
    
!    implicit none
    
!    integer:: i, num_thread=2, thread_num
    
!    !$ call omp_set_num_threads(num_thread)
    
!    !$omp parallel
    
!    !$omp sections private(i, thread_num)
    
!        !$omp section
!        !$ thread_num = omp_get_thread_num()
!        do i = 1, 3
!        write(*, '( "section 1, thread_num: ", i2 )'), thread_num
!        end do
        
!        !$omp section 
!        !$ thread_num = omp_get_thread_num()
!        do i = 1, 3
!        write(*, '( "Section 2, thread_num: ", i2 )'), thread_num
!        end do
        
!        !$omp section 
!        !$ thread_num = omp_get_thread_num()
!        do i = 1, 3
!        write(*, '( "Section 3, thread_num: ", i2 )'), thread_num
!        end do
!    !$omp end sections
   
!    !$omp end parallel

!end program sec_par


!!=======================================================
!!This program is to test the clause parallel
!program clau_paral
!    use omp_lib
    
!    implicit none
    
!    integer :: i, ista, iend, num_thread=7, ppt, thread_num
!    integer, parameter:: m=2600
!    real:: dx
!    real, dimension(m) :: resul
!    real, parameter:: pi= 2*asin(1.0)
    
!    dx=2*pi/(m-1)
!    ppt=(m+num_thread-1)/num_thread
    
!    ista=1
!    iend=m
    
!    !$ call omp_set_num_threads(num_thread)
    
!    !$omp parallel default(none) private(ista, iend, i, thread_num) shared(dx, resul, ppt)
    
!    !$ thread_num = omp_get_thread_num()
!    !$ ista = thread_num*ppt
!    !$ iend = min(m, thread_num*ppt+ppt-1)
    
!    !!$omp critical
!        !$ write(*,'( "thread_num: ", i2, " ista: ", i4, " iend: ", i4 )'), thread_num, ista, iend
    
!    !!$omp end critical
    
    
!    do i = ista, iend
!        resul(i) = sin( dx*(i-1) )**2 + cos( dx*(i-1) )**2
!        !print *, resul(i)
!    end do
    
    
!    !$omp end parallel 
    
!    !print *, resul
!    !print *, dx ,2*pi/(m-1)
    

!end program clau_paral


!!===================================================
!program x
!    implicit none
    
!    double precision :: pi=2*asin(1.0d+0)
    
!    print *, pi, sin(pi)       !???How to fix this problem caused by
!                               ! sin(pi)
!end program x


!!======================================================
!!This
!program test
!    implicit none
    
!    integer :: i
!    integer, parameter:: m=5
    
!    integer, dimension(m-2:m):: a
    
!    do i=3, 5
!        a(i) = i
!    end do
    
!    print *, a   !it only outputs 3 elements

!end program test


!!========================================================
!program a
!    implicit none
    
!    double precision :: pi=asin(1.0d+0)*2
!    !real:: b=2.0
    
!    print *, 1.0*pi

!end program a


!!=========================================================
!program para_mul      !Segmentation error? Use ulimit -s 65000 in the
!                      !current running directory.
!    use omp_lib
    
!    implicit none
    
!    integer:: i, j, k, num_thread=4, ppt, ista, iend, thread_num, count1, count2&
!    &, count_rate, max_count
!    integer, parameter:: m=7500, n=1000, o=1000 !m, n, o must be parameters
!                                             ! not varables.
!    real, dimension(1:m, 1:n) :: a
!    real, dimension(1:n, 1:o) :: b
!    real, dimension(1:m, 1:o) :: c
!    real :: t1, t2, t3, ep
!    a=1.0
!    b=1.0
!    c=0.0
!    ppt = (m+num_thread-1)/num_thread
!    ista=1
!    iend = m
    
!    !$ call omp_set_num_threads(num_thread)

!    call cpu_time(t1)
!    call system_clock(count1, count_rate, max_count)
!    !$omp parallel default(none) private(i, ista, iend, j, k, thread_num) shared(a, b, c, ppt)
!    !$ thread_num = omp_get_thread_num()
!    !$ ista = 1+ thread_num*ppt
!    !$ iend = min(m, ista+ppt-1)
    
!    !$ write(*, '( "thread_num: ", i2, " ista: ", i4, " iend: ", i4 )'), thread_num, ista, iend
!        do i = ista, iend
!            do j = 1, o
!                do k = 1, n
!                    c(i,j)= c(i,j)+a(i,k)*b(k,j)
!                end do
!            end do   
!        end do
!    !$omp end parallel 
!    call cpu_time(t2)
    
!    ep = t2-t1
!    call cpu_time(t3)
!    call system_clock(count2, count_rate, max_count)
    
!    !$ ep = ep
    
!    print *, ep, real(count2-count1)/real(count_rate)

!end program para_mul


!!=========================================================
!!This program is to find the number of threads and its maximum value.
!program thred_max
!    use omp_lib
    
!    print *, omp_get_num_procs(),omp_get_num_threads(), omp_get_max_threads()


!end program thred_max

!!=========================================================
!!This is to test the open mp
!!This program is to practice omp_set_num_threads() and omp_get_thread_num()
!program OP
!    use omp_lib

!    implicit none
    
!    integer:: num, num_thread=40
!    real:: t1, t2
    
!    !$ call omp_set_num_threads(num_thread)
    
!    !omp parallel is to make sure every thread does these operations.
!    call cpu_time(t1)
!    !$omp parallel     
!      !$omp critical 
!        !$ num = omp_get_thread_num()
!        write(*,*), "The thread number is ", num
    
!      !$omp end critical 
      
!    !$omp end parallel
!    call cpu_time(t2)
    
!    print *, t2-t1
!end program OP
