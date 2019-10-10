program GS_Overrelaxation
!A,b,L,U,y,x
!i.j.n
!sum
    real*8 :: a(9,9),d(9,9),l(9,9),u(9,9),bt(9,1),b(1,9)
    real*8 :: x0(9),x1(9)
    integer :: i,j,k
    real*8 :: w 
    real*8 :: G(9,9),g(9,1)

    !**************读取矩阵****************!
    open( file="A.txt", unit=10, action="read" )
    read(10,*)a
    a = transpose(a)
    open( file="b.txt", unit=10, action="read" )
    read(10,*)bt
    b = transpose(bt)

    print "(a)", "matrix A is:"
    print "(9f8.3)",(a(i,:),i=1,9)
    print "(a)", "vector b is:"
    print "(9f8.3)",(b(:,i),i=1,9)

    !!Solve D&L&U
    do i=1,9
        do j=i,9
            d(i,j)=0
            u(i,j)=0
            l(i,j)=0
        enddo 
    enddo
   
    do k=1,9
        d(k,k)=a(k,k)
    enddo

    do i=1,9
        do j=i+1,9
            u(i,j)=-a(i,j)
        enddo 
    enddo

    do i=1,9
        do j=1,i-1
            l(i,j)=-a(i,j)
        enddo 
    enddo
    
    print "(a)", "matrix D is:"
    print "(9f8.3)",(d(i,:),i=1,9)
    print "(a)", "matrix L is:"
    print "(9f8.3)",(l(i,:),i=1,9)
    print "(a)", "matrix U is:"
    print "(9f8.3)",(u(i,:),i=1,9)
    
    
    do i=1,9
        x(i)=0
    enddo
    k=0


    w=1 !!GS



    w=1.4 !!Over


end program GS_Overrelaxation