program DoolittleDecomposition
!A,b,L,U,y,x
!i.j.n
!sum
    real*8 :: a(9,9),l(9,9),u(9,9),bt(9,1),b(1,9)
    real*8 :: x(9),y(9)
    integer :: i,j,k,r
    real*8 :: sum 

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

    !!Solve L&U
    do i=1,9
        do j=i,9
            u(i,j)=0
            l(i,j)=0
        enddo 
    enddo
   
    do k=1,9
        i=k
        do j=k,9
            sum = 0 
            do r=1,k-1
                sum = sum +l(k,r)*u(r,j)
            enddo
            u(k,j)=a(k,j)-sum
        enddo 
        j=k
        do i=k,9
            sum = 0 
            do r=1,k-1
                sum = sum +l(i,r)*u(r,k)
            enddo
            l(i,k)=(a(i,k)-sum)/u(k,k)
        enddo 
    enddo
    
    print "(a)", "matrix L is:"
    print "(9f8.3)",(l(i,:),i=1,9)
    print "(a)", "matrix U is:"
    print "(9f8.3)",(u(i,:),i=1,9)
    
    
    !!Solve y with Ly=b
    y(1) = b(1,1)/l(1,1)
    do i = 2,9
        sum = 0
        do j = 1,i-1
            sum = sum + l(i,j)*y(j)
        end do
        y(i) = ( b(1,i)-sum )/ l(i,i)
    end do
    do i = 1,9
        print *,  "y", i , "=",y(i)
    end do
    
    !!Solve x with Ux=y
    x(9) = y(9)/u(9,9)
    do i = 8,1
        sum = 0
        do j = 9,i+1
            sum = sum + u(i,j)*x(j)
        end do
        x(i) = ( y(i)-sum )/ u(i,i)
    end do
    do i = 1,9
        print *,  "x", i , "=",x(i)
    end do


end program DoolittleDecomposition