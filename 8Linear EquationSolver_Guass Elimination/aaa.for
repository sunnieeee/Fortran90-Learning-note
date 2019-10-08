Program GE
    implicit none

    !**************变量声明****************!
    integer :: i
    real*8 :: matb(1,9),matA(9,9)

    !**************读取矩阵****************!
    open( file="b.txt", unit=10, action="read" )
    read(10,*)matb
    matb = transpose(matb)

    open( file="b.txt", unit=10, action="read" )
    read(10,*)matA
    matb = transpose(matA)

    print "(a)", "vector b is:"
    print "(9f8.0)",(matb(:,i),i=1,9)

    print "(a)", "matrix A is:"
    print "(9f8.0)",(matA(i,:),i=1,9)

    !****************三角化****************!
    

end program GE