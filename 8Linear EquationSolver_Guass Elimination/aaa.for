Program GE
    implicit none

    !**************变量声明****************!
    integer :: i
    real*8 :: matbb(9,1),matA(9,9),matb(1,9),matAb(9,10)

    !**************读取矩阵****************!
    open( file="b.txt", unit=10, action="read" )
    read(10,*)matbb
    matb = transpose(matbb)

    open( file="A.txt", unit=10, action="read" )
    read(10,*)matA
    matA = transpose(matA)

    print "(a)", "vector b is:"
    print "(9f8.1)",(matb(:,i),i=1,9)

    print "(a)", "matrix A is:"
    print "(9f8.1)",(matA(i,:),i=1,9)

    !**************增广矩阵****************!


    !****************三角化****************!
    

end program GE 