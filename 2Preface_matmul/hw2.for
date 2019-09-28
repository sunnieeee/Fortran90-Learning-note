program hw_two
implicit none
    real*8 :: A(5,5)
    real*8 :: b(5,1)
    integer :: i
    real*8 :: Ab(5,1)
    real*8 :: vAv(1,1)
    
    !open A.txt    
    open(file="A.txt",unit=10,action="read")
    read(10,*)A
    A=transpose(A)
    print "(a)","matrix A elements:"
    print "(5f8.1)", (A(i,:),i=1,5)

    !open b.txt
    open(file="b.txt",unit=10,action="read")
    read(10,*)b
    print "(a)","vector b elements:"
    print "(1f8.1)", (b(i,:),i=1,5) 

    call subhw_two(A,b,Ab,vAv)
    print "(a)", "vector A*b is:"
    print "(1f8.1)", (Ab(i,:),i=1,5)

    print "(a)", "scalar b^T*A*b is:"
    print *, vAv

end program hw_two


 
subroutine subhw_two(Mat,v,Matv,vMatv)
implicit none
    real*8 :: Mat(5,5)
    real*8 :: v(5,1)
    integer :: j
    real*8 :: Matv(5,1)
    real*8 :: vMatv(1,1)
    
    Matv = matmul(Mat,v)

    vMatv = matmul(matmul(transpose(v),Mat),v)
    
end subroutine subhw_two

