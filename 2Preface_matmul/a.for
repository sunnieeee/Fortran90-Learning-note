program hw_two
    implicit none
  

        !open A.txt    
        real*8 :: A(5,5)
        open(file="A.txt",unit=10,action="read")
        read(10,*)A
        A=transpose(A)

        integer :: i
        print "(a)","matrix A elements:"
        print "(4f8.3)", (A(i,:),i=1,5)


        !open b.txt
        real*8 :: b(5)
        open(file="b.txt",unit=10,action="read")
        read(10,*)b
        b=transpose(b)

        print "(a)","vector b elements:"
        print "(4f8.3)", (b(i),i=1,5) 
        
        call subhw_two(A,b)
    end program hw_two
    
    

    subroutine subhw_two(x,y) 
    implicit none
        
        real*8 :: x(5,5)
        real*8 :: y(5)

        real*8 :: vector(5)
        vector = matmul(x,y)
        print "(a)", "vector A*b is:"
        print "(4f8.3)", (vector(i),i=1,5)
       
        real*8 :: scalar(5,5)
        scalar = matmul(matmul(transpose(y),x),y)
        print "(a)", "scalar b^T*A*b is:"
        print "4f8.3", (scalar(i,:),i=1,5)

    end subroutine subhw_two
    