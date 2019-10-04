program jacobi
    implicit none

    integer :: k
    real*8 :: x_k,x_k1,error
  
    print*, "input the maximum error:"
    read *,error

    !***************************  -1<x<1  **************************!
    k = 0
    print*, "input an initial value in the range of (-1,1):"
    read *,x_k
    x_k1 = (x_k**3)/3
    do while (abs(x_k1-x_k)>=error)
        x_k = x_k1
        x_k1 = (x_k**3)/3
        k = k+1    
    end do

    print *, "iteration steps:", k
    print *, "the first approximate root is:", x_k1
    print *, "the error is:", (abs(x_k1-0))
    
    !****************************   x<-3   **************************!
    k = 0
    print*, "input an initial value that is less than -3:"
    read *,x_k
    x_k1 = sign( abs(3*x_k)**(1.0d0/3.0d0),x_k )
    do while (abs(x_k1-x_k)>=error)
        x_k = x_k1
        x_k1 = sign( abs(3*x_k)**(1.0d0/3.0d0),x_k )   !sign()函数是取逗号前面的绝对值，取逗号后面的符号
        k = k+1    
    end do

    print *, "iteration steps:", k
    print *, "the second approximate root is:", x_k1
    print *, "the error is:", (abs(x_k1+3**(1.0/2.0)))

    !****************************   x>3   **************************!
    k = 0
    print*, "input an initial value that is over 3:"
    read *,x_k
    x_k1 = (3*x_k)**(1.0d0/3.0d0)
    do while (abs(x_k1-x_k)>=error)
        x_k = x_k1
        x_k1 = (3*x_k)**(1.0d0/3.0d0)
        k = k+1    
    end do

    print *, "iteration steps:", k
    print *, "the third approximate root is:", x_k1
    print *, "the error is:", (abs(x_k1-3**(1.0/2.0)))

end program jacobi