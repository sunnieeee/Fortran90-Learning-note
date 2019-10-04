program postacc
    implicit none

    integer :: k
    real*8 :: x_k,x_k1,error
    real*8 :: L
  
    print*, "input the maximum error:"
    read *,error

    !***************************  -1<x<1  **************************!
    k = 0
    print*, "input an initial value in the range of (-1,1):"
    read *,x_k
    
    L = 0
    x_k1 = (x_k**3)/3         !不同于Jacobi acceleration
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
    
    L = 1.0/(9.0*4.0)**(1.0/3.0)
    x_k1 = ( sign( abs(3*x_k)**(1.0d0/3.0d0),x_k ) - L*x_k )/(1-L)
    do while (abs(x_k1-x_k)>=error)
        x_k = x_k1
        x_k1 = ( sign( abs(3*x_k)**(1.0d0/3.0d0),x_k ) - L*x_k )/(1-L)
        k = k+1    
    end do

    print *, "iteration steps:", k
    print *, "the second approximate root is:", x_k1
    print *, "the error is:", (abs(x_k1+3**(1.0/2.0)))

    !****************************   x>3   **************************!
    k = 0
    print*, "input an initial value that is over 3:"
    read *,x_k
    
    L = 1.0/(3.0*2.0)**(2.0/3.0)
    x_k1 = ( (3*x_k)**(1.0d0/3.0d0) - L*x_k )/(1-L)
    do while (abs(x_k1-x_k)>=error)
        x_k = x_k1
        x_k1 = ( (3*x_k)**(1.0d0/3.0d0) - L*x_k )/(1-L)
        k = k+1    
    end do

    print *, "iteration steps:", k
    print *, "the third approximate root is:", x_k1
    print *, "the error is:", (abs(x_k1-3**(1.0/2.0)))

end program postacc