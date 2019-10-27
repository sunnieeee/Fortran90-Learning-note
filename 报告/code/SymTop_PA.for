program SymTop_PA
implicit none
    integer :: k
    real*8 :: x_k,x_k1,error
    real*8 :: funcU
    real*8 :: a,b,alpha,beita
    real*8 :: L

    print*, "input the parameter a:"
    read *,a
    print*, "input the parameter b:"
    read *,b
    print*, "input the parameter alpha:"
    read *,alpha
    print*, "input the parameter beita:"
    read *,beita  
    
    print*, "input the maximum error:"
    read *,error

    !***************************  -1<x<0  **************************!
    k = 0
    print*, "input an initial value in the range of (-1,0):"
    read *,x_k
    
    L = 0
    x_k1 = funcU(a,b,alpha,beita,x_k)         !不同于Jacobi acceleration
    do while (abs(x_k1-x_k)>=error)
        x_k = x_k1
        x_k1 = funcU(a,b,alpha,beita,x_k)
        k = k+1    
    end do

    print *, "iteration steps:", k
    print *, "the first approximate root is:", x_k1
    print *, "the error is:", (abs(x_k1-0))
    
    !***************************  0<x<1  **************************!
    k = 0
    print*, "input an initial value in the range of (0,1):"
    read *,x_k
    
    L = 0
    x_k1 = funcU(a,b,alpha,beita,x_k)         !不同于Jacobi acceleration
    do while (abs(x_k1-x_k)>=error)
        x_k = x_k1
        x_k1 = funcU(a,b,alpha,beita,x_k)
        k = k+1    
    end do

    print *, "iteration steps:", k
    print *, "the first approximate root is:", x_k1
    print *, "the error is:", (abs(x_k1-0))

end program SymTop_PA

function funcU(fa,fb,falpha,fbeita,u)
implicit none
    real*8 :: funcU
    real*8 :: fa,fb,falpha,fbeita,u
    funcU = ( -fbeita * u**3 + (falpha+fb**2) * u**2 - (falpha-fa**2) ) / ( 2*fa*fb-fbeita )
end function funcU