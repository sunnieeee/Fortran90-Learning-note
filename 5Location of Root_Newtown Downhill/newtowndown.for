program newtowndown
implicit none

    !******************Newton Downhill********************!
    real*8 :: x_k0,x_k1,x_0,x_1
    real*8 :: fx_k0,fx_k1
    real*8 :: error_1,error_2,errorlam,kexi
    integer :: lambda
    integer :: k  !记迭代次数

    !********* Define constant *********!
    error_1 = 0.00001
    error_2 = 0.00001
    errorlam = 0.001
    kexi = 0.001
    lambda = 1
    k = 0

    print *, "input x_0:"
    read *, x_0
    print *, "input x_1:"
    read *, x_1

    fx_k0 = x_0**3-3*x_0
    fx_k1 = x_1**3-3*x_1
    x_k0 = x_0
    x_k1 = x_0 - lambda * (fx_k1)*(x_1-x_0)/((fx_k1)-(fx_k0))
     

    do while ( abs(fx_k0)<error_1 .or. abs(x_k1 - x_k0)<error_2 )
       
        if( abs(fx_k1)>=abs(fx_k0) ) then
            
            if(lambda>errorlam) then  
                lambda = lambda/2
            else
                x_0 = x_0+kexi
            end if

            fx_k0 = x_0**3-3*x_0
            fx_k1 = x_1**3-3*x_1
            x_k0 = x_0
            x_k1 = x_0 - lambda * (fx_k1)*(x_1-x_0)/((fx_k1)-(fx_k0))
        
        else
            fx_k0 = fx_k1
            fx_k1 = x_k1**3-3*x_k1
            x_k0 = x_k1
            x_k1 = x_k0 - lambda * (fx_k1)*(x_k1-x_k0)/((fx_k1)-(fx_k0))
    
        end if

        k = k+1

    end do

    print *, "an aprroximate root is:",x_k1
    print *, "iteration steps:",k

end program newtowndown