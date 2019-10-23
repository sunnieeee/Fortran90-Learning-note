program RK
implicit none

    real*8 :: fxy

    real*8 :: h
    real*8 :: K1,K2,K3,K4
    real*8 :: xn
    integer :: interval
    real*8 :: y1(16),y2(31),y3(61),y4(121)

    h = 0.1
    y1(1) = 3.0
    do interval = 1,15
        xn = 0.0 + interval * h
        K1 = fxy( xn,y1(interval) )
        K2 = fxy( xn+h/2.0, y1(interval)+h/2.0*K1 )
        K3 = fxy( xn+h/2.0, y1(interval)+h/2.0*K2 )
        K4 = fxy( xn+h, y1(interval)+h*K3)
        y1(interval+1) = y1(interval) + h/6.0 * ( K1+2*K2+2*K3+K4 )
    end do 
    
    print "(a)", "when stepsize = 0.1, y= :"
    print "(16f8.3)", y1(:)
    print "(a)", "-----------------------------------------------------------------"

    h = 0.1/2.0
    y2(1) = 3.0
    do interval = 1,30
        xn = 0.0 + interval * h
        K1 = fxy( xn,y2(interval) )
        K2 = fxy( xn+h/2.0, y2(interval)+h/2.0*K1 )
        K3 = fxy( xn+h/2.0, y2(interval)+h/2.0*K2 )
        K4 = fxy( xn+h, y2(interval)+h*K3)
        y2(interval+1) = y2(interval) + h/6.0 * ( K1+2*K2+2*K3+K4 )
    end do 
    
    print "(a)", "when stepsize = 0.1, y= :"
    print "(31f8.3)", y2(:)
    print "(a)", "-----------------------------------------------------------------"

    h = 0.1/4.0
    y3(1) = 3.0
    do interval = 1,60
        xn = 0.0 + interval * h
        K1 = fxy( xn,y3(interval) )
        K2 = fxy( xn+h/2.0, y3(interval)+h/2.0*K1 )
        K3 = fxy( xn+h/2.0, y3(interval)+h/2.0*K2 )
        K4 = fxy( xn+h, y3(interval)+h*K3)
        y3(interval+1) = y3(interval) + h/6.0 * ( K1+2*K2+2*K3+K4 )
    end do 
    
    print "(a)", "when stepsize = 0.1, y= :"
    print "(31f8.3)", y3(:)
    print "(a)", "-----------------------------------------------------------------"

    h = 0.1/8.0
    y4(1) = 3.0
    do interval = 1,120
        xn = 0.0 + interval * h
        K1 = fxy( xn,y4(interval) )
        K2 = fxy( xn+h/2.0, y4(interval)+h/2.0*K1 )
        K3 = fxy( xn+h/2.0, y4(interval)+h/2.0*K2 )
        K4 = fxy( xn+h, y4(interval)+h*K3)
        y4(interval+1) = y4(interval) + h/6.0 * ( K1+2*K2+2*K3+K4 )
    end do 
    
    print "(a)", "when stepsize = 0.1, y= :"
    print "(31f8.3)", y4(:)
    print "(a)", "-----------------------------------------------------------------"

end program RK

function fxy(x,y)
implicit none
    real*8 :: x,y
    real*8 :: fxy
    fxy = - x**2 * y**2
end function