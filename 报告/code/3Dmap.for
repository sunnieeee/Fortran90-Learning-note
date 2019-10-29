program map
implicit none
    real*8 :: x(4),y(4)
    real*8 :: x1,x2,x3,x4,y1,y2,y3,y4
    real*8 :: h,u,lambda,M(4),differ(2)
    real*8 :: a(3),b(3),c(3),d(3)   !coefficients
    real*8 :: spline
    integer :: i
    real*8 :: temp

    data x /0.0,600.0,1200.0,1800.0/
    data y /30.0,20.0,15.0,30.0/
 
    h = 600.0    ! h=x_(i+1)-x_i
    u = h/(h+h)
    lambda = 1-u
    M(1) = 0
    M(4) = 0     !Boundary condition

    !********************三弯矩方程**********************!
    do i = 1,2
        differ(i) = 6/(h+h) * ( (y(i+2)-y(i+1))/h-(y(i+1)-y(i))/h )
    end do
    print "(a)", "differ(i) = "
    print "(f16.8)", differ(:)
    M(3) = (differ(1)- 2.0/u*differ(2)) / (lambda-4.0/u)  
    M(2) = (differ(1)-lambda*M(3))/2.0
    print "(a)", "M(i) = "
    print "(f16.8)", M(:)
    !**********************SOLVE COEFFICIENTS***********************!
    print "(a)", "Cubic spline interpolation in the first interval is:"
    do i = 1,3
        a(i) = ( M(i+1)-M(i) ) / (6.0*h)
        b(i) = ( x(i+1)*M(i)-x(i)*M(i+1) )/ (2.0*h)
        c(i) = 3* x(i)**2 * M(i+1)/(6.0*h) - 3* x(i+1)**2 * M(i)/(6.0*h) + (y(i+1)-y(i))/h -(M(i+1)-M(i))*h/6.0
        d(i) = M(i)* x(i+1)**3 /(6.0*h) - M(i+1)* x(i)**3/(6.0*h) + y(i+1) - M(i+1)* (h**2) /6.0 &
         - (y(i+1)-y(i)) *x(i+1)/h + (M(i+1)-M(i))*x(i+1)*h/6.0
      !  d(i) = M(i)* x(i+1)**3/(6.0*h) - M(i+1)* x(i)**3/(6.0*h) + y(i) * x(i+1)/h &
      !   - M(i)*x(i+1)*h/6.0 -y(i+1)*x(i)/h + M(i+1)*x(i)*h/6.0 
        print *, "a=",a(i)
        print *, "b=",b(i)
        print *, "c=",c(i)
        print *, "d=",d(i)
        print "(a,f8.3,a,f8.3,a,f16.3,a,f16.3)", "S(x)=",a(i),"x^3+",b(i),"x^2+",c(i),"x+",d(i)
        print "(a)", "-----------------------------------------------"
    end do
    
    print "(a)","----------------------test--------------------"
    temp = 0.0
    print *, spline(a(1),b(1),c(1),d(1),temp)
    do i = 1,3
        temp = i*600.0
        print *, spline(a(i),b(i),c(i),d(i),temp)
    end do
    print "(a)", "-----------------------------------------------"

end program map

function spline(aa,bb,cc,dd,variable)
implicit none
    real*8 :: spline
    real*8 :: aa,bb,cc,dd,variable
    spline = aa* variable**3 + bb* variable**2 + cc* variable +dd
end function spline