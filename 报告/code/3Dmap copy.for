program map
implicit none
    real*8 :: xx(4),yy(4)
    real*8 :: hh
    real*8 :: matmap(76,91)
    real*8 :: temp
    real*8 :: spline
    
    integer :: k

    !**********************在x轴方向上做插值************************!
    hh = 600.0    ! h=x_(i+1)-x_i
    data xx /0.0,600.0,1200.0,1800.0/
    
    yy(1:4) = (/30.0,20.0,15.0,30.0/)
    do k = 1,91
        temp = (k-1.0)*20.0
        matmap(76,k) = spline(xx,yy,hh,temp)
    end do
    !print "(76f8.3)", matmap(76,:)
    !print "(a)", "-----------------------------------------------------"
    yy(1:4) = (/15.0,100.0,140.0,350.0/)
    do k = 1,91
        temp = (k-1.0)*20.0
        matmap(51,k) = spline(xx,yy,hh,temp)
    end do
    !print "(76f8.3)", matmap(51,:)
    !print "(a)", "-----------------------------------------------------"
    yy(1:4) = (/10.0,95.0,200.0,120.0/)
    do k = 1,91
        temp = (k-1.0)*20.0
        matmap(26,k) = spline(xx,yy,hh,temp)
    end do
    !print "(76f8.3)", matmap(26,:)
    !print "(a)", "-----------------------------------------------------"
    yy(1:4) = (/20.0,45.0,25.0,35.0/)
    do k = 1,91
        temp = (k-1.0)*20.0
        matmap(1,k) = spline(xx,yy,hh,temp)
    end do
    !print "(76f8.3)", matmap(1,:)
    !print "(a)", "-----------------------------------------------------"

    !**********************在y轴方向上做插值************************!
    hh = 500.0
    xx(1:4) = (/0.0,500.0,1000.0,1500.0/)
    yy(1:4) = (/30.0,15.0,10.0,20.0/)
    do k = 1,76
        temp = (k-1.0)*20.0
        matmap(1,k) = spline(xx,yy,hh,temp)
    end do
    print "(76f8.3)", matmap(1,:)
    print "(a)", "-----------------------------------------------------"

end program map


function spline(x,y,h,variable)
implicit none
    real*8 :: cube
    real*8 :: spline
    real*8 :: x(4),y(4),h,variable
    
    real*8 :: a(3),b(3),c(3),d(3)   !coefficients
    real*8 :: u,lambda,M(4),differ(2)
    real*8 :: testx,testy
    integer :: i

    
    u = h/(h+h)
    lambda = 1-u
    M(1) = 0
    M(4) = 0     !Boundary condition

    !********************三弯矩方程**********************!
    do i = 1,2
        differ(i) = 6/(h+h) * ( (y(i+2)-y(i+1))/h-(y(i+1)-y(i))/h )
    end do
    !print "(a)", "differ(i) = "
    !print "(f16.8)", differ(:)
    M(3) = (differ(1)- 2.0/u*differ(2)) / (lambda-4.0/u)  
    M(2) = (differ(1)-lambda*M(3))/2.0
    !print "(a)", "M(i) = "
    !print "(f16.8)", M(:)
    !**********************SOLVE COEFFICIENTS***********************!
    !print "(a)", "Cubic spline interpolation in the first interval is:"
    do i = 1,3
        a(i) = ( M(i+1)-M(i) ) / (6.0*h)
        b(i) = ( x(i+1)*M(i)-x(i)*M(i+1) )/ (2.0*h)
        c(i) = 3* x(i)**2 * M(i+1)/(6.0*h) - 3* x(i+1)**2 * M(i)/(6.0*h) + (y(i+1)-y(i))/h -(M(i+1)-M(i))*h/6.0
        d(i) = M(i)* x(i+1)**3 /(6.0*h) - M(i+1)* x(i)**3/(6.0*h) + y(i+1) - M(i+1)* (h**2) /6.0 &
         - (y(i+1)-y(i)) *x(i+1)/h + (M(i+1)-M(i))*x(i+1)*h/6.0
      !  d(i) = M(i)* x(i+1)**3/(6.0*h) - M(i+1)* x(i)**3/(6.0*h) + y(i) * x(i+1)/h &
      !   - M(i)*x(i+1)*h/6.0 -y(i+1)*x(i)/h + M(i+1)*x(i)*h/6.0 
    !    print *, "a=",a(i)
    !    print *, "b=",b(i)
    !    print *, "c=",c(i)
    !    print *, "d=",d(i)
    !    print "(a,f8.3,a,f8.3,a,f16.3,a,f16.3)", "S(x)=",a(i),"x^3+",b(i),"x^2+",c(i),"x+",d(i)
    !    print "(a)", "-----------------------------------------------"
    end do

    !**************************OUTPUT****************************!
    if ( variable >= 0.0*h .and. variable < 1.0*h ) then
        spline = cube( a(1),b(1),c(1),d(1),variable )
    else if ( variable >= 1.0*h .and. variable < 2.0*h ) then
        spline = cube( a(2),b(2),c(2),d(2),variable )
    else if (variable >= 2.0*h .and. variable <= 3.0*h ) then
        spline = cube( a(3),b(3),c(3),d(3),variable )
    else
        print "(a)", "x is out of range!"
    end if
    
    !print "(a)", "----------------------test--------------------"
    !testx = 0.0*h
    !testy = cube(a(1),b(1),c(1),d(1),testx)
    !print *, testy
    !do i = 1,3
    !    testx = i*h
    !    testy = cube(a(i),b(i),c(i),d(i),testx)
    !    print *, testy
    !end do
    !print "(a)", "-----------------------------------------------"

end function spline

function cube(aa,bb,cc,dd,t)
implicit none
    real*8 :: cube
    real*8 :: aa,bb,cc,dd,t
    cube = aa* t**3 + bb* t**2 + cc* t +dd
end function cube