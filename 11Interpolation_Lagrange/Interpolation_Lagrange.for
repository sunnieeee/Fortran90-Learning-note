program Lagrange
    implicit none

    integer :: i,n
    real*8 :: x(16),y(16),coef(16)
    real*8 :: lx(9),ly(9)
    real*8 :: Times_xixj

    real*8 :: Lag   !******声明

    do i = 0,15
        x(i+1) = -5.0 + 10.0/15.0*i
    end do 

    do i = 1,16
        y(i) = 1/(1+x(i)**2)
    end do

    print "(a)","xi ="
    print "(f8.5)", x(:)
    print "(a)" ,"-------------------------------------------"
    print "(a)","yi ="
    print "(f8.5)", y(:)
    
    print "(a)" ,"-------------------------------------------"
    print "(a)","coefficients_1 ="
    do i = 0,15
        Times_xixj = 1.0
        do n = 0,i-1
            Times_xixj = Times_xixj*( x(i+1)-x(n+1) )
        end do
        do n = i+1,15
            Times_xixj = Times_xixj*( x(i+1)-x(n+1) )
        end do
        coef(i+1) = y(i+1)/Times_xixj
        print *, coef(i+1)
    end do


    data lx /-4.0,-3.0,-2.0,-1.0,0.0,1.0,2.0,3.0,4.0/
    do i = 1,9
        ly(i) = Lag(lx(i),x,coef)
    end do
    
    print "(a)","when x = "
    print "(f8.1)", lx(:)
    print "(a)","Lagrange(x) = "
    print , ly(:)

end program Lagrange


function Lag(variable,fun_x,fun_coef)
implicit none
    real*8 :: Lag,Times_xxj,variable
    real*8 :: fun_x(16),fun_coef(16)
    integer :: stamp,mm

    Lag = 0.0
    do stamp = 0,15
        Times_xxj = 1.0
        do mm = 0,stamp-1
            Times_xxj = Times_xxj*( variable-fun_x(mm+1) )
        end do
        do mm = stamp,15
            Times_xxj = Times_xxj*( variable-fun_x(mm+1) )
        end do
        Lag = Lag + fun_coef(stamp+1)*Times_xxj
    end do

end function Lag