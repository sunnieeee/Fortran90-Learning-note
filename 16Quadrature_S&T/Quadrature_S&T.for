program ST
implicit none
    real*8 :: sum_fxk,h,sum_odd,sum_even
    real*8 :: trape_40,simp_40
    real*8 :: trape_20,simp_20
    real*8 :: errorTrape,errorSimp
    integer :: k

    h = 0.1
    sum_fxk = 0.0
    do k = 1,39
        sum_fxk = sum_fxk + sin(1.0 + k*h)
    end do
    trape_40 = 1/20.0 * ( sin(1.0) + 2*sum_fxk + sin(5.0) )

    sum_fxk = 0.0
    do k = 1,19
        sum_fxk = sum_fxk + sin(1.0 + k*h)
    end do
    trape_20 = 1/20.0 * ( sin(1.0) + 2*sum_fxk + sin(5.0) )
    errorTrape = 1/3.0 * (trape_40 -trape_20)
    
    print "(a)", "using repeated trapezoid quadrature:"
    print "(f8.6)", trape_40
    print "(a)", "repeated trapezoid quadrature error analysis:"
    print *, errorTrape
    print "(a)", "-------------------------------------------------"

    sum_odd = 0.0
    sum_even = 0.0
    k = 1
    do while (k<=39)
        sum_odd = sum_odd + sin(1.0+k*h)
        k = k+2
    end do
    k = 2
    do while (k<=39)
        sum_even = sum_even + sin(1.0+k*h)
        k = k+2
    end do
    simp_40 = 1/30.0 * ( sin(1.0) + 4*sum_odd + 2*sum_even +sin(5.0) )

    sum_odd = 0.0
    sum_even = 0.0
    k = 1
    do while (k<=19)
        sum_odd = sum_odd + sin(1.0+k*h)
        k = k+2
    end do
    k = 2
    do while (k<=19)
        sum_even = sum_even + sin(1.0+k*h)
        k = k+2
    end do
    simp_20 = 1/30.0 * ( sin(1.0) + 4*sum_odd + 2*sum_even +sin(5.0) )
    errorSimp = 1/15.0 * (simp_40 - simp_20)

    print "(a)", "using repeated Simpson quadrature:"
    print "(f8.6)", simp_40
    print "(a)", "repeated Simpson quadrature error analysis:"
    print *, errorSimp

end program ST