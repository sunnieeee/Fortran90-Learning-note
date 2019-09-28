program main
implicit none
    integer :: m,n
    print *,"Input m="
    read *,m
    print *,"Input n="
    read *,n
    if(m<=n) then
        call PerANDCom(m,n)
    else
        print *,"Wrong Num!"
    end if
end program main
    
!求解Per和Com
subroutine PerANDCom(x,y)
implicit none
    
    real*8 :: p,c
    integer :: x,y
    integer :: xx,yy,yx
    
    call Factorial(x,xx)
    print *,"m!=",xx
    call Factorial(y,yy)
    print *,"n!=",yy
    call Factorial(y-x,yx)
    print *,"(n-m)!=",yx
    
    p=yy/yx
    c=p/xx
    print *,"P(m,n)=",p
    print *,"C(m,n)=",c
end subroutine PerANDCom
    
!求解某个数字的阶乘
subroutine Factorial(t,tt)
implicit none
    integer::t,tt,k
    tt=1
    k=1
    do while(k<=t)
        tt=tt*k
        k=k+1
    end do
end subroutine Factorial