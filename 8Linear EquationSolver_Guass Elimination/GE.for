Program GE
implicit none

    !**************变量声明****************!
   
    integer :: i,j,n               !计数器
    integer :: pivot(1)               !记录列最大值下标
    real*8 :: matbb(9,1),matA(9,9),matb(1,9),matAb(9,10)
	real*8 :: temp(9),Tri(10)
	real*8 :: minval                !储存矩阵最小值，用于temp的初始化
    real*8 :: sum
    real*8 :: tmp

    !**************读取矩阵****************!

    open( file="b.txt", unit=10, action="read" )
    read(10,*)matbb
    matb = transpose(matbb)


    open( file="A.txt", unit=10, action="read" )
    read(10,*)matA
    matA = transpose(matA)


    print "(a)", "vector b is:"
    print "(9f8.1)",(matb(:,i),i=1,9)


    print "(a)", "matrix A is:"
    print "(9f8.1)",(matA(i,:),i=1,9)


    !**************增广矩阵****************!
    do i = 1,9
        do j = 1,9
            matAb(j,i) = matA(j,i) 
        end do
    end do

    do i = 1,9
        matAb(i,10) = matb(1,i)
    end do

    print "(a)", "matrix Ab is:"
    print "(10f8.1)", ( matAb(i,:),i=1,9 )

    !****************三角化****************!
	minval = matA(1,1)
	do i = 1,9
		do j = 1,9
			if (minval>matA(j,i)) then
			minval = matA(j,i)
			end if
    	end do
    end do



   	do j = 1,9   
   	   
 
    	!**************Exchange Column Pivot elemnet***************!
        do i = 1,9
            temp(i) = minval
         end do
         do n = j,9
   			temp(n) = matAb(n,j)
	   	 end do
		
    	 pivot = MaxLoc(temp)
        do i = 1,10
            Tri(i) = matAb(pivot(1),i)
        end do
        do i = 1,10
        	  matAb(pivot(1),i) = matAb(j,i)
        end do
        do i = 1,10
        	  matAb(j,i) = Tri(i)
        end do
        
        !**************Triangularization***************!
        if (j<9) then
            do n = j+1,9
                 do i = 1,10
                    matAb(n,i) = matAb(n,i) - matAb(j,i) *matAb(n,j)/matAb(j,j) 
                 end do 
              end do
        end if

        
        
   	end do
   	
  	print "(a)", "the triangularization matrix is:"
   	print "(10f8.1)", ( matAb(i,:),i=1,9 )
       

    
    !------------------求解x--------------
    matAb(9,10) = matAb(9,10)/matA(9,9)
    do i = 9-1,1,-1
        tmp = dot_product(matA(i,i+1:9),matAb(i+1:9,10))
        matAb(i,10) = matAb(i,10)-tmp
        matAb(i,10) = matAb(i,10)/matA(i,i)
    enddo

    do i = 1,9
        print "(a,i0,a,f8.3)",  "x", i , "=",matAb(i,10)
    end do

end program GE