program main
implicit none

    real*8 :: mat(5,5)
    integer :: i,j
    
    !read from screen
    print*, "input 5x5 mat:"
    do i = 1,5
        read*, mat(i,1:5)
    end do

    !output to data.txt
    open(1, file="data.txt",status='new')
    do i = 1,5
        write(1,*) mat(i,1:5)
    end do
    close(1)

    !open data.txt
    open(file="data.txt",unit=10,action="read")
    read(10,*)mat
    mat=transpose(mat)
    
    !print onto the screen
    print"(a)","data.txt elements:"
    print"(5f8.3)",(mat(i,:),i=1,5)

end program main
