!Program which will exam if a x and y cord is inside or outside of a circle
program read_circle
    implicit none
    real::x,y,x1,y1,h,k,r,s
    print*, 'Enter the value of x and y: '
    read*,x,y
    print*, 'Enter the value of h and k: '
    read*,h,k
    print*,'enter the value of x1,y1'
    read*,x1,y1
    r=sqrt((x-h)**2+(y-k)**2)
    s=sqrt((x1-h)**2+(y1-k)**2)
    if (r == s) then
        print*, '[+] The point lies ON the circle'
    elseif(r>s) then
        print*, '[+] The point lies INSIDE of the circle'
    else
        print*, '[+] The point lies OUTSIDE of the circle'
    end if
end program read_circle