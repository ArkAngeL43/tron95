program main
    implicit none
    REAL :: a4, b4, c4, root1, root2, d4, p4, q4 
    print*,'enter the 3 values of A, B, and C:'
    read*,a4, b4, c4
    d4=b4**2-4.0*a4*c4
    IF (d4>=0.00) THEN
        root1=-b4+sqrt(d4)
        root2=-b4-sqrt(d4)
    print*,'root one is=',root1
    print*,'root two is=',root2
    ELSE
        p4=-b4/(2.0*a4)
        q4=sqrt(abs(d4))
        print*,'1st root',p4,'+i',p4
        print*,'2st root',p4,'-i',q4
    END IF
end program main