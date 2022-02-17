program area_triangle
implicit none

REAL :: area_9, s_9, sqrt, x11, y11, x22, y22, x33, y33, p_1, a_1, b_1, c_1

print*,'enter the value of three sides of triangle x1,y1'
read*,x11,y11

print*,'enter the value of three sides of triangle x2,y2'
read*,x22,y22

print*,'enter the value of three sides of triangle x3,y3'
read*,x33,y33

a_1=sqrt((x22-x11)**2+(y22-y11)**2)
b_1=sqrt((x33-x22)**2+(y33-y22)**2)
c_1=sqrt((x11-x33)**2+(y11-y33)**2)

IF (a_1+b_1>c_1 .and. b_1+c_1>a_1 .and. c_1+a_1>b_1) THEN
    s_9=(a_1+b_1+c_1)/2
    area_9=sqrt(s_9*(s_9-a_1)*(s_9-b_1)*(s_9-c_1))
    print*,'[+] Area of the triangle         |-> ',area_9
    p_1=2*s_9
    print*,'[+] Perimiter of the triangle is |-> ',p_1
    print*,""
    print*,""
    print*,"---------------------------Variable TABLE------------------------------"
    print*,"[^] Variable X1 and Y2 -> | X variable => ", x11, " Y Variable -> ", y11
    print*,"[^] Variable X2 and Y2 -> | X variable => ", x22, " Y Variable -> ", y22
    print*,"[^] Variable X3 and Y3 -> | X variable => ", x33, " Y Variable -> ", y33
ELSE 
    print*,'[!] These points do not seem to validate a triangle'
    print*,""
    print*,""
    print*,"---------------------------Variable TABLE------------------------------"
    print*,"[^] Variable X1 and Y2 -> | X variable => ", x11, " Y Variable -> ", y11
    print*,"[^] Variable X2 and Y2 -> | X variable => ", x22, " Y Variable -> ", y22
    print*,"[^] Variable X3 and Y3 -> | X variable => ", x33, " Y Variable -> ", y33
END IF

end program area_triangle