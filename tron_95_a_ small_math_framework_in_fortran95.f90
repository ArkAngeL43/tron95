!
! Author   => ArkAngeL43
! Language => Fortran95 
! Program  => Main 
! Name     => TRON-95
!
! Description:
!   This is a simple program which will do the following 
!   :
!   : Calculate basic equations: addition, subtractionm division, multiplication
!   : Calculate the radius of a cylinder
!   : Check if x1, x2, x3, x4 and y1, y2, y3, y4 values match up to | rectangle, square, Rhombus, or a Parallelogram
!   : Calculate if certian points lie inside, outside, or not in a circle
!   : Calculate the biggest number out of a group of 3 numbers
!   : Calculate the area, circumference and volume of a circle
!   : Calculate and find any roots of a 2nd degree equation
!   : Calculate and convert temperature from celcius to F
!   : Locate the area of a triangle and the input point
!   :.....................................................  
!   :
!   : This programming lesson was taught by the original class of *******VS, NAME BLOCKED FOR AUTHOR SECURITY ( I dont feel like releasing my school :D ) 
!   : 
!   : Thank you for using this program, read the following for the setup and layout of the options
!   : options are layed out as following 
!   : 
!   : this menu operates out of [] with numbers on the inside if the square brackets work like [1] then it will choose option 1 if option 2 then [2] etc etc.....
!   : 
!   : Working along side of this it may be quite difficult with my current knowlege of Fortran95 and Fortran77 since i might here be mixing concepts of older fortran 
!   :
!   : And newer Fortran or aka Fortran95, this was a fun but long process and project to start, why not run a fortran for math since here soon i will be using it to calculate 
!   :
!   : All of the data in my current database, while being a fast and quick Language for forumla translation and mathematics it can be used for this type of thing or to automate
!   : 
!   : Data pillaging with regex in statements like the following written by the author of fortran_pcre
!   : 
!   :START__CODE__LINE
!   :=>  character(len=*), parameter :: pattern &
!   :=>     = "[ \s,]*(~@|[\[\]{}()'`~^@]|""(?:\\.|[^\\""])*""|;.*|[^ \s\[\]{}('""`,;)]*)"
!   :=> character(len=*), parameter :: subject = "(+ 2 (* 3 4))"
!   :=> integer, parameter :: subject_length = len_trim(subject)
!   :
!   :END__CODE__LINE
!
!
!
! Code decleration starts here
program main
    implicit none
    ! character decleration for the simple clear statement of the terminal 
    INTEGER :: options
	INTEGER :: i
	REAL :: a, b
	REAL :: addanswer, subtractanswer
	REAL :: multiplyanswer, divisionanswer
	REAL, dimension(100) :: p, q
    REAL :: pi
  	REAL :: radius
  	REAL :: height
  	REAL :: area
  	REAL :: volume
    ! VARIABLES to examin if a x and y cord is inside or outside of a circle
    REAL :: x1,x2,x3,x4,y1,y2,y3,y4,a2,b2,c2,d2,p2,q2
    ! verify a vertex
    REAL :: x5, x6, x7, x8, y5, y6, y7, y8, a3, b3, c3, d3, p3, q3
    ! verify if a cord is in or out of a circle
    REAL :: x, y, x0, y0, h, k, r, s
    ! Find bigger numbers
    INTEGER :: first_s, second_s, third_s, big
    ! Find circumference variables
    REAL :: area_2, circumference, vol_2, rad_2
    ! root finding of a quadratic equation
    REAL :: a4, b4, c4, root1, root2, d4, p4, q4 
    ! VARS for the temperature conversion
    REAL :: F_C, CEL
    ! PIE PARAM
    REAL, PARAMETER::pie=3.1416
    !
    !
    REAL :: area_9, s_9, sqrt, x11, y11, x22, y22, x33, y33, p_1, a_1, b_1, c_1


    CHARACTER CLEAR*6
    CLEAR=CHAR(27)//'[H'//CHAR(27)//'[J'
	pi = 3.1415927
    WRITE(*,*)CLEAR
    print *, " _______  ______  _____  __   _      ___   ___ "
    print *, "    |    |_____/ |     | | \  | ___ | . | |  _|"
    print *, "    |    |    \_ |_____| |  \_|     |_  | |_  |"
    print *, "                                    |___| |___|"
    print *, "Tron-95 simple math with fortran "
    print *, "_____________________________________________________"
    print *, "|++> Option usages: [number_option]                 |" 
    print *, "|++> Example      : 1 for option -> [1]             |" 
    print *, "|---------------------------------------------------|"
    print *, "|->_+++ LISTS:                                      |"
    print *, "|         List A:                                   |"
    print *, "|         rectangle, square, Rhombus, Parallelogram |"
    print *, "|---------------------------------------------------|..........."
    print *, "|[1]  => Calculate a basic equation                            |"
    print *, "|[2]  => Calculate the radius of a cylinder                    |"
    print *, "|[3]  => Check if values match up to list A                    |"
    print *, "|[4]  => Calculate if a point lies inside or out of a circle   |"
    print *, "|[5]  => Calculate the biggest number out of 3 integers        |"
    print *, "|[6]  => Calculate the area, circumference and vol of a circle |"
    print *, "|[7]  => Calculate and find any roots of a 2nd degree equation |"
    print *, "|[8]  => Calculate and convert temperature from celcius to F   |"
    print *, "|[9]  => Locate the area of a triangle and the input point     |"
    print *, ":..............................................................:"
    print *, ": Please Enter a command below -> OPTION -> "
    read(*,*) options
    !
    ! start if statements for main functions
    !
    IF (options == 1) THEN
        print *, ''	 
		print *, '[ | ] Please input the first integer  '
		read(*,*) a
		print *, ''
		print *, '[ | ] Please input the second integer '
		read(*,*) b
		!
		addanswer = a + b
    	subtractanswer = a - b
    	multiplyanswer = a * b
    	divisionanswer = a / b
		print *, "_________________________________________________________|"
        print *, "|Results : Addition, Subtraction, Multiplication, divsion|"
       	print *, "------------------------------------------------------------------------"
    	write(*,*) a, " + ", b, " = ", addAnswer
    	write(*,*) a, " - ", b, " = ", subtractanswer
    	write(*,*) a, " * ", b, " = ", multiplyanswer
    	write(*,*) a, " / ", b, " = ", divisionanswer
		open(unit = 2, file = "output.txt")
       	!the files unit is = to 2 so i would need to make the write unit the same as 2
		print *, ''//achar(27)//'[31m '//achar(27)//'[0m'
		write(2, *) a, " + ", b, " = ", addAnswer
		write(2, *) a, " - ", b, " = ", subtractanswer
		write(2, *) a, " * ", b, " = ", multiplyanswer
       	write(2, *) a, " / ", b, " = ", divisionanswer
		print *, "------------------------------------------------------------------------"
		close(2)
		PRINT*, "[ | ] Data written to a file |output.txt|  [ | ] "
    ! calculating the radius

    ELSE IF (options == 2) THEN
        print *, ''
        print *, '[ | ] Enter cylinder base radius:'
        read(*,*) radius
		print *, ''
        print *, '[ | ] Enter cylinder height:'
        read(*,*) height
        area = pi * radius**2.0
        volume = area * height
		print *, ''
		print *, ''
		print *, ''
		print *, '_______________________________________'
        print *, '[ = ] Cylinder radius      => ', radius
	    print *, '[ = ] Cylinder height      => ', height
        print *, '[ = ] Cylinder base area   => ', area
  		print *, '[ = ] Cylinder volume      =>  ', volume	
    ! 
    ELSE IF (options == 3) THEN
        print *, "Number format is as follows: x x"
        print *, "please space after each number, ex 1 3"
        print *, ""
        print *, ""
        print*, 'Enter the values of x1 and y1: '
        read*,x1,y1
        print*,'enter the value of x2 & y2'
        read*,x2,y2
        print*,'enter the value of x3 & y3'
        read*,x3,y3
        print*,'enter the value of x4 & y4'
        read*,x4,y4
        a2=sqrt((x1-x2)**2+(y1-y2)**2)
        b2=sqrt((x2-x3)**2+(y2-y3)**2)
        c2=sqrt((x3-x4)**2+(y3-y4)**2)
        d2=sqrt((x4-x1)**2+(y4-y1)**2)
        p2=sqrt((x1-x3)**2+(y1-y3)**2)
        q2=sqrt((x2-x4)**2+(y2-y4)**2)
        if(a2==c2 .and. b2==d2 .and. a2/=b2 .and. p2==q2 ) then
        print*,'[+] Numbers seem to add up to a rectangle'

        else if(a2==c2 .and. b2==d2 .and. a2/=b2 .and. p2/=q2 ) then
        print*,'[+] Variables seem to match up to a Parallelogram'

        else if( a2==b2 .and. b2==c2 .and. c2==d2 .and. d2==a2 .and. p2==q2 ) then
        print*,'[+] Variables match up to be a SQUARE'

        else if( a2==b2 .and. b2==c2 .and. c2==d2 .and. d2==a2 .and. p2/=q2 ) then
        print*,'this is Rhombus'
        else
            print*,'[+] This vertext does not seem to be classified as any of the following'
            print*,'|-> Square'
            print*,'|-> Parallelogram'
            print*,'|-> Rhombus'
            print*,'|-> rectangle'
            print*,'|=> FINAL -> Values dont match!'
            !VALUES OF THE FOLLOWING DONT MATCH LIST rectangle, Rhombus, Parallelogram, square -> -> >>>? 
            write(*,'(A)') ' ||-< || => || _> Numbers dont seem to be equal to the following list' // achar(13) // achar(10) // ''
        end if
        print*, '------------------------------------'
        write(*,*) " X variables -<1-2> >+>> ", x1, x2 
        write(*,*) " X variables -<3-4> >+>> ", x3, x4
        print*, ";;; Y SETS ;;;"
        write(*,*) " Y variables -<1-2> >+>> ", y1, y2
        write(*,*) " Y variables -<3-4> >+>> ", y3, y4 
    ELSE IF (options == 4) THEN
        print*, ""
        print*, ""
        print*, "-------------spacer----------"
        print*, 'Enter the value of x and y: '
        read*,x,y
        print*, 'Enter the value of h and k: '
        read*,h,k
        print*,'enter the value of x0,y0'
        read*,x0,y0
        r=sqrt((x-h)**2+(y-k)**2)
        s=sqrt((x0-h)**2+(y0-k)**2)
        if (r == s) then
            print*, '[+] The point lies ON the circle'
        elseif(r>s) then
            print*, '[+] The point lies INSIDE of the circle'
        else
            print*, '[+] The point lies OUTSIDE of the circle'
        end if
        print*, "|"
        print*, "|"
        write(*,*) "X  and Y  Variables used: X  -> ", x,  "Y  -> ", y
        write(*,*) "H  and K  Variables used: H  -> ", h,  "K  -> ", k
        write(*,*) "X0 and Y0 Variables used: X0 -> ", x0, "Y0 -> ", y0
    ELSE IF (options == 5) THEN
        print *, "[=~] Enter the value's of a, b, and c"
        read*, first_s, second_s, third_s
        IF (first_s > second_s) THEN
            IF (first_s > third_s) THEN
                big=first_s
            ELSE
                big=third_s
            END IF
        ELSE
            IF (second_s > third_s) THEN
                big=second_s
            ELSE
                big=third_s
            END IF
        END IF
        write(*,*) "[+] The biggest number out of -> ", first_s, " => ", second_s, " => ", third_s, " Is ->>>>>+++> ", big
    ELSE IF (options == 6) THEN
        print*,'--- Enter the radius --- '
        read*, rad_2
        area_2 = pie*rad_2**2
        vol_2=(4.0/3.0)*pie*rad_2**3
        circumference=2*pie*rad_2
        print*,"----------- DATA TABLE ------------"
        write(*,*) "[^] AREA         => ", area_2
        write(*,*) "[^] VOLUME       => ", vol_2
        write(*,*) "[^] CIRCUMFRENCE => ", circumference
    ELSE IF (options == 7) THEN
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
    ELSE IF (options == 8) THEN
        print*, "[+] Enter a Celcius value"
        print*, ""
        read*, CEL
        print*, "[!] Set -> ", CEL, " As a SETTING for TEMP CONV, line 273"
        F_C=1.8*CEL+32.0
        print*, "[+] Conversion: "
        print*, "[+]        Celcius    -> ", CEL
        print*, "[+]        FAHRENHEIT -> ", F_C
        ELSE IF (options == 9) THEN
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
    END IF
! Program written by ArkAngeL43
! ENDING NOTES:
!   Program was decent to make had a fun time
!
!What did you learn:
!          How to convert temps, calculate areas and radiuses etc
!
!
!Date of lesson:
!           00:00:00:0000
!
!Name: 
!  ArkAngeL43
!  RE43P3R
!
!
!Github project postage => https://github.com/ArkAngeL43/tron95
!
end program main
