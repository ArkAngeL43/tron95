!..............................................................
!: Program Written by ArkAngeL43                              :
!: Determin if a vertex is a rectangle, Parallelogram         :
!: square, or a Rhombus                                       :
!:                                                            :
!: New line Statement: WRITE(*,'(A,/,A')') 'new line active'  :
!: ALTERNATIVE: print '(*(A))', 'First line', NEW_LINE('a'),  :
!: ..NEW_LINE('a'), 'Second line'                             :
!:                                                            :
!: VARS: REAL: x1..4, y1..4, a..q                             :
!:                                                            :
!: Date: Thu 17 Feb 2022 03:28:20 AM                          :
!:............................................................:
!
!
! ERR: INPUT MUST BE REAL NUMBERS ONLY OR ERROR WILL OCCUR
!...................................................
!: Error termination. Backtrace:                   :
!: #0  0x7f87b138bbd0 in ???                       :
!: #1  0x7f87b138c685 in ???                       :
!: #2  0x7f87b138d25b in ???                       :
!: #3  0x7f87b15b9c98 in ???                       :
!: #4  0x7f87b15bb0c4 in ???                       :
!: #5  0x7f87b15bbcd1 in ???                       :
!: #6  0x56053bbe026a in ???                       :
!: #7  0x56053bbe0e6e in ???                       :
!: #8  0x7f87b11cdd09 in __libc_start_main         :
!:        at ../csu/libc-start.c:308               :
!: #9  0x56053bbe00e9 in ???                       :
!: #10  0xffffffffffffffff in ???                  :
!:.................................................:
!
! DEVELOPER NOTES
!
!
! clear statement but this isnt clear? its just \n\n....... not correct ANSI?
!print *, achar(27)//"[2J"
!print *, "[H[2J[3J" ! not correct code windows support only
! hmmm this one isnt working either
!
!
!WRITE(*,'(2f15.9)',advance='no') CALL SYSTEM('clear')
!
! ended up with 
!
! CHARACTER CLEAR*6
! CLEAR=CHAR(27)//'[H'//CHAR(27)//'[J'
! WRITE(*,*)CLEAR

program check_vertext
    implicit none
    real::x1,x2,x3,x4,y1,y2,y3,y4,a,b,c,d,p,q
    CHARACTER CLEAR*6
    CLEAR=CHAR(27)//'[H'//CHAR(27)//'[J'
    WRITE(*,*)CLEAR
    print *, " _______  ______  _____  __   _      ___   ___ "
    print *, "    |    |_____/ |     | | \  | ___ | . | |  _|"
    print *, "    |    |    \_ |_____| |  \_|     |_  | |_  |"
    print *, "                                    |___| |___|"
    print *, "Tron-95 simple math with fortran "
    print *, "            List options work 1 input at a time"
    print *, "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
    print *, "" 
    print *, "Number format is as follows: x x"
    print *, ""
    print*, 'Enter the values of x1 and y1: '
    read*,x1,y1
    print*,'enter the value of x2 & y2'
    read*,x2,y2
    print*,'enter the value of x3 & y3'
    read*,x3,y3
    print*,'enter the value of x4 & y4'
    read*,x4,y4
    a=sqrt((x1-x2)**2+(y1-y2)**2)
    b=sqrt((x2-x3)**2+(y2-y3)**2)
    c=sqrt((x3-x4)**2+(y3-y4)**2)
    d=sqrt((x4-x1)**2+(y4-y1)**2)
    p=sqrt((x1-x3)**2+(y1-y3)**2)
    q=sqrt((x2-x4)**2+(y2-y4)**2)
    if(a==c .and. b==d .and. a/=b .and. p==q ) then
    print*,'this is rectangle'

    else if(a==c .and. b==d .and. a/=b .and. p/=q ) then
    print*,'this is Parallelogram'

    else if( a==b .and. b==c .and. c==d .and. d==a .and. p==q ) then
    print*,'this is square'

    else if( a==b .and. b==c .and. c==d .and. d==a .and. p/=q ) then
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
end program check_vertext