
! ##############################################################################################################   

   module m_selinf
   
   implicit none
    
   !   SELAFIN FILE INFORMATION:
    character*80 :: TITLE
    character*32 :: NAMES(50)
    integer :: NBV(2),NBV1(2),IPARAM(10),NELEM,NPOIN,NDP1,EXTRA,n_names
    integer, allocatable, dimension(:,:) :: IKLE
    integer, allocatable, dimension(:) :: IPOBO
  !  real :: TIME0
  !  real, allocatable, dimension(:)               :: XIS,YIS
  !  real, allocatable, dimension(:,:)             :: UIS,VIS,HIS,BIS,EVOIS,BLIS,SCIS
  !  real, allocatable, dimension(:)               :: TIMES
    real, allocatable, dimension(:)   :: XI,YI
    real, allocatable, dimension(:,:) :: UI,VI,HI,BI,EVOI,BLI,SCI
    real, allocatable, dimension(:)   :: TIME
    real, allocatable, dimension(:)   :: pvalues 
    
    end module m_selinf
    
! ##############################################################################################################   

   program sisvali

!   ################################################################################################################

! Purpose:

! Read in TELEMAC2D type SELAFIN format file and perform avrious checks inc L2 error computations

!   compile with:
!   gfortran -o sisvali sisvaliKD09.f90 -fconvert=big-endian -frecord-marker=4 -O3 -g


!   NOTE:
! For speed I've just assumed that the format of the selafin file is such that it contains
! the following output variables:
! H,U,V,B,E,QSBL

! 

!   ################################################################################################################

!   AUTHOR: Dr. D. M. KELLY (HRW)
!   MODIFIED: 28nd January 2013
!   SUBROUTINES CALLED: 

    use m_selinf
  
    implicit none
    
    integer :: ierr,i,ii,j,NDP,NIP,imesh,nmesh,elementcount,nodecount
    integer :: t, naly
    integer, allocatable, dimension(:,:) :: BOUND
    integer ::  md,nxi,nyi,nzi,IER

    character*100 ::  fname
    character*500 ::  junk
 
    integer :: inr,jnr,TIM,TIMF,TIMW,jnumb
    real, allocatable, dimension(:,:) :: DEPV

    integer :: npp, dvn, spt
    real :: min_x, max_x, ypv, dx, hdx, wA, wB, wC, areaT
    real :: num, denom, sumnum, sumdenom, delvol
    real, allocatable, dimension(:)   :: susmass, xpp, valu, valua1,valua2, vol
    real, allocatable, dimension(:)   :: areaN, L2errS, L2errT
    real, allocatable, dimension(:)   :: xan
    real, allocatable, dimension(:,:) :: valan

    logical :: INPOLY
   
   
!   Use implicit FORTRAN 90 routine to get the file stem direct from the command line:
!    call getarg ( 1, fname )

    fname = "sis_kd09" ! tmp hard coded
    
                                                   
!   Open TELEMAC-2D Selafin file:
!   ============================== SELAFIN FILE READ =======================================================
    
    open (convert='big_endian',unit=100,file=trim(fname)//'.slf',status='old',form='unformatted')
   
    read (100,ERR=11) TITLE(1:80)
    read (100,ERR=11) NBV(1), NBV(2)
    do i=1,(NBV(1)+NBV(2))
      read (100,ERR=11) NAMES(i)(1:32)
    end do
    read (100,ERR=11) (IPARAM(i),i=1,10)
    read (100,ERR=11) NELEM, NPOIN, NDP, EXTRA
    print *, 'NDP ', NDP, ' ELEMENTS ',NELEM,' NODES ', NPOIN
    allocate(IKLE(NDP,NELEM),IPOBO(NPOIN))
!    allocate(XIS(NPOIN),YIS(NPOIN))
    allocate(XI(NPOIN),YI(NPOIN))
    TIM=500

    ! SP arrays
    allocate(UI(TIM,NPOIN),VI(TIM,NPOIN),HI(TIM,NPOIN),BI(TIM,NPOIN))
    allocate(EVOI(TIM,NPOIN),BLI(TIM,NPOIN))
    allocate(SCI(TIM,NPOIN),TIME(TIM))
    
    read (100,ERR=11) (IKLE(1:3,i),i=1,NELEM)
    read (100,ERR=11) (IPOBO(i),i=1,NPOIN)
    read (100,ERR=11) (XI(i),i=1,NPOIN)
    read (100,ERR=11) (YI(i),i=1,NPOIN)
    TIM=0
!   read data at this time: 
10  TIM=TIM+1
!    print *, "Time Record Number: ", TIM
    read (100,END=12,ERR=11) TIME(TIM)
    read (100,END=12,ERR=11) (UI(TIM,i),i=1,NPOIN)   ! Velcoity-x
    read (100,END=12,ERR=11) (VI(TIM,i),i=1,NPOIN)   ! Velocity-y
    read (100,END=12,ERR=11) (HI(TIM,i),i=1,NPOIN)   ! Depth
    read (100,END=12,ERR=11) (BI(TIM,i),i=1,NPOIN)   ! Bottom
    read (100,END=12,ERR=11) (EVOI(TIM,i),i=1,NPOIN) ! Evolution
    read (100,END=12,ERR=11) (BLI(TIM,i),i=1,NPOIN)  ! Bed load
    GOTO 10
    
11  stop 'Error in selafin input file (produced by SISYPHE) - TERMINATING.'
   
12  print *, 'Selafin file read in for validation there were', TIM-1, 'times.'
    TIMF=TIM-1
    close(100)
    
   
!   ============================== END SELAFIN FILE READ ====================================================

!   Get size of the data  file: 
!   Note there are 9 header lines in ggensimp.out and it contains a single snapshot of the dependent variables in time
    open (unit=100,file='ggensimp.out',status='old')
    i=0
    ierr = 1
    do
    i=i+1
    read(100,*,END=14,ERR=13) junk
    end do

13 stop 'Error in ggensimp.out file - TERMINATING.'

14  npp = i-1
    close(100)
    
    npp = npp-9             ! account for header lines

    allocate (xan(npp))     ! Cross sectional spacing
    allocate (valan(6,npp)) ! There are 6 dependent variables output
    
!   Read in the data    
    open (unit=10,file='ggensimp.out',status='old')
    ii = 0
    do i=1,npp+9
      if (i < 10) then
      read(10,*) junk
      else  
      ii = ii+1
      read(10,*) valan(1,ii), xan(ii), valan(4,ii), valan(5,ii), valan(6,ii),  valan(2,ii), valan(3,ii)
      end if
    end do
    
    close(10)

    
!    ============================= DATA ON CROSS-SECTIONAL PROFILE ===============================================  
    
    allocate(vol(TIMF),pvalues(npp),L2errS(2))
    
    naly   = 100

    allocate(xpp(naly),valu(naly),valua1(naly),valua2(naly))

    min_x  = minval(xan)
    xan    = xan + 5.D0      ! move all xvals up so they initial dam location is at 5.0
    max_x  = maxval(xan)
    min_x  = minval(xan)

    xpp(1)  = min_x + 1E-6
    valu(1) = 0.D0

    print *, "minx", min_x, "maxx", max_x

    dx     = abs(max_x-min_x)/real(naly)  ! profile line spacing dx
    hdx    = 0.5D0*dx

    do i=2,naly
    xpp(i) = xpp(i-1) + dx
    end do

    do i=1,naly
    call locate(xan(1:npp),npp,xpp(i),ii)
    !valua(i) = valan(3,ii)+ (valan(3,ii+1)-valan(3,ii))*((xpp(i)-xan(ii))/(xan(ii+1)-xan(ii)))

    !call polint(xan(ii:ii+3),valan(1,ii:ii+3),4,xpp(i),valua(i)) ! Depth values
    !call polint(xan(ii:ii+2),valan(3,ii:ii+2),3,xpp(i),valua(i)) ! Bed values

!   Rational Function Interpolation:
    call ratint(xan(ii:ii+2),valan(1,ii:ii+2),3,xpp(i),valua1(i)) ! Depth values
    call ratint(xan(ii:ii+2),valan(3,ii:ii+2),3,xpp(i),valua2(i)) ! Bed values
    end do

    ypv    = 0.05             ! y value for the profile

 !   ============================= PERFORM VALIDATION OPERATIONS ON DATA ===============================================  

    do ii=1,2
    
 !  This operation is for the KD09 test case:   

      if (ii==1) then

        !open (unit=101,file='kd09dep.xy',status='replace')
	open (unit=101,file='kd09dep.csv',status='replace')
        call profvals(TIMF,naly,xpp,ypv,valu,"H")       ! Get Depth profile values
        
    write (101,*)'# Depth profile values'
    write (101,*)'Distance, Depth Level, Depth Level'
    write (101,*)'m, m, m'
    
        do i =1,naly
        !write(101,*) xpp(i), valu(i), valua1(i)
	write (101, '(1x, F0.6, 2(",", F0.6))') xpp(i), valu(i), valua1(i)
        end do 
        close(101)

        call L2error(1,naly,TIMF,valua1,valu,L2errS(1))     ! L2 error norms for the Depth
      
        else if (ii==2) then
        !open (unit=101,file='kd09bed.xy',status='replace')
	open (unit=101,file='kd09bed.csv',status='replace')
        call profvals(TIMF,naly,xpp,ypv,valu,"B")            ! Get bed profile values
	
	write (101,*)'# Bed profile values'
    write (101,*)'Distance, Bed Level, Bed Level'
    write (101,*)'m, m, m'
    
        do i =1,naly
       ! write(101,*) xpp(i), valu(i), valua2(i)
	write (101, '(1x, F0.6, 2(",", F0.6))') xpp(i), valu(i), valua2(i)
        end do 
        close(101)

        call bedcon(TIMF,naly,hdx,valu,vol(TIMF))           ! Check sediement is conserved in the bed     

        call L2error(1,naly,TIMF,valua2,valu,L2errS(2))     ! L2 error norms for the Bed

        print *, naly, hdx, vol(TIMF), L2errS(1)

        open(UNIT=101,file="Mass_Con.csv")
        write(101,*) "Mass Con Error"
        write(101,*) vol(TIMF)
        close(101)

        open(UNIT=101,file="Errors.csv")
        write(101,*) "L2 Depth, L2 Bed" 
        write(101,*) " -, - " 
        write(101,'(F6.4 ,A, F6.4)') L2errS(1),',', L2errS(2)
        close(101)

        end if

 !     end do

    end do


!   Call external R program to do the plotting:
!   Now done using Python
!    call system("R CMD BATCH plotkd09.R")

!   Clear memory:
    deallocate(XI,YI,IKLE,UI,VI,HI,BI)
    deallocate(EVOI,BLI)
    deallocate(vol,pvalues,L2errS,valua1,valua2)

    end 
 
! #####################################################################################################################################
 
   subroutine brierskill(lim1,lim2,dvn,valu,valan,valanin,bss)
    
   implicit none
    
   integer, intent(in)           :: lim1, lim2, dvn
   real, intent(in)  :: valu(lim2), valan(dvn,lim2), valanin(dvn,lim2)
   real, intent(out) :: bss(lim1)
    
   integer                       :: i,ii
   real              :: delMSEmod, delMSEbed,  MSEmod,  MSEbed

   do ii=1,lim1
     delMSEmod = 0.d0
     delMSEbed = 0.D0
   
     do i=1,lim2
     delMSEmod = (valu(i)-valan(dvn,i))*(valu(i)-valan(dvn,i))
     delMSEbed = (valan(dvn,i)-valanin(dvn,i))*(valan(dvn,i)-valanin(dvn,i))  
     MSEmod    = MSEmod + delMSEmod
     MSEbed    = MSEbed + delMSEbed
     end do   
     
     bss(ii)   = 1.D0 - MSEmod/MSEbed
     
   end do
   
   return
   end   
    
 subroutine bedcon(ntims,npp,hdx,values,vol)
!   ============================= DO VALIDATION CHECKS ===============================================
!   == 1. CONSERVATION OF SEDIMENT IN BED FOR EACH TIME AND INTEGRATE BED AREA IN TRANSECT USE COMPOSITE TRAPEZIUM RULE FOR SIMPLICITY

   implicit none

   integer, intent(in)           :: npp, ntims
   real, intent(in)  :: hdx, values(npp)
   real, intent(out) :: vol

   integer                       :: i
   real              :: delvol
 
  vol    = 0.D0
  delvol = 0.D0
    
  do i=1,(npp-1)
  delvol    = hdx*(values(i)+values(i+1))
  vol       = vol + delvol
  end do
    
  return

  end
 
! ###############################################################################################
 
   subroutine profvals(spt,npp,xpp,ypv,valu,var) 
    
   use m_selinf 
    
   implicit none 
    
   integer, intent(in)           :: npp, spt 
   character*1, intent(in)       :: var
   real, intent(in)              :: xpp(npp), ypv
   real, intent(out)             :: valu(npp)
   integer                       :: i, ii
   real                          :: areaT, wA, wB, wC
   logical                       :: INPOLY

   valu = 0.0
   
! Get values at profile points:
    do ii=1,npp
      i = 0
      do 
        i = i + 1

        if (INPOLY(xpp(ii),ypv,XI(IKLE(1:3,i)),YI(IKLE(1:3,i)),3)) then
        ! Compute weights for interpolation using the "shoelace" formula to compute triangle areas:

        areaT = 0.5*abs(   XI(IKLE(1,i))*YI(IKLE(2,i)) + XI(IKLE(2,i))*YI(IKLE(3,i)) + &
                           XI(IKLE(3,i))*YI(IKLE(1,i)) - XI(IKLE(2,i))*YI(IKLE(1,i)) - &
                           XI(IKLE(3,i))*YI(IKLE(2,i)) - XI(IKLE(1,i))*YI(IKLE(3,i))   )

        wA    = 0.5*abs(   xpp(ii)*YI(IKLE(2,i)) + XI(IKLE(2,i))*YI(IKLE(3,i)) + &
                           XI(IKLE(3,i))*ypv     - XI(IKLE(2,i))*ypv           - &
                           XI(IKLE(3,i))*YI(IKLE(2,i)) - xpp(ii)*YI(IKLE(3,i))   ) / areaT

        wB     = 0.5*abs(  XI(IKLE(1,i))*ypv + xpp(ii)*YI(IKLE(3,i))           + &
                           XI(IKLE(3,i))*YI(IKLE(1,i)) - xpp(ii)*YI(IKLE(1,i)) - &
                           XI(IKLE(3,i))*ypv - XI(IKLE(1,i))*YI(IKLE(3,i))       ) / areaT

        wC     = 0.5*abs(  XI(IKLE(1,i))*YI(IKLE(2,i)) + XI(IKLE(2,i))*ypv     + &
                           xpp(ii)*YI(IKLE(1,i)) - XI(IKLE(2,i))*YI(IKLE(1,i)) - &
                           xpp(ii)*YI(IKLE(2,i)) - XI(IKLE(1,i))*ypv             ) / areaT

        exit
        else if (i==NELEM) then
        print *, 'profile points ', xpp(ii), ypv , ' NOT on mesh - TERMINATING'; STOP
        end if

      end do
          
   ! Compute value of the variable at that profile point for a specific time:
   if      (var=="B") then
   valu(ii) = wA*BI(spt,IKLE(1,i)) + wB*BI(spt,IKLE(2,i)) + wC*BI(spt,IKLE(3,i))
   else if (var=="H") then
   valu(ii) = wA*HI(spt,IKLE(1,i)) + wB*HI(spt,IKLE(2,i)) + wC*HI(spt,IKLE(3,i))
   end if
      
   end do
     
    return
    end
! ##############################################################################################################   
 

   subroutine L2error(lim1,lim2,dvn,valu,valan,L2err)
    
   implicit none
    
   integer, intent(in)           :: lim1, lim2, dvn
   real, intent(in)              :: valu(lim2), valan(lim2)
   real, intent(out)             :: L2err(lim1)
    
    integer                      :: i,ii 
    real                         :: sumnum, num, sumdenom, denom
    
!    == 4. COMPUTE L2 ERROR NORMS (SPACE)
 !    allocate (L2errS(TIMF))
 !    dvn is Dependent Variable Number: 1. Depth, 2. Velocity (x-component), 3. Bed
     
     do ii=1,lim1
       sumnum   = 0.D0
       sumdenom = 0.D0
       do i=1,lim2 
         num      = (valu(i) - valan(i)) * (valu(i)-valan(i))
         denom    = valan(i) * valan(i)
         sumnum   = sumnum + num; sumdenom = sumdenom + denom
       end do
     L2err(ii) = sumnum/sumdenom     
     end do 
     
     return
     
     end

! ##########################################################################################
    
   subroutine tricentroid(x1,y1,x2,y2,x3,y3,xc,yc)
    
!  Subroutine to compute the centroid of a triangle
    
   implicit none
    
   real, intent(in)  :: x1, y1, x2, y2, x3, y3
   real, intent(out) :: xc, yc
    
   xc = (x1+x2+x3)/3.D0; yc = (y1+y2+y3)/3.D0
     
   return
     
   end
    
 ! ##########################################################################################
    

! ================================= THIRD PARTY ROUTINES BELOW HERE ==========================================================    
    

LOGICAL FUNCTION INPOLY( X , Y , XSOM , YSOM , NSOM )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INDICATES IF A POINT WITH COORDINATES X AND Y IS
!+                IN A POLYGON WITH GIVEN VERTICES.
!code
!+    PRINCIPLE: TAKES HALF A LINE STARTING FROM THE POINT AND COUNTS THE
!+               NUMBER OF TIMES IT INTERSECTS WITH THE POLYGON
!+
!+    ALSO WORKS IF THE POLYGON IS NOT CONVEX
!+
!+    INTERSECTIONS ARE IDENTIFIED USING THE LINES PARAMETRIC EQUATIONS :
!+
!+
!+    X + A * MU = XDEP + (XARR-XDEP) * LAMBDA
!+    Y + B * MU = YDEP + (YARR-YDEP) * LAMBDA
!+
!+    THE HALF-LINE IS CHARACTERISED BY THE CHOICE OF A AND B, AND THE
!+    SIGN OF MU. THERE IS INTERSECTION IF MU > 0 AND 0 < LAMBDA < 1
!
!warning  THE POLYGON VERTICES MUST BE DISTINCT (NO DUPLICATE NODES)
!
!history  E. DAVID (LHF)
!+
!+
!+   ORIGINAL IDEA AND CODE
!
!history  J.-M. HERVOUET (LNH)
!+        18/06/96
!+        V5P2
!+
!
!history  JEAN-PHILIPPE RENAUD (CSN BRISTOL)
!+        27/07/99
!+
!+   CORRECTION FOR A SPECIAL CASE
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NSOM           |-->| NUMBER OF APICES OF POLYGON
!| X              |-->| ABSCISSA OF POINT
!| Y              |-->| ORDINATE OF POINT
!| XSOM           |-->| ABSCISSAE OF POLYGON APICES
!| YSOM           |-->| ORDINATES OF POLYGON APICES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NSOM
      real, INTENT(IN) :: X,Y
      real, INTENT(IN) :: XSOM(NSOM),YSOM(NSOM)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,NSECT
!
      real A,B,ANGLE,XDEP,YDEP,XARR,YARR,DET,MU,LAMBDA,EPS
!
      INTRINSIC COS,SIN,ABS,MOD
!
!-----------------------------------------------------------------------
!
      EPS = 1.D-9
      ANGLE = -1.D0
!
! CHOOSES A AND B SUCH AS TO AVOID SPECIAL CASES
!
1000  CONTINUE
      ANGLE = ANGLE + 1.D0
      IF(ANGLE.GT.360.D0) THEN
!       SPECIAL CASE OF A POINT ON THE CONTOUR
        INPOLY=.TRUE.
        RETURN
      ENDIF
      A = COS(ANGLE*3.141592653D0/180.D0)
      B = SIN(ANGLE*3.141592653D0/180.D0)
      NSECT=0
!
! LOOP ON ALL THE SEGMENTS OF THE POLYGON
!
      DO 10 N=1,NSOM
!
!     DEP : 1ST POINT OF THE SEGMENT    ARR : 2ND POINT
!
      XDEP=XSOM(N)
      YDEP=YSOM(N)
      IF(N.LT.NSOM) THEN
        XARR=XSOM(N+1)
        YARR=YSOM(N+1)
      ELSE
        XARR=XSOM(1)
        YARR=YSOM(1)
      ENDIF
!
!     CASE WHERE TWO SUCCESSIVE POINTS ARE DUPLICATES
!
      IF(ABS(XDEP-XARR)+ABS(YDEP-YARR).LT.EPS) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'INPOLY : POINTS CONFONDUS DANS LE POLYGONE'
          WRITE(LU,*) 'AU POINT DE COORDONNEES : ',XDEP,'  ET  ',YDEP
          WRITE(LU,*) 'DE NUMERO ',N
          IF(N.EQ.NSOM) THEN
            WRITE(LU,*) 'LE DERNIER POINT NE DOIT PAS ETRE EGAL AU'
            WRITE(LU,*) 'PREMIER (POUR UN TRIANGLE, PAR EXEMPLE,'
            WRITE(LU,*) 'IL FAUT DONNER TROIS POINTS ET NON QUATRE)'
          ENDIF
          WRITE(LU,*) 'INPOLY EST PROBABLEMENT CALLED BY FILPOL'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'INPOLY: SUPERIMPOSED POINTS IN THE POLYGON'
          WRITE(LU,*) 'AT POINT: ',XDEP,'  AND  ',YDEP,' WITH NUMBER ',N
          IF(N.EQ.NSOM) THEN
            WRITE(LU,*) 'THE LAST POINT MUST NOT BE EQUAL TO THE FIRST'
            WRITE(LU,*) 'FOR EXAMPLE, GIVE 3 POINTS FOR A TRIANGLE'
          ENDIF
          WRITE(LU,*) 'INPOLY IS PROBABLY CALLED BY FILPOL'
        ENDIF
        WRITE(LU,*) ' '
        WRITE(LU,*) ' '
        STOP
      ENDIF
!
!     CASE WHERE THE POINT IS A VERTEX
!     (THE GENERAL ALGORITHM WOULD DEFINE IT AS EXTERNAL WITH 2 INTERSECTIONS)
!
      IF(ABS(X-XDEP).LE.EPS.AND.ABS(Y-YDEP).LE.EPS) THEN
        NSECT=1
        GO TO 2000
      ENDIF
!
!     DETERMINANT OF THE KRAMER SYSTEM
!
      DET = A*(YDEP-YARR)-B*(XDEP-XARR)
      IF(ABS(DET).LT.EPS) GO TO 1000
!
      MU     = ( (XDEP-X)*(YDEP-YARR)-(YDEP-Y)*(XDEP-XARR) ) / DET
      LAMBDA = (    A    *(YDEP-Y   )-    B   *(XDEP-X   ) ) / DET
!
!-------------------------------------------------------
! JP RENAUD (CSN BRISTOL) CORRECTION TO AVOID THAT THE INTERSECTION
! POINT BE ONE OF THE VRTICES
!
! IF THE INTERSECTION POINT IS A VERTEX, INCREASES THE ANGLE
! OTHERWISE THE POINT WOULD BE COUNTED TWICE INSTEAD OF JUST ONCE
!
      IF ((ABS(X+A*MU-XDEP).LE.EPS.AND.ABS(Y+B*MU-YDEP).LE.EPS).OR. &
      (ABS(X+A*MU-XARR).LE.EPS.AND.ABS(Y+B*MU-YARR).LE.EPS)) GOTO 1000
!
! END OF JP RENAUD CORRECTION
!-------------------------------------------------------
!
      IF(MU.GE.-EPS.AND.LAMBDA.GE.-EPS.AND.LAMBDA.LE.1.D0+EPS) THEN
        NSECT=NSECT+1
      ENDIF
!
10    CONTINUE
!
2000  CONTINUE
!
      INPOLY=(MOD(NSECT,2).EQ.1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

! ######################################################################################

 SUBROUTINE ratint(xa,ya,n,x,y)

      implicit none

      integer, intent(in)           :: n
      real, intent(in)              :: x, xa(n), ya(n)
      real, intent(out)             :: y
      
      integer                       :: i,m,ns
      real, parameter   :: TINY = 1E-25
      real              :: dy, dd,h,hh,t,w,c(n),d(n)

      ns=1
      hh=abs(x-xa(1))
      do i=1,n
        h=abs(x-xa(i))
        if (h==0.D0)then
          y=ya(i)
          dy=0.D0
          return
        else if (h<hh) then
          ns=i
          hh=h
        endif
        c(i)=ya(i)
        d(i)=ya(i)+TINY
       end do
      y=ya(ns)
      ns=ns-1
      do m=1,n-1
        do i=1,n-m
          w=c(i+1)-d(i)
          h=xa(i+m)-x
          t=(xa(i)-x)*d(i)/h
          dd=t-c(i+1)
          if(dd==0.D0) stop 'failure in ratint'
          dd=w/dd
          d(i)=c(i+1)*dd
          c(i)=t*dd
         end do
        if (2*ns.lt.n-m)then
          dy=c(ns+1)
        else
          ns=ns-1
        endif
        y=y+dy
      end do
      return
      END

! #########################################################################################

subroutine polint(xa,ya,n,x,y)

      implicit none

      integer, intent(in) :: n
      real, intent(in)    :: x,xa(n),ya(n)
      real, intent(out)   :: y

      integer             :: i,m,ns
      real                :: den,dif,dift,ho,hp,w,c(n),d(n),dy
      ns=1
      dif=abs(x-xa(1))
      do i=1,n
        dift=abs(x-xa(i))
        if (dift<dif) then
          ns=i
          dif=dift
        end if
        c(i)=ya(i)
        d(i)=ya(i)
      end do
      y=ya(ns)
      ns=ns-1
     
      do m=1,n-1
        do i=1,n-m
          ho=xa(i)-x
          hp=xa(i+m)-x
          w=c(i+1)-d(i)
          den=ho-hp
          if(den==0.0) stop 'failure in polint'
          den=w/den
          d(i)=hp*den
          c(i)=ho*den
         end do
        if (2*ns<n-m)then
          dy=c(ns+1)
        else
          dy=d(ns)
          ns=ns-1
        endif
        y=y+dy
      end do

      return
      END

!! ###################################################################

! finds the index, j, of x in array xx(n). assuming xx is sorted, x lies
! between xx(j) and xx(j+1) -- from NUMERICAL RECIPES IN FORTRAN 77  
      subroutine locate(xx,n,x,j)
      integer, intent(in) :: n
      real, intent(in) :: xx(n), x
      integer, intent(out) :: j
      integer :: jl, ju, jm
      jl=0
      ju=n+1
      do while(ju-jl>1)
         jm=(ju+jl)/2
         if((xx(n)>xx(1)).eqv.(x>xx(jm)))then
            jl=jm
         else
            ju=jm
         end if
      end do
      j=jl
      end subroutine locate


