!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Diff_Time                      !********************************************
!***                                              ********************************************
!***                                              ********************************************
!
!cgl  &(TDEB,TFIN)
     &(TDEB,TFIN,NDAY,HOURS,MINUTES,SECONDS)! by Boris 2014/12/12
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRINTS THE DURATION BETWEEN TWO TIMES GIVEN BY THE
!+                CALL TO DATE_AND_TIME.
!
!history  J-M HERVOUET (LNHE)
!+        20/09/2001
!+        V5P3
!+
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
!| TDEB           |-->| TIME OF BEGINNING
!| TFIN           |-->| TIME OF END
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: TDEB(8),TFIN(8)
      INTEGER, INTENT(OUT):: NDAY, HOURS, MINUTES, SECONDS  ! by Boris 2014/12/12
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#ifndef NESTOR_INTERFACES
!
      !------- local variables ---------------
!
      INTEGER YEAR,MONTH,DAY,     J1,J2  ! by Boris 2014/12/12
      INTEGER GREG,Y,M
      DOUBLE PRECISION TD,TF,TT
!
      INTRINSIC INT
!
      PARAMETER (GREG=15+31*(10+12*1582))
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE NUMBER OF DAYS (USES THE JULIAN DAY SYSTEM)
!
      YEAR=TDEB(1)
      MONTH=TDEB(2)
      DAY=TDEB(3)
      IF(MONTH >  2) THEN
       Y=YEAR
       M=MONTH+1
      ELSE
       Y=YEAR-1
       M=MONTH+13
      ENDIF
!
      J1=INT(365.25D0*Y)+INT(30.6001D0*M)+DAY+1720995
      IF(DAY+31*(MONTH+12*YEAR) >= GREG) THEN
        J1=J1+2-INT(0.01D0*Y)+INT(0.25D0*INT(0.01D0*Y))
      ENDIF
!
      YEAR=TFIN(1)
      MONTH=TFIN(2)
      DAY=TFIN(3)
      IF(MONTH >  2) THEN
       Y=YEAR
       M=MONTH+1
      ELSE
       Y=YEAR-1
       M=MONTH+13
      ENDIF
!
      J2=INT(365.25D0*Y)+INT(30.6001D0*M)+DAY+1720995
      IF(DAY+31*(MONTH+12*YEAR) >= GREG) THEN
        J2=J2+2-INT(0.01D0*Y)+INT(0.25D0*INT(0.01D0*Y))
      ENDIF
!
      NDAY=J2-J1
!
      TD = 3600.D0*TDEB(5) + 60.D0*TDEB(6) + TDEB(7)
      TF = 3600.D0*TFIN(5) + 60.D0*TFIN(6) + TFIN(7)
      IF(TF <  TD) THEN
        NDAY=NDAY-1
        TF=TF+86400.D0
      ENDIF
      TT=TF-TD
      HOURS=INT(TT/3600.D0)
      TT=TT-3600.D0*HOURS
      MINUTES=INT(TT/60.D0)
      TT=TT-60.D0*MINUTES
!
      SECONDS = INT(TT)  ! by Boris 2014/12/12
!
!---------------------------------------------------------------------
!                                                                      ! debug
!      WRITE(6,*)   'ELAPSE TIME : '                                   ! debug
!      IF(NDAY >  0) THEN                                              ! debug
!        WRITE(6,*) '                  ',NDAY,' DAYS'                  ! debug
!      ENDIF                                                           ! debug
!      IF(HOURS >  0) THEN                                             ! debug
!        WRITE(6,*) '                  ',HOURS,' HOURS'                ! debug
!      ENDIF                                                           ! debug
!      IF(MINUTES >  0) THEN                                           ! debug
!        WRITE(6,*) '                  ',MINUTES,' MINUTES'            ! debug
!      ENDIF                                                           ! debug
!      WRITE(6,*)   '                  ',INT(TT),' SECONDS'            ! debug
!                                                                      ! debug
!---------------------------------------------------------------------
!
      RETURN
!***                                               ********************************************
!***                                               ********************************************
#endif
      END SUBROUTINE Diff_Time                    !********************************************
!***                                               ********************************************
!***                                               ********************************************
!**********************************************************************************************
!**********************************************************************************************