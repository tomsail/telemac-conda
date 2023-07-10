!                   **********************
                    SUBROUTINE USER_DEBSCE
!                   **********************
!
     &(TIME, I, DISCE, DEBSCE)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    USER GIVES THE PRESCRIBED DISCHARGE OF EVERY SOURCE POINT.
!+
!+            VARIATIONS WRT TIME AND SPACE MAY BE IMPLEMENTED.
!
!note     T2DVEF IS THE SOURCES FILE IN TELEMAC-2D
!
!history  J-M HERVOUET (LNHE)
!+        03/04/2008
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DISCE          |-->| ARRAY OF DISCHARGES OF SOURCES.
!|                |   | READ IN THE PARAMETER FILE.
!|                |   | NAME OF DISCE IS DSCE IN TELEMAC-2D.
!| I              |-->| NUMBER OF THE SOURCE OR OF THE SOURCE REGION
!| TIME           |-->| TIME
!| DEBSCE         |-->| DISCHARGE OF SOURCE POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME,DISCE(*)
      INTEGER         , INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(INOUT) :: DEBSCE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!       GLOBAL NUMBER OF SOURCE I IS ISCE(I) IN TELEMAC-2D
!
        IF(TIME.GE.1800.D0.AND.TIME.LE.3600.D0) THEN
          DEBSCE = DISCE(I)
        ELSEIF(TIME.GE.4800.D0.AND.TIME.LE.7200.D0) THEN
          DEBSCE = DISCE(I)
        ELSE
          DEBSCE = 0.D0
        ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
