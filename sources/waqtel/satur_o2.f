!                   *******************
                    SUBROUTINE SATUR_O2
!                   *******************
!
     &(SATO2,FORMCS,WATTEMP,EPS)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES THE CONCENTRATION OF O2 SATURATION OF WATER
!
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+    First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EPS            |-->| TO AVOID DIVISION BY 0
!| FORMCS         |-->| WHICH FORMULAE FOR SATUR_O2
!| SATO2          |<--| CONCENTRATION OF O2 SATURATION OF WATER
!| WATTEMP        |-->| TEMPERATURE OF WATER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_WAQTEL, EX_SATUR_O2 => SATUR_O2
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: FORMCS
      DOUBLE PRECISION, INTENT(IN)    :: WATTEMP,EPS
      DOUBLE PRECISION, INTENT(INOUT) :: SATO2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC ABS
!
!-----------------------------------------------------------------------
!
      IF(FORMCS.NE.0) THEN
        IF(FORMCS.EQ.1) THEN
          SATO2 = 14.652D0 - 0.41022D0  * WATTEMP
     &                     + 0.007991D0 * WATTEMP**2
     &                     - 7.7774D-5  * WATTEMP**3
        ELSEIF(FORMCS.EQ.2)THEN
          IF(ABS(31.6D0+WATTEMP).GT.EPS)THEN
            SATO2 = 468.D0/(31.6D0+WATTEMP)
          ELSE
            SATO2 = 468.D0/EPS
          ENDIF
        ELSE
          WRITE(LU,101) FORMCS
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!     ERROR MESSAGES
!
101   FORMAT(1X,'CS FORMULA :',I3,/,1X, 'NOT AVAILABLE')
!
!-----------------------------------------------------------------------
!
      RETURN
      END

