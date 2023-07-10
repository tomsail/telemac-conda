!                   *****************
                    SUBROUTINE CALCUW
!                   *****************
!
     & ( UW, H, HW, TW, GRAV ,NPOIN)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE WAVE ORBITAL VELOCITY.
!
!history
!+        20/05/96
!+        V5P1
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| H              |-->| WATER DEPTH
!| HW             |-->| WAVE DEPTH
!| NPOIN          |-->| NUMBER OF POINTS
!| TW             |-->| WAVE PERIOD
!| UW             |<--| ORBITAL VELOCITY (WAVE)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: UW(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: TW(NPOIN),H(NPOIN), HW(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: GRAV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION   PI,DPI2
      DOUBLE PRECISION   POL, Y ,X
      INTEGER I
      INTRINSIC SQRT, SINH, ATAN
!
!-----------------------------------------------------------------------
!
      PI = 4.D0 * ATAN( 1.D0 )
      DPI2 = 4.D0 * PI * PI
!
!  SOLVES Y=X*TH(X) WITH Y=(2*PI/TW)**2*H/G AND X=(2*PI/L)*H
!  USING A POLYNOMIAL FUNCTION (HUNT METHOD - 9TH ORDER)
!
      DO I=1,NPOIN
        IF ( (TW(I) .GT. 0.D0).AND.(HW(I).GT.0.D0) ) THEN
          Y = DPI2 / GRAV * H(I) / (TW(I) * TW(I))
          POL = 1.D0 + Y * ( 0.66667D0 +
     &                Y * ( 0.35550D0 +
     &                Y * ( 0.16084D0 +
     &                Y * ( 0.06320D0 +
     &                Y * ( 0.02174D0 +
     &                Y * ( 0.00654D0 +
     &                Y * ( 0.00171D0 +
     &                Y * ( 0.00039D0 +
     &                Y * ( 0.00011D0 ) ))))))))
          X = SQRT( Y*Y + Y / POL )
!
          IF ( X .GT. 10.D0) THEN
            UW(I) = 0.D0
          ELSE
            UW(I) = PI / TW(I) * HW(I) / (SINH(X))
          ENDIF
        ELSE
          UW(I) = 0.D0
        ENDIF
      ENDDO
      RETURN
      END SUBROUTINE CALCUW
