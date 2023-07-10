!                   ******************
                    SUBROUTINE USER_SL
!                   ******************
!
     &(I , N, SL)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    USER PRESCRIBES THE FREE SURFACE ELEVATION FOR LEVEL IMPOSED
!+                LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF LIQUID BOUNDARY
!| N              |-->| GLOBAL NUMBER OF POINT
!|                |   | IN PARALLEL NUMBER IN THE ORIGINAL MESH
!| SL             |<->| FREE SURFACE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_USER_SL => USER_SL, EX_SL => SL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I,N
      DOUBLE PRECISION, INTENT(INOUT) :: SL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI,OMEGA,PERIODE,A
!
!-----------------------------------------------------------------------
!
!     DESCENTE DE LA COTE 0 A -0.55 EN 300 S
      PERIODE = 600.D0
      PI = 4.D0*ATAN(1.D0)
      OMEGA = 2.D0*PI/PERIODE
      A = 0.55D0/2.D0
      SL = A*(COS(OMEGA*AT)-1.D0)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
