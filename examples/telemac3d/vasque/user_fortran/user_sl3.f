!                   *******************
                    SUBROUTINE USER_SL3
!                   *******************
!
     &(I, TIME, N, ENTET, SL3)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER PRESCRIBES THE FREE SURFACE ELEVATION FOR LEVEL
!+                IMPOSED LIQUID BOUNDARIES.
!
!history  Y. AUDOUIN (LNHE)
!+        22/10/18
!+        V8P1
!+   Creation from SL3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ENTET          |-->| IF YES, LISTING PRINTOUTS ALLOWED
!| I              |-->| NUMBER OF LIQUID BOUNDARY
!| N              |-->| GLOBAL NUMBER OF POINT
!|                |   | IN PARALLEL NUMBER OF POINT IN ORIGINAL MESH
!| SL3            |<->| THE FREE SURFACE ELEVATION
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: I,N
      DOUBLE PRECISION , INTENT(IN) :: TIME
      LOGICAL          , INTENT(IN) :: ENTET
      DOUBLE PRECISION , INTENT(INOUT) :: SL3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI,OMEGA,A
!
!-----------------------------------------------------------------------
!
      PI = 4.D0*ATAN(1.D0)
      OMEGA = PI/300.D0
      A = 0.275D0
      SL3 = A*(COS(OMEGA*TIME)-1.D0)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
