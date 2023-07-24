!                   **********************
                    SUBROUTINE USER_DRIUTI
!                   **********************
!
     & (FRI, RI, ITYP, ITRAC, NPOIN3)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER DEFINED DAMPING FUNCTIONS.
!
!warning  DEFAULT VALUE FOR THE DUMPING
!+            FUNCTION (FRI) IS 1
!warning  THE KEYWORD 'DAMPING FUNCTION' MUST BE SET TO 1 IN
!+            THE STEERING FILE (USER-DEFINED)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FRI            |<->| DAMPING FUNCTION
!| ITRAC          |-->| TRACER NUMBER
!| ITYP           |-->| =1 FOR VELOCITIES
!|                |   | =2 FOR TRACERS
!| NPOIN3         |-->| NUMBER OF POINTS IN THE 3D MESH
!| RI             |<->| RICHARDSON NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE BIEF, ONLY: OV
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ITYP, ITRAC, NPOIN3
      DOUBLE PRECISION, INTENT(INOUT) :: FRI(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: RI(NPOIN3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(.FALSE.) THEN
      IF(ITYP.EQ.1) THEN
!
!       DAMPING FUNCTION FOR VELOCITIES
!
        CALL OV('X=C     ', X=FRI, C=1.D0, DIM1=NPOIN3)
!
      ELSEIF(ITYP.EQ.2) THEN
!
!       DAMPING FUNCTION FOR TRACERS
!
        CALL OV('X=C     ', X=FRI, C=1.D0, DIM1=NPOIN3)
!
      ELSE
!
        WRITE(LU,*) 'USER_DRIUTI: UNEXPECTED PARAMETER ITYP: ',ITYP
        CALL PLANTE(1)
        STOP
!
      ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
