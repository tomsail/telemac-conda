!                   *****************
                    SUBROUTINE DRIUTI
!                   *****************
!
     & (FRI, RI, ITYP, ITRAC, NPOIN3)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    USER DEFINED DAMPING FUNCTIONS.
!
!warning  DEFAULT VALUE FOR THE DUMPING
!+            FUNCTION (FRI) IS 1
!warning  THE KEYWORD 'DAMPING FUNCTION' MUST BE SET TO 1 IN
!+            THE STEERING FILE (USER-DEFINED)
!
!history  A MALCHEREK (HANOVRE); E PELTIER (LNH)    ; F LEPEINTRE (LNH)
!+        25/11/97
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!| FRI            |<->| DAMPING FUNCTION
!| ITRAC          |-->| TRACER NUMBER
!| ITYP           |-->| =1 FOR VELOCITIES
!|                |   | =2 FOR TRACERS
!| NPOIN3         |-->| NUMBER OF POINTS IN THE 3D MESH
!| RI             |<->| RICHARDSON NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
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
      CALL USER_DRIUTI(FRI, RI, ITYP, ITRAC, NPOIN3)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DRIUTI
