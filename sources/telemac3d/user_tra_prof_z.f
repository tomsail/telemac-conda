!                   **************************
                    SUBROUTINE USER_TRA_PROF_Z
!                   **************************
!
     &( TRA_PROF_Z, I , IPOIN2 , IPLAN , IOPT , ITRAC )
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    GIVES THE VERTICAL PROFILE FOR TRACERS.
!
!history  J-M HERVOUET (LNHE)
!+        12/09/2007
!+        V5P8
!+   First version.
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/06/2015
!+        V5P8
!+   First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF THE LIQUID BOUNDARY
!| IOPT           |-->| OPTION : 0 : USER DEFINED
!|                |   | 2 : ROUSE PROFILE FOR SEDIMENT
!|                |   | 3 : MODIFIED ROUSE PROFILE (VISCOSITY)
!| IPLAN          |-->| PLAN NUMBER
!| IPOIN2         |-->| 2D GLOBAL NUMBER OF POINT CONSIDERED
!| ITRAC          |-->| TRACER NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: TRA_PROF_Z
      INTEGER          , INTENT(IN) :: I,IPOIN2,IPLAN,IOPT
      INTEGER          , INTENT(IN) :: ITRAC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!
      WRITE(LU,*) 'USER DEFINE PROFILE MISSING IN USER_TRA_PROF_Z'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END

