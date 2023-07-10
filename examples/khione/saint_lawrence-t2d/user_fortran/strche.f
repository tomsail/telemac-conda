!                    *****************
                     SUBROUTINE STRCHE
!                    *****************
!
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE BOTTOM FRICTION COEFFICIENT
!+                IF VARIABLE IN SPACE.
!
!note     IN PARAMETER ESTIMATION WITH A LIST OF TESTS,
!+         THESE VALUES ARE DISCARDED.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!code
!+  COMMENTS CEX MUST BE REMOVED TO IMPLEMENT THE EXAMPLE.
!+  HERE A CONSTANT FRICTION VALUE IS GIVEN:
!+
!+CEX   DO I=1,NPOIN
!+CEX     CHESTR%R(I) = 60.D0
!+CEX   ENDDO
!
!history  J-M HERVOUET (LNH)
!+        01/10/96
!+        V5P2
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
!     DECLARATIONS MUST BE ADAPTED TO EVERY CODE
!     THIS EXAMPLE APPLIES TO TELEMAC2D
!
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I,ERR
!
!-----------------------------------------------------------------------
!
!     HERE A CONSTANT FRICTION VALUE IS GIVEN
!
      CALL FIND_VARIABLE(T2D_FILES(T2DGEO)%FMT,
     &                   T2D_FILES(T2DGEO)%LU ,
     &                   'ZONES                           ',
     &                   CHESTR%R, 
     &                   NPOIN,
     &                   ERR)
!
      IF(ERR.NE.0)THEN
       WRITE(LU,*)'PROBLEM TO READ FRICTION ZONES FROM GEO FILE'
       CALL PLANTE(1)
       STOP
      ENDIF
      DO I=1,NPOIN
       SELECT CASE(INT(CHESTR%R(I)))
        CASE(1)
          CHESTR%R(I) = 0.043D0
        CASE(2)
          CHESTR%R(I) = 0.05D0
        CASE(3)
          CHESTR%R(I) = 0.045D0
        CASE(4)
          CHESTR%R(I) = 0.093D0
        CASE(5)
          CHESTR%R(I) = 0.033D0
        CASE(6)
          CHESTR%R(I) = 0.034D0
        CASE(7)
          CHESTR%R(I) = 0.02625D0
        CASE(8)
          CHESTR%R(I) = 0.024D0
        CASE(9)
          CHESTR%R(I) = 0.037D0
        CASE(10)
          CHESTR%R(I) = 0.033D0
        CASE(11)
          CHESTR%R(I) = 0.04D0
        CASE(12)
          CHESTR%R(I) = 0.053D0
        CASE(13)
          CHESTR%R(I) = 0.09D0
        CASE(14)
          CHESTR%R(I) = 0.038D0
        CASE(15)
          CHESTR%R(I) = 0.027D0
        CASE(16)
          CHESTR%R(I) = 0.025D0
        CASE(17)
          CHESTR%R(I) = 0.033D0
        CASE(18)
          CHESTR%R(I) = 0.039D0
        CASE(19)
          CHESTR%R(I) = 0.025D0
       END SELECT
      ENDDO
!
!-----------------------------------------------------------------------
!
!     COMMENTS HERE MAY BE CHANGED
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'STRCHE (BIEF) : PAS DE MODIFICATION DU FROTTEMENT'
        WRITE(LU,*)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'STRCHE (BIEF): FRICTION READ FRM FENGBIN ZONES'
        WRITE(LU,*)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
