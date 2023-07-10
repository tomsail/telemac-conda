!                   ********************
                    SUBROUTINE USER_BORH
!                   ********************
!
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!brief    TAKES INTO ACCOUNT USER-SPECIFIED BOUNDARY CONDITIONS.
!+        THEY ARE GIVEN BY SEGMENT.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/08/2000
!+        V5P1
!+
!
!history  C. DENIS (SINETICS)
!+        18/03/2010
!+        V6P0
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
!history  N.DURAND (HRW)
!+        November 2016
!+        V7P2
!+   BORH void for general use now that wave / boundary parameters
!+   can be read from cli file, and that TETAB is assigned before
!+   call to BORH for all cases (uni or multidirectional waves)
!+   The subroutine can still be used for advanced definition of
!+   boundaries or if the user chooses not to use the cli file, but
!+   does nothing by default
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   PI now defined in DECLARATIONS_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I,JB
!
      INTRINSIC DCOS,DABS,DACOS
!
!-----------------------------------------------------------------------
!
! ALL ANGLES ARE IN  DEGREES
!                    -------
! ---------------------------------------
!
      DO I=1,NPTFR
        JB=BOUNDARY_COLOUR%I(I)
!
!       NORTHERNMOST SOLID BOUNDARIES
!
        IF(JB.GE.59 .AND. JB.LE.140) THEN
            LIHBOR%I(I) = KLOG
            TETAP%R(I) = DABS(40.D0 - TETAB%R(I))
            IF(DCOS(TETAP%R(I)).LT.0.D0) THEN
              TETAP%R(I) = DACOS(-DCOS(TETAP%R(I)))
            ENDIF
        ENDIF
!
        IF(JB.GE.141 .AND. JB.LE.196) THEN
            LIHBOR%I(I) = KLOG
            TETAP%R(I) = DABS(102.D0 - TETAB%R(I))
            IF(DCOS(TETAP%R(I)).LT.0.D0) THEN
              TETAP%R(I) = DACOS(-DCOS(TETAP%R(I)))
            ENDIF
        ENDIF
!
!       SOUTHERNMOST INCIDENT BOUNDARIES - ALREADY DEFINED AS SUCH IN CLI
!
!
      ENDDO !I=1,NPTFR
!
!-----------------------------------------------------------------------
!
      RETURN
      END
