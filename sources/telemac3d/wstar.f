!                   ****************
                    SUBROUTINE WSTAR
!                   ****************
!
     & (WS,HWSTAR,Z,NPOIN2,NPLAN)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES HW* AT EACH NODE WITH THE HELP OF HW*
!+                AT EACH LEVEL.
!
!history  J-M HERVOUET   (LNHE)
!+        22/01/09
!+        V5P9
!+   VELOCITY CANCELLED IF DEPTH LESS THAN 1 CM
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
!| HWSTAR         |-->| DELTAZ * WSTAR GIVEN PER LAYER
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| WS             |<--| DEPTH * WSTAR GIVEN PER POINT
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),    INTENT(IN   ) :: HWSTAR
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: WS
      DOUBLE PRECISION,  INTENT(IN   ) :: Z(*)
      INTEGER,           INTENT(IN   ) :: NPOIN2,NPLAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IAD,IPLAN
      DOUBLE PRECISION :: HH
      DOUBLE PRECISION, PARAMETER :: EPSH = 1.D-2
!
!-----------------------------------------------------------------------
!
!     INITIALISES TO 0.D0
!     WILL REMAIN FOR BOTTOM, FREE SURFACE, AND TIDAL FLATS WITH
!     DEPTH LESS THAN A MINIMUM
!
      DO I=1,NPOIN2*NPLAN
        WS%R(I)=0.D0
      ENDDO
!
!     INTERMEDIATE LEVELS
!
      IF(NPLAN.GT.2) THEN
!
        DO I=1,NPOIN2
          HH=Z(I+(NPLAN-1)*NPOIN2)-Z(I)
          IF(HH.GT.EPSH) THEN
            DO IPLAN = 2,NPLAN-1
              IAD=(IPLAN-1)*NPOIN2+I
              WS%R(IAD) = (HWSTAR%R(IAD)+HWSTAR%R(IAD-NPOIN2))*0.5D0
            ENDDO
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
