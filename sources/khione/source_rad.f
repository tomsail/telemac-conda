!                     *********************
                      SUBROUTINE SOURCE_RAD
!                     *********************
!
     & (NPOIN3,NPOIN2,NPLAN,Z,RHOT3D,TEXP)
!
!***********************************************************************
! KHIONE   V8P3
!***********************************************************************
!
!brief    Penetration of the radiative flux in the water column
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPLAN          |-->| NUMBER OF VERTICAL PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN ONE PLANE
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| RHOT3D         |-->| WATER DENSITY
!| TEXP           |-->| EXPLICIT SOURCE TERM.
!| TN             |-->| TRACERS AT TIME N
!| Z              |-->| Z COORDINATES FOR NODES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF_DEF
      USE DECLARATIONS_KHIONE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPOIN2,NPOIN3,NPLAN
      DOUBLE PRECISION, INTENT(IN)  :: Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: RHOT3D(NPOIN3)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TEXP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,IPLAN
      DOUBLE PRECISION LAMB
      INTRINSIC EXP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO I=1,NPOIN2
        DO IPLAN=1,NPLAN
          J = I + (IPLAN-1)*NPOIN2
          LAMB=RHOT3D(NPOIN3-NPOIN2+I)*CP_EAU
          TEXP%ADR(IND_T)%P%R(J) = TEXP%ADR(IND_T)%P%R(J) + EXTINC*
     &                      EXP(EXTINC*(Z(J)-Z(I+(NPLAN-1)*NPOIN2)))*
     &                      PHPS%R(I)/LAMB
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
