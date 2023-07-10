!                   **********************
                    SUBROUTINE TAUB_WAQTEL
!                   **********************
!
     &(CF,DENSITY,TAUB,NPOIN,UN,VN)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES BED SHEAR STRESS FOR WAQTEL.
!+        SEE THE USE OF TOB_SISYPHE
!
!
!
!history  R. ATA (EDF LAB, LNHE)
!+        17/03/2016
!+        V7P2
!+    First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| FRICTION COEFFICIENT
!| DENSITY        |-->| DENSITY
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| TAUB           |<--| BED SHEAR STRESS
!| UN,VN          |-->| VELOCITY COMPONENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_WAQTEL, EX_TAUB_WAQTEL => TAUB_WAQTEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: NPOIN
      DOUBLE PRECISION , INTENT(IN)    :: DENSITY
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TAUB
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION CC
!
!-----------------------------------------------------------------------
!
!     TOB=0.5*RHO*CF*U^2
!
      CC=0.5D0*DENSITY
!
      CALL CPSTVC(CF,TAUB)
!
      DO I=1,NPOIN
        TAUB%R(I)=CC*CF%R(I)*(UN%R(I)**2+VN%R(I)**2)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

