!                  ***********************
                    SUBROUTINE DEPOS_FX
!                   **********************
!
     &(SEDP,TAUB,CSUS,TAUS,VITCHU,NPOIN)
!
!***********************************************************************
! TELEMAC2D                                                       V7P2
!***********************************************************************
!
!brief    COMPUTES DEPOSITION FLUX
!
!
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| SEDP           |<--| DEPOSITION FLUX
!| TAUB           |-->| BED SHEAR STRESS
!| TAUS           |-->| SEDIMENTATION CRITICAL STRESS
!| CSUS           |-->| SUSPENDED LOAD (TRACER 1)
!| VITCHU         |-->| SEDIMENT SETTLING VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_WAQTEL, EX_DEPOS_FX => DEPOS_FX
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: TAUS,VITCHU
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TAUB,CSUS
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: SEDP
      INTRINSIC MAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     LOCAL VARIABLES
      INTEGER I
!
!
      IF (ABS(TAUS).LT.1.E-10)THEN
        WRITE(LU,*) 'DEPOS_FX: CRITICAL STRESS OF RESUSPENSION    '
        WRITE(LU,*) '          TAUS VERY SMALL OR NIL - VERIFY !!!'
        WRITE(LU,*) '          TAUS = ',TAUS
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     THIS WAY WORKS WELL FOR 2D AND FOR 3D AS WELL SINCE BED LAYER IS
!     FOR I=1 TO NPOIN
      DO I=1,NPOIN
        SEDP%R(I)=VITCHU*CSUS%R(I)*MAX(1.D0-TAUB%R(I)/TAUS,0.D0)
      ENDDO
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!
