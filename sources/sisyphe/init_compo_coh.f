!                 *************************
                  SUBROUTINE INIT_COMPO_COH
!                 *************************
!
     &(ES,CONC_VASE,CONC,NPOIN,NOMBLAY,NSICLA,AVAIL,AVA0)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!+                VARIATION IN SPACE.
!
!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AVA0           |-->| VOLUME PERCENT
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| CONC           |<->| CONC OF EACH BED LAYER (KG/M3)
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| NOMBLAY        |-->| NUMBER OF LAYERS FOR CONSOLIDATION
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_INIT_COMPO_COH=> INIT_COMPO_COH
      USE DECLARATIONS_SISYPHE, ONLY : NLAYMAX
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NPOIN,NOMBLAY,NSICLA
      DOUBLE PRECISION, INTENT(INOUT)  :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(IN)     :: AVA0(NSICLA)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION  EPAI_VASE(NLAYMAX),EPAI_SABLE(NLAYMAX)

      INTEGER I,J
!
!-----------------------------------------------------------------------
!
      ! INITIALISING LOCAL VARIABLES
      EPAI_VASE(:) = 0.D0
      EPAI_SABLE(:) = 0.D0
      ! USER FUNTION
      CALL USER_INIT_COMPO_COH
     &(ES,CONC_VASE,CONC,NPOIN,NOMBLAY,NSICLA,AVAIL,AVA0,
     & EPAI_VASE, EPAI_SABLE)
!-----------------------------------------------------------------------
!
!     INITIALISING OF LAYER THICKNESS AND CONC
!

!     BY DEFAULT : UNIFORM BED COMPOSITION (KEY WORDS)
!     V6P3: IT WILL BE POSSIBLE TO HAVE A SPATIAL DISTRIBUTION OF THE BED CONC
!     V6P2: SO FAR THE MUD CONC IS CONSTANT PER LAYER
!     si mixte: calculer aussi les AVAI!
!
      DO I=1,NPOIN
        DO J= 1,NOMBLAY
!
          CONC(I,J) = CONC_VASE(J)
          ES(I,J)   = EPAI_VASE(J)
!
          IF(NSICLA.GT.1) THEN
            ES(I,J)= ES(I,J) + EPAI_SABLE(J)
            IF(ES(I,J).GE.1.D-6) THEN
!             Class 1 is for sand, class 2 is mud
              AVAIL(I,J,1)= EPAI_SABLE(J)/ES(I,J)
              AVAIL(I,J,2)= EPAI_VASE(J)/ES(I,J)
            ELSE
              AVAIL(I,J,1)= AVA0(1)
              AVAIL(I,J,2)= AVA0(2)
            ENDIF
          ENDIF
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
