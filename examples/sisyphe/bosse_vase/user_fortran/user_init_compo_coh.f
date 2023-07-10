!                   ******************************
                    SUBROUTINE USER_INIT_COMPO_COH
!                   ******************************
!
     &(ES,CONC_VASE,CONC,NPOIN,NOMBLAY,NSICLA,AVAIL,AVA0,
     & EPAI_VASE,EPAI_SABLE)
!
!***********************************************************************
! SISYPHE
!***********************************************************************
!
!brief    USER INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
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
!| EPAI_VASE      |<->| MUD THICKNESS
!| EPAI_SABLE     |<->| SAND THICKNESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_USER_INIT_COMPO_COH
     &                       => USER_INIT_COMPO_COH
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
      DOUBLE PRECISION, INTENT(INOUT)  :: EPAI_VASE(NLAYMAX)
      DOUBLE PRECISION, INTENT(INOUT)  :: EPAI_SABLE(NLAYMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      EPAI_VASE(1)=0.01D0
      EPAI_VASE(2)=0.02D0
      EPAI_VASE(3)=0.03995D0
      EPAI_VASE(4)=0.0437D0
      EPAI_VASE(5)=0.0517D0
      EPAI_VASE(6)=0.1259D0
      EPAI_VASE(7)=0.4889D0
      EPAI_VASE(8)=1.5071D0
      EPAI_VASE(9)=0.86410D0
!
!-----------------------------------------------------------------------
!
      RETURN
      END
