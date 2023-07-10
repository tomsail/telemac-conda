!                  **********************
                    SUBROUTINE NUTEFF
!                   **********************
!
     &(LNUT,TRR,NPOIN,IPO4,INO3,KP,KN)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES LNUT: EFFECTS OF PHOSPHORIOUS AND NITROGENIOUS
!           NUTRIMENTS ON ALGAE GROWTH
!
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| INO3           |-->| INDEX OF NO3 IN TRR
!| IPO4           |-->| INDEX OF PO4 IN TRR
!| KN             |-->| CONSTANT OF SEMI-SATURATION WITH PHOSPHATE
!| KP             |-->| CONSTANT OF HALF-SATURATION WITH NITROGEN
!| LNUT           |<--| NUTRIMENTS EFFECT ON ALGAE GROWTH
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| TRR            |-->| TRACER (CAN BE PHY: PHYTOPLAKTONIC BIOMASS)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_WAQTEL, EX_NUTEFF => NUTEFF
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,IPO4,INO3
      DOUBLE PRECISION, INTENT(IN)    :: KN,KP
      DOUBLE PRECISION, INTENT(INOUT) :: LNUT(NPOIN)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: TRR
!     LOCAL VARIABLES
      INTEGER                    :: KK
      INTRINSIC MIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!
      DO KK=1,NPOIN
        LNUT(KK)= MIN(TRR%ADR(IPO4)%P%R(KK)/(KP+TRR%ADR(IPO4)%P%R(KK)),
     &                TRR%ADR(INO3)%P%R(KK)/(KN+TRR%ADR(INO3)%P%R(KK)))
      ENDDO
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!
