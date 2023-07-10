!                   ***********************
                    SUBROUTINE ALGAE_DEATH
!                   ***********************
!
     &(ALD,MP,CMOR,TRR,TRESP,GT,TOX,NPOIN )
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES THE DISAPPEARANCE RATE OF ALGAE
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALD            |<--| ALGAE DEATH
!| CMOR           |-->| COEFFICIENTS OF ALGAE DEATH RATE AT 20C
!| RAY            |-->| EFFECT OF SUNSHINE in [0,1]
!| GT             |-->| EFFECT OF OF TEMPERATURE ON ALGAE GROWTH
!|                |   | GT=T/20,  T: WATER TEMPERATURE
!| MP             |<--| DISAPPEAANCE RATE OF ALGAL BIOMASS AT 20 DEG C
!| NPOIN          |-->| NUMBER OF NODES
!| TOX            |-->| COEFFICIENT OF WATER TOXICITY
!| TRR            |-->| TRACER (PHYTOPLANCTOPN BIOMASS)
!| TRESP          |-->| RESPIRATION RATE OF ALGAL BIOMASS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_WAQTEL, EX_ALGAE_DEATH=>ALGAE_DEATH
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN   ) :: NPOIN
      DOUBLE PRECISION, INTENT(IN   ) :: CMOR(2),TRR(NPOIN),TOX,TRESP
      DOUBLE PRECISION, INTENT(INOUT) :: ALD(NPOIN),MP(NPOIN)
      TYPE(BIEF_OBJ)  , INTENT(IN   ) :: GT
!     LOCAL VARIABLES
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO I=1,NPOIN
        MP(I) = CMOR(1)+CMOR(2)*TRR(I)+TOX
        ALD(I)= GT%R(I)*(MP(I)+TRESP)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
