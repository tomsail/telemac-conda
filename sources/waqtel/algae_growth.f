!                   ************************
                    SUBROUTINE ALGAE_GROWTH
!                   ************************
!
     &(ALG,CMAX,RAY,GT,NUTR,TOX,NPOIN )
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES THE GROWTH RATE OF ALGAE
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!history  R. ATA (LNHE)
!+        02/04/2016
!+        V7P2
!+        VARIABLE TEMPERATURE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALG            |<--| ALGAE GROWTH
!| CMAX           |-->| MAXIMAL GROWTH RATE AT 20C
!| RAY            |-->| EFFECT OF SUNSHINE in [0,1]
!| GT             |-->| EFFECT OF OF TEMPERATURE ON ALGAE GROWTH
!|                |   | GT=T/20,  T: WATER TEMPERATURE
!| NUTR           |-->| EFFECT OF PHOSPHORED AND NITROGENIOUS NUTRIMENTS
!| TOX            |-->| COEFFICIENT OF WATER TOXICITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_WAQTEL, EX_ALGAE_GROWTH => ALGAE_GROWTH
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN   ) :: NPOIN
      DOUBLE PRECISION, INTENT(IN   ) :: CMAX,RAY(NPOIN),NUTR(NPOIN),TOX
      DOUBLE PRECISION, INTENT(INOUT) :: ALG(NPOIN)
      TYPE(BIEF_OBJ)   , INTENT(IN  ) :: GT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DO I=1,NPOIN
        ALG(I)=CMAX*RAY(I)*GT%R(I)*NUTR(I)*TOX
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
