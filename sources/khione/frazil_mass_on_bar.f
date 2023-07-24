!                   ******************************
                    SUBROUTINE FRAZIL_MASS_ON_BAR
!                   ******************************
!
     &    (RFR0,RFR1,DB,BAR,NBAR,ANG1,FM1,FMT)
!
!***********************************************************************
! KHIONE                                                          V8P0
!***********************************************************************
!
!brief
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ANG1           |<--|
!| BAR            |<--|
!| DB             |<--|
!| FM1            |-->|
!| FMT            |-->|
!| NBAR           |<--|
!| RFR0,RFR1      |<--|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_KHIONE, ONLY : RHO_ICE,EF0
      USE INTERFACE_KHIONE, EX_FMOB => FRAZIL_MASS_ON_BAR
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NBAR
      DOUBLE PRECISION, INTENT(IN)    :: RFR0,RFR1,BAR,ANG1,DB
      DOUBLE PRECISION, INTENT(INOUT) :: FM1,FMT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION :: RB,DR,AMNU,ASH,RATIO,ADONT,AICE
!
!      RB = RFR0/2.0/COS(ANG1)
      RB = DB/2.D0
      DR = 2.D0*RB - RFR0
!
      AMNU = RFR0**2*COS(ANG1)*SIN(ANG1) + 2.D0*ANG1*RB**2 -
     &       RB**2*COS(2.D0*ANG1)*SIN(2.D0*ANG1)
      ASH = 2.D0*ANG1*RB**2 - (ANG1*RFR0**2 - RB*RFR0*SIN(ANG1)) ! SHADE AREA
!
      IF (RFR1.GT.2.D0*RB) THEN
        AICE = ANG1*RFR1**2 - AMNU
      ELSE
        ADONT = (RFR1**2 - RFR0**2)*ANG1
        RATIO = (RFR1 - RFR0) / DR
        AICE = ADONT - RATIO*ASH
      ENDIF
!
      FM1 = AICE*BAR*RHO_ICE*(1.D0 - EF0)
      FMT = FM1*NBAR
!
      END SUBROUTINE FRAZIL_MASS_ON_BAR
