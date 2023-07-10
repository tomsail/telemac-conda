!                   **********************
                    SUBROUTINE USER_CONDIH
!                   **********************
!
!
!***********************************************************************
! ARTEMIS
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS.
!
!history  J-M HERVOUET (LNH)
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION AMPLC
      DOUBLE PRECISION X0,Y0,RR0,RR(NPOIN)

      INTRINSIC EXP, SQRT
!
!-----------------------------------------------------------------------
!
      IF(COURANT) THEN
        AMPLC=0.D0
        X0=2500.D0
        Y0=2500.D0
        RR0=900.D0
!
        DO I=1,NPOIN
          UC%R(I)=AMPLC
          VC%R(I)=AMPLC
          RR(I)=SQRT((X(I)-X0)**2+(Y(I)-Y0)**2)
!
          IF(RR(I).LT.RR0)THEN
            UC%R(I)=  (0.9D0*(RR(I)/RR0)**2)*((Y(I)-Y0)/RR(I))
            VC%R(I)= -(0.9D0*(RR(I)/RR0)**2)*((X(I)-X0)/RR(I))
          ELSE
            UC%R(I)=  (1.D0*EXP(-((1000.D0-RR(I))/300.D0)**2))*
     &                ((Y(I)-Y0)/RR(I))
            VC%R(I)= -(1.D0*EXP(-((1000.D0-RR(I))/300.D0)**2))*
     &                ((X(I)-X0)/RR(I))
          ENDIF
!
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
