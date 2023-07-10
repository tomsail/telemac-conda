!                   ******************
                    SUBROUTINE SHIELDS
!                   ******************
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Compute the critical shields parameter according to
!!       the Shields formulation only for non cohesive sediments.
!!       For cohesive sediment the Partheniades formula is used.
!!
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      USE DECLARATIONS_GAIA
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER            :: ICLA,ISAND,IPOIN
      DOUBLE PRECISION   :: DSTAR,DENS
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO ICLA=1,NSICLA
        IF(TYPE_SED(ICLA).EQ.'NCO') THEN
          DENS = (XMVS0(ICLA) - XMVE) / XMVE
          IF(AC(ICLA).LT.0) THEN
            DSTAR = DCLA(ICLA)*(GRAV*DENS/VCE**2)**(1.D0/3.D0)
            IF (DSTAR <= 4.D0) THEN
              AC(ICLA) = 0.24D0/DSTAR
            ELSEIF (DSTAR <= 10.D0) THEN
              AC(ICLA) = 0.14D0*DSTAR**(-0.64D0)
            ELSEIF (DSTAR <= 20.D0) THEN
              AC(ICLA) = 0.04D0*DSTAR**(-0.1D0)
!             CORRECTION 27/06/2016
!            ELSEIF (DSTAR <= 150.D0) THEN
            ELSEIF (DSTAR <= 72.D0) THEN
              AC(ICLA) = 0.013D0*DSTAR**0.29D0
            ELSE
!             CORRECTION 30/05/2012
!             AC(ICLA) = 0.055D0
              AC(ICLA) = 0.045D0
            ENDIF
          ENDIF
        ENDIF
      ENDDO
!
      IF(NSAND.GT.0) THEN
        DO ISAND = 1,NSAND
          TOCE_SAND0(ISAND) = AC(NUM_ISAND_ICLA(ISAND))*
     &    DCLA(NUM_ISAND_ICLA(ISAND))*GRAV*(XMVS0(NUM_ISAND_ICLA(ISAND))
     &    - XMVE)
          DO IPOIN=1,NPOIN
            TOCE_SAND(ISAND,IPOIN) =TOCE_SAND0(ISAND)
          ENDDO
        ENDDO
      ENDIF
!
      RETURN
      END
