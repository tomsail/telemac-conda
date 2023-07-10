!                   ***********************
                    SUBROUTINE SETTLING_VEL
!                   ***********************
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief  Compute the settling velocities according to the Stokes,
!!      Zanke and Van Rijn formulation, in case of non cohesive
!!      sediments. Sets the default value, in case of cohesive
!!      sediments.
!!
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      USE DECLARATIONS_GAIA
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER            :: ICLA
      DOUBLE PRECISION   :: DENS
!
!
!     SETTLING VELOCITY
!
      DO ICLA = 1, NSICLA
        IF(XWC0(ICLA).LT.0) THEN
!         SETTLING VELOCITY IS NOT GIVEN IN THE PARAMETER FILE AND INITIALISED HERE
!         IF T3D: SETTLING VELOCITY IS POTENTIALY MODIFIED IN 3D AND THEN GIVEN TO GAIA
!     IF T2D: ?
          DENS = (XMVS0(ICLA) - XMVE) / XMVE
          IF (DCLA(ICLA).LT.1.D-4) THEN
            XWC0(ICLA) = DENS * DCLA(ICLA) *
     &                  DCLA(ICLA) * GRAV / ( 18.D0 * VCE )
          ELSEIF (DCLA(ICLA).LT.1D-3) THEN
            XWC0(ICLA) = 10.D0 * VCE / DCLA(ICLA) *
     &       (SQRT( 1.D0 + 0.01D0* DENS * GRAV *
     &        DCLA(ICLA)**3.D0 / (VCE*VCE) ) -1.D0 )
          ELSE
            XWC0(ICLA) = 1.1D0 * SQRT( DENS * GRAV * DCLA(ICLA) )
          ENDIF
        ENDIF
        XWC(ICLA)=XWC0(ICLA)
      ENDDO
!
      RETURN
      END
