!                   *********************
                    SUBROUTINE GAIA_SLIDE
!                   *********************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Compute slide step
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE INTERFACE_GAIA
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_GAIA
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(ENTET) CALL ENTETE_GAIA(14,AT0,LT)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MAXSLOPE_GAIA'
      CALL MAXSLOPE_GAIA
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MAXSLOPE_GAIA'
!
!-----------------------------------------------------------------------
!
      RETURN
      END
