!                   **********************************
                    SUBROUTINE GAIA_SUSPENSION_DEPOSIT
!                   **********************************
     &(CODE)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Compute deposition step of suspension
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      CODE       Name of calling programme (telemac2d or 3d)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_GAIA_SUSPENSION_DEPOSIT =>
     &                    GAIA_SUSPENSION_DEPOSIT
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
      CHARACTER(LEN=24), INTENT(IN) :: CODE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'GAIA PART 1, COMPUTE_SUSP TRUE'
!
!     HERE WE COME BACK AFTER SOLUTION OF ADVECTION DIFFUSION EQ
!
      IF(BED_MODEL.EQ.1.OR.BED_MODEL.EQ.2)THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'BED1_SUSPENSION_DEPOSIT'
        CALL BED1_SUSPENSION_DEPOSIT(CODE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END BED1_SUSPENSION_DEPOSIT'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

