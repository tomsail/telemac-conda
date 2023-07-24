!                   *****************************
                    SUBROUTINE GAIA_CONSOLIDATION
!                   *****************************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Handles consolidation part of gaia.
!!
!!       Only works for bed model 2 so far
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
      IF(BED_MODEL.EQ.2) THEN
!       BED1_UPDATE is called to update ratio_mud
        CALL BED1_UPDATE(ZR,ZF,VOLU2D)
        CALL BED1_CONSOLIDATION_LAYER
      ELSEIF(BED_MODEL.EQ.3)THEN
!>      @todo Create gibson model
        WRITE(LU,*)'GIBSON IN GAIA: TO DO'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
