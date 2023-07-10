!                   **************************
                    SUBROUTINE GAIA_EVOLUTIONS
!                   **************************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Compute bed evolutions (Update the bed layers)
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
!     BED EVOLUTION
      IF(BED_MODEL.EQ.1.OR.BED_MODEL.EQ.2)THEN
        IF(HIRANO.AND.VSMTYPE==0) THEN
          IF(DEBUG.GT.0) WRITE(LU,*)'BED1_UPDATE'
            CALL BED1_UPDATE(ZR,ZF,VOLU2D)
          IF(DEBUG.GT.0) WRITE(LU,*)'END BED1_UPDATE'
          IF(DEBUG.GT.0) WRITE(LU,*)'UPDATE_ACTIVELAYER_HIRANO'
            CALL BED1_UPDATE_ACTIVELAYER_HIRANO
          IF(DEBUG.GT.0) THEN
            WRITE(LU,*)'END UPDATE_ACTIVELAYER_HIRANO'
          ENDIF
        ENDIF
!
        IF(VSMTYPE==0) THEN
          IF(DEBUG.GT.0) WRITE(LU,*)'BED1_UPDATE'
          CALL BED1_UPDATE(ZR,ZF,VOLU2D)
          IF(DEBUG.GT.0) WRITE(LU,*)'END BED1_UPDATE'
        ELSEIF(VSMTYPE==1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*)'CVSP_INTERFACE'
          CALL CVSP_INTERFACE      
          IF(DEBUG.GT.0) WRITE(LU,*)'CVSP_INTERFACE'
        ENDIF !VSMTYPE     
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
