!                   ********************************
                    SUBROUTINE GAIA_SUSPENSION_ERODE
!                   ********************************
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief After computing fluer, set compute_susp.eq.true for coupling
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     COMPUTES EROSION FLUX   : FLUER
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     SKIN FRICTION TAUP
!
      CALL OS('X=CYZ   ', X=TAUP, Y=TOBCW_MEAN, Z=MU, C=1.D0)
      CALL OS('X=+(Y,C)', X=TAUP, Y=TAUP, C=0.D0)
!
      IF (BED_MODEL.EQ.1.OR.BED_MODEL.EQ.2)THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'BED1_SUSPENSION_ERODE'
        CALL BED1_SUSPENSION_ERODE
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_BED1_SUSPENSION_ERODE'
      ELSE
        WRITE(LU,*)'ONLY BED_MODEL 1 or 2 for tis time'
        STOP
      ENDIF
!
      RETURN
      END
