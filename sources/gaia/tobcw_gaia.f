!                   *********************
                    SUBROUTINE TOBCW_GAIA
!                   *********************
!
     & (TOB, TOBW, THETAC, THETAW, TOBCW_MEAN, TOBCW_MAX, NPOIN)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Compute mean wave + current shear stress. SOULSBY (1993)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     NPOIN      Number of points
!>@param[in]     THETAW     Waves direction (deg wrt ox axis)
!>@param[in]     THETAC     Current direction
!>@param[in]     TOB        Current shear stress
!>@param[in]     TOBW       Wave shear stress
!>@param[in,out] TOBCW_MEAN Wave + current mean shear stress
!>@param[in,out] TOBCW_MAX  Wave + current maximum shear stress
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA, EX_TOBCW_GAIA=>TOBCW_GAIA
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
!
      DOUBLE PRECISION, INTENT(IN)    :: THETAC(NPOIN), THETAW(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: TOB(NPOIN),TOBW(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: TOBCW_MEAN(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: TOBCW_MAX(NPOIN)
      !
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION DIFF_THETA, PI
!
!-----------------------------------------------------------------------
!
      PI = 4.D0 * ATAN( 1.D0 )
!
!-----------------------------------------------------------------------
!
      DO  I=1,NPOIN
!       COMPUTE MEAN WAVE + CURRENT SHEAR STRESS SOULSBY (1997)
        IF(TOB(I)+TOBW(I).GT.1E-9) THEN
          TOBCW_MEAN(I)= TOB(I)*(1.D0 + 1.2D0 *
     &                   (TOBW(I)/(TOB(I)+TOBW(I)))**3.2D0)
        ELSE
          TOBCW_MEAN(I) = 0.D0
        ENDIF
!       COMPUTE MAXIMUM WAVE + CURRENT SHEAR STRESS SOULSBY (1997)
        DIFF_THETA = THETAW(I)-THETAC(I)
        TOBCW_MAX(I) = DSQRT((TOBCW_MEAN(I)+
     &                 TOBW(I)*DCOS(DIFF_THETA*PI/180.D0))**2 +
     &                 (TOBW(I)*DSIN(DIFF_THETA*PI/180.D0))**2)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE TOBCW_GAIA
