!                   ********************
                    SUBROUTINE TOBW_GAIA
!                   ********************
!
     &(TOBW ,CF, FW, UW,TW,HN,NPOIN,XMVE)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the wave friction stress. the friction
!!       coefficient is computed using SWART formulation (1976).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     CF    Quadratic friction coefficient
!>@param[in]     FW    Quadratic friction coefficient (wave)
!>@param[in]     HN    Water depth at time n
!>@param[in]     NPOIN Number of points
!>@param[in,out] TOBW  Wave induced shear stress
!>@param[in]     TW    Wave period
!>@param[in]     UW    Orbital velocity (wave)
!>@param[in]     XMVE  Fluid density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
!
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: UW(NPOIN),TW(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XMVE
      DOUBLE PRECISION, INTENT(INOUT) :: TOBW(NPOIN),FW(NPOIN)
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION KS,AUX
      DOUBLE PRECISION PI,AW
      DOUBLE PRECISION, PARAMETER :: KARMAN = 0.4D0
!
!-----------------------------------------------------------------------
!
      PI = 4.D0 * ATAN( 1.D0 )
!
!-----------------------------------------------------------------------
!
      DO  I=1,NPOIN
!       KS : NIKURADSE COEFFICIENT (TOTAL FRICTION)
        AUX=1.D0+KARMAN*SQRT(2.D0/MAX(CF(I),1.D-10))
        KS=30.D0*MAX(HN(I),1.D-8)*EXP(-AUX)
        AW= UW(I)*TW(I) / (2.D0*PI)
        IF(AW/KS.GT.1.59D0) THEN
          FW(I)=EXP( -6.D0 + 5.2D0 * (AW/KS)**(-0.19D0) )
        ELSE
          FW(I)=0.3D0
        ENDIF
        TOBW(I)=0.5D0 * XMVE * FW(I) * UW(I)*UW(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE TOBW_GAIA
