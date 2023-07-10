!                 ********************************
                  SUBROUTINE BEDLOAD_ENGEL_CC_GAIA
!                 ********************************
!
     &(TETAP,CF,NPOIN,GRAV,DCLA,DENS,TETA,QSC,XMVS)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief    Engelund-Hansen bedload transport formulation.
!
!>@warning  Formulation is different from that in bedload_engel_gaia
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     CF    Quadratic friction coefficient
!>@param[in]     DENS  Relative density of sediment
!>@param[in]     DCLA   Sediment grain diameter
!>@param[in]     GRAV  Acceleration of gravity
!>@param[in]     NPOIN Number of points
!>@param[in,out] QSC   Bed load transport rate
!>@param[in,out] TETA  Dimensionless bed shear stress
!>@param[in]     TETAP Adimensional skin friction
!>@param[in]     XMVS  Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_BEDLOAD_ENGEL_CC => BEDLOAD_ENGEL_CC_GAIA
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP,CF
      INTEGER, INTENT(IN)             :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, DCLA, DENS,XMVS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETA! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I
      DOUBLE PRECISION :: CENGEL
!
      INTRINSIC SQRT
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     ADIMENSIONAL SKIN STRESS: TETAP
!
!     ADIMENSIONAL TOTAL STRESS
!
      DO I = 1, NPOIN
        IF(TETAP%R(I) <= 0.06D0) THEN
          TETA%R(I) = 0.D0
        ELSEIF(TETAP%R(I) <  0.384D0) THEN
          TETA%R(I) = SQRT( 2.5D0 * (TETAP%R(I) - 0.06D0))
        ELSEIF(TETAP%R(I) <  1.080D0) THEN
          TETA%R(I) = 1.066D0 * TETAP%R(I)**0.176D0
        ELSE
          TETA%R(I) = TETAP%R(I)
        ENDIF
      ENDDO
!
!     BEDLOAD TRANSPORT
!
      CENGEL = 0.05D0*SQRT(DENS*GRAV*DCLA**3)
      DO I=1,NPOIN
        QSC%R(I)=CENGEL*SQRT(TETA%R(I)**5)/MAX(CF%R(I),1.D-6)
      ENDDO
!
!     SOLID DISCHARGE IS TRANSFORMED IN [kg/(m*s)]
!
      CALL OS('X=CX    ', X=QSC, C=XMVS)
!-----------------------------------------------------------------------
!
      RETURN
      END
