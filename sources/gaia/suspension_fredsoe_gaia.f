!               **********************************
                SUBROUTINE SUSPENSION_FREDSOE_GAIA
!               **********************************
!
     &(DCLA,TAUP,NPOIN,GRAV,XMVE,XMVS,AC,CSTAEQ,RATIO_TOCE)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the reference concentration at z= 2*d50
!!       according to ZYSERMAN and FREDSOE formulation (1994).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     AC         Critical shields parameter
!>@param[in]     FDM        Sediment grain diameter
!>@param[in,out] CSTAEQ     Equilibrium concentration
!>@param[in]     GRAV       Acceleration of gravity
!>@param[in]     NPOIN      Number of points
!>@param[in]     RATIO_TOCE Ratio between critical shear stress of pure
!!                          sediment and mixed sediment in the same layer
!>@param[in]     TAUP       Shear stress
!>@param[in]     XMVE       Fluid density
!>@param[in]     XMVS       Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,EX_SUSPENSION_FREDSOE =>
     &       SUSPENSION_FREDSOE_GAIA
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!     GLOBAL VARIABLES
!     -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    ::  TAUP
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: AC,DCLA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ
      TYPE(BIEF_OBJ),   INTENT(IN)    :: RATIO_TOCE
!     LOCAL VARIABLES
!     ------------------
      INTEGER            :: I
      DOUBLE PRECISION   ::  TETAP,AUX
!
!     MAXIMUM CONCENTRATION CORRESPONDING TO DENSE PACKING
!
      DOUBLE PRECISION, PARAMETER :: CMAX = 0.6D0
!!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     ********************************
!      I - CRITICAL SHIELDS PARAMETER
!     ********************************
!
      DO I=1,NPOIN
!
!       ****************** !
!       II - SKIN FRICTION !
!       ****************** !
!
        TETAP = TAUP%R(I) / (GRAV*(XMVS-XMVE)*DCLA)
!
!       *****************
!       IV - EROSION FLUX
!       *****************
!       CONCENTRATION INCREASED BY AVA BECAUSE IT IS COMPUTED
!       ONLY WITH ONE CLASS OF SEDIMENT (ASSUMPTION)
!
!        TOCE IS MODIFIED IN TOCE_MIX : TOCE_MIX=TOCE_SAND*RATIO_TOCE
!        WE CAN APPLICATE DIRECTLY THIS RATIO ON AC
!        because TOCE=AC*DM*GRAV*(XMVS-XMVE)
!        (ratio_toce=1 if no mud)
!        IF(TETAP.GT.(AC) THEN
!          AUX=(TETAP-AC)**1.75D0
        IF(TETAP.GT.(AC*RATIO_TOCE%R(I))) THEN
          AUX=(TETAP-(AC*RATIO_TOCE%R(I)))**1.75D0
          CSTAEQ%R(I) = 0.331D0*AUX/(1.D0+0.72D0*AUX)
          CSTAEQ%R(I) = MIN(CSTAEQ%R(I),CMAX)
        ELSE
          CSTAEQ%R(I) = 0.D0
        ENDIF
!------------  in kg/m3
        CSTAEQ%R(I) = XMVS*CSTAEQ%R(I)
!
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
