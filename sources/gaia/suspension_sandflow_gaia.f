!                 ***********************************
                  SUBROUTINE SUSPENSION_SANDFLOW_GAIA
!                 ***********************************
!
     &  (DCLA, NPOIN, GRAV, XMVE, XMVS, ZERO, CSTAEQ, HN,
     &   U2D, V2D, CSRATIO, RATIO_TOCE)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Attempt to ensure GAIA computes C_a in the same way as
!!       HRW-SANDFLOW. Uses Soulsby-van Rijn formula to give a
!!       (depth-averaged) saturated concentration.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     DCLA       Sediment diameter d50
!>@param[in]     NPOIN      Number of points
!>@param[in]     GRAV       Acceleration due to gravity
!>@param[in]     XMVE       Water density
!>@param[in]     XMVS       Sediment density
!>@param[in]     ZERO       Threshold for a variable to be considered equal to
!!                          zero
!>@param[in,out] CSTAEQ     Equilibrium sediment concentration
!>@param[in]     HN         Water depth
!>@param[in]     U2D        Depth averaged velocity in x direction
!>@param[in]     V2D        Depth averaged velocity in y direction
!>@param[in]     CSRATIO    Ratio between bottom concentration and
!                           average concentration
!>@param[in]     RATIO_TOCE Ratio between critical shear stress of pure
!!                          sediment and mixed sediment in the same layer
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_GAIA,
     & EX_SUSPENSION_SANDFLOW => SUSPENSION_SANDFLOW_GAIA
      USE BIEF
      USE DECLARATIONS_GAIA, ONLY: VCE,UW,HOULE,PRIVE,KSPRATIO,D90,
     &                             NSAND
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!     GLOBAL VARIABLES
!      -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)       :: HN,U2D,V2D,CSRATIO
      INTEGER,          INTENT(IN)       :: NPOIN
      DOUBLE PRECISION, INTENT(IN)       :: GRAV, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)       :: ZERO,DCLA
      TYPE(BIEF_OBJ),   INTENT(INOUT)    :: CSTAEQ
      TYPE(BIEF_OBJ),   INTENT(IN)       :: RATIO_TOCE
!
!     LOCAL VARIABLES
!     ------------------
      INTEGER            :: I
      DOUBLE PRECISION   :: DSTAR,DENS,COEFF,UCR,ASS,UB
      DOUBLE PRECISION   :: WORB, CD, FD90
      DOUBLE PRECISION, PARAMETER :: Z0=0.006D0
      INTRINSIC MAX
!======================================================================!
!======================================================================!
!                               PROGRAMME                              !
!======================================================================!
!======================================================================!
!     VALUE OF FD90 OF SEDIMENT
!     WITH ONE SAND CLASS, IT IS USER SET
!     WITH MORE THAN ONE SAND CLASS, IT IS
!     CONSIDERED AS THE RATIO BETWEEN SKIN FRICTION AND MEAN
!     DIAMETER * D50
      IF (NSAND.EQ.1) THEN
        FD90 = D90
      ELSE
        FD90=DCLA*KSPRATIO
      ENDIF
!
!-----------------------------------------------------
      DENS  = (XMVS - XMVE )/ XMVE

      DO I=1,NPOIN
!       *****************
!       IV - EROSION FLUX
!       *****************
        DSTAR = DCLA*(GRAV*DENS/(VCE*VCE))**(1.D0/3.D0)
        COEFF = (DENS*GRAV*DCLA)**1.2D0
        IF(DSTAR.LE. ZERO) THEN
          WRITE(LU,*)'ERROR SUSPENSION_SANDFLOW_GAIA:DSTAR =',DSTAR,
     &               DCLA,VCE,GRAV
          CALL PLANTE(1)
          STOP
        ENDIF
        IF ((HN%R(I).GT.ZERO).AND.(COEFF.GT.ZERO)) THEN
!         CRITICAL VELOCITIES:
          IF (DCLA<500.E-6) THEN
!            UCR = 0.19D0*(D50**0.1D0)*LOG10(4.0D0*(HN%R(I)/D50))    ! SANDFLOW
            UCR = 0.19D0*(DCLA**0.1D0)*LOG10(4.0D0*HN%R(I)/FD90)  ! CORRECT
          ELSE
!               UCR = 8.5D0*(D50**0.6D0)*LOG10(4.D0*(HN%R(I)/D50))      ! SANDFLOW
            UCR = 8.5D0*(DCLA**0.6D0)*LOG10(4.D0*HN%R(I)/FD90)    ! CORRECT
          END IF
!
!         TOCE IS MODIFIED IN TOCE_MIX : TOCE_MIX=TOCE_SAND*RATIO_TOCE
!         WE CAN APPLICATE  RATIO**0.5 ON UCR
!         because To=rho*CD*U**2
!         (ratio_toce=1 if no mud)
          UCR = UCR*SQRT(RATIO_TOCE%R(I))
!
!         WAVE ORBITAL VELOCITIES:
          IF (HOULE) THEN
            CD   = (0.4D0/(LOG(MAX(HN%R(I),Z0)/Z0)-1.D0))**2.D0
            WORB = (0.018D0/CD)*(UW%R(I)**2.D0)
          ELSE
            WORB = 0.D0
          ENDIF
          ASS = (0.012D0*DCLA*(DSTAR**(-0.6D0)))/COEFF      ! JUST FOR SUSPENDED LOAD (A LA SANDFLOW)
          UB  = (U2D%R(I)*U2D%R(I))+(V2D%R(I)*V2D%R(I))+WORB ! VEL
!             NOTE WE MULTIPLY EQ. (136A) OF SOULSBY (1997) THROUGH BY (UH)^{-1} TO GET DIMENSIONLESS
!              DEPTH-AVERAGED SATURATION CONCENTRATION CA:
          CSTAEQ%R(I)=ASS*(MAX(0.D0,(SQRT(UB)-UCR))**2.4D0)/HN%R(I) !MISSING ORB VELS
!              DUMP SATURATED CONC OUTPUT TO SUPPLEMENTARY VARIABLE A (PRIVATE ARRAY WITH INDEX 1):
!         to do: create graphic printout for cstaeq
!         PRIVE%ADR(1)%P%R(I)=CSTAEQ%R(I)
!         CSRATIO is equal to 1 if the Miles profile is not used
          CSTAEQ%R(I)=CSTAEQ%R(I)*CSRATIO%R(I)
        ELSE
          CSTAEQ%R(I) = 0.D0
        ENDIF
!------------  in kg/m3
        CSTAEQ%R(I) = XMVS*CSTAEQ%R(I)
      ENDDO
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE SUSPENSION_SANDFLOW_GAIA
