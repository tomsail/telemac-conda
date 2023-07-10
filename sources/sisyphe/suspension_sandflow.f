!                   ***************************************
                    SUBROUTINE SUSPENSION_SANDFLOW
!                   ***************************************
!
     &  (FDM, FD90, NPOIN, GRAV,
     &   XMVE, XMVS, ZERO, CSTAEQ,HN,U2D,V2D,CSRATIO)
!
!***********************************************************************
! SISYPHE
!***********************************************************************
!
!brief    Attempt to ensure SISYPHE computes C_a in the same way as
!+        HRW-SANDFLOW. Uses Soulsby-van Rijn formula to give a
!+        (depth-averaged) saturated concentration.
!
!history    D.M.Kelly (HRW)
!+          19/07/2012
!+          V6P1
!+        Original implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->|
!| FDM            |-->| D50
!| GRAV           |-->| ACCELERATION DUE TO GRAVITY
!| NPOIN          |-->|
!| XMVE           |-->| WATER DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
!| CSTAEQ         |<->| EQUILIBRIUM SEDIMENT CONCENTRATION
!! HN             |-->| WATER DEPTJ
!| U2D            |-->| DEPTH AVERAGED VELOCITY IN X DIRECTION
!| V2D            |-->| DEPTH AVERAGED VELOCITY IN Y DIRECTION
!| T2             |<->| WORK ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     & EX_SUSPENSION_SANDFLOW => SUSPENSION_SANDFLOW
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : VCE,UW,HOULE,PRIVE
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)       :: HN,U2D,V2D,CSRATIO
      INTEGER,          INTENT(IN)       :: NPOIN
      DOUBLE PRECISION, INTENT(IN)       :: GRAV, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)       :: ZERO,FDM,FD90
      TYPE(BIEF_OBJ),   INTENT(INOUT)    :: CSTAEQ
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER            :: I
      DOUBLE PRECISION   :: DSTAR,DENS,COEFF,UCR,ASS,UB
      DOUBLE PRECISION   :: WORB,CD
      DOUBLE PRECISION, PARAMETER :: Z0=0.006D0
      INTRINSIC MAX
!======================================================================!
!======================================================================!
!                               PROGRAMME                              !
!======================================================================!
!======================================================================!

      DENS  = (XMVS - XMVE )/ XMVE

      DO I=1,NPOIN
        ! ***************** !
        ! IV - EROSION FLUX ! (_IMP_)
        ! ***************** !
        DSTAR = FDM*(GRAV*DENS/(VCE*VCE))**(1.D0/3.D0)
        COEFF = (DENS*GRAV*FDM)**1.2D0
        IF(DSTAR.LE. ZERO) THEN
          WRITE(LU,*)'ERROR SUSPENSION_SANDFLOW:DSTAR = ',DSTAR,FDM,
     &               VCE,GRAV
          CALL PLANTE(1)
          STOP
        ENDIF
        IF ((HN%R(I).GT.ZERO).AND.(COEFF.GT.ZERO)) THEN
!         CRITICAL VELOCITIES:
          IF (FDM<500.E-6) THEN
!            UCR = 0.19D0*(D50**0.1D0)*LOG10(4.0D0*(HN%R(I)/D50))    ! SANDFLOW
            UCR = 0.19D0*(FDM**0.1D0)*LOG10(4.0D0*HN%R(I)/FD90)  ! CORRECT
          ELSE
!               UCR = 8.5D0*(D50**0.6D0)*LOG10(4.D0*(HN%R(I)/D50))      ! SANDFLOW
            UCR = 8.5D0*(FDM**0.6D0)*LOG10(4.D0*HN%R(I)/FD90)    ! CORRECT
          END IF
!              WAVE ORBITAL VELOCITIES:
          IF (HOULE) THEN
            CD   = (0.4D0/(LOG(MAX(HN%R(I),Z0)/Z0)-1.D0))**2.D0
            WORB = (0.018D0/CD)*(UW%R(I)**2.D0)
          ELSE
            WORB = 0.D0
          ENDIF
          ASS = (0.012D0*FDM*(DSTAR**(-0.6D0)))/COEFF      ! JUST FOR SUSPENDED LOAD (A LA SANDFLOW)
          UB  = (U2D%R(I)*U2D%R(I))+(V2D%R(I)*V2D%R(I))+WORB ! VEL
!             NOTE WE MULTIPLY EQ. (136A) OF SOULSBY (1997) THROUGH BY (UH)^{-1} TO GET DIMENSIONLESS
!              DEPTH-AVERAGED SATURATION CONCENTRATION CA:
          CSTAEQ%R(I)=ASS*(MAX(0.D0,(SQRT(UB)-UCR))**2.4D0)/HN%R(I) !MISSING ORB VELS
!              DUMP SATURATED CONC OUTPUT TO SUPPLEMENTARY VARIABLE A (PRIVATE ARRAY WITH INDEX 1):
          PRIVE%ADR(1)%P%R(I)=CSTAEQ%R(I)
          CSTAEQ%R(I)=CSTAEQ%R(I)*CSRATIO%R(I)
        ELSE
          CSTAEQ%R(I) = 0.D0
        ENDIF
      ENDDO
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE SUSPENSION_SANDFLOW
