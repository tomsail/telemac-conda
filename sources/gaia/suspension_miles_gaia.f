!                   ********************************
                    SUBROUTINE SUSPENSION_MILES_GAIA
!                   ********************************
!
     &(HN,NPOIN,HMIN,FDM,FD90,XWC,CSRATIO)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the bed exchange factor beta based on Miles (1986)
!found in HR Wallingford report: SR75.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param [in]     HN        Water depth
!>@param [in]     NPOIN     Number of points
!>@param [in]     HMIN      Minimal water depth to consider erosion
!>@param [in]     FDM       Grain size (D50)
!>@param [in]     FD90      Grain size (D90)
!>@param [in]     XWC       Settling velocity
!>@param [in,out] CSRATIO   Ratio between bottom concentration and
!                           average concentration
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA, ONLY: KS,U2D,V2D,DT
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HN
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: FDM,FD90,XWC,HMIN
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSRATIO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I
      DOUBLE PRECISION :: UB,USTARS,RB,BETAS
      DOUBLE PRECISION :: SFBETA(NPOIN),AUX,RRTPI
      DOUBLE PRECISION :: FVINV
      DOUBLE PRECISION :: DZ,TAU,TAU_SQ,UCR
!
      DOUBLE PRECISION BIEF_ERF
      EXTERNAL         BIEF_ERF
!
      INTRINSIC SQRT,EXP
!     INTRINSIC ERF
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     COMPUTE CONSTANTS:
!
      DOUBLE PRECISION PI
      PI = 4.D0 * ATAN( 1.D0 )
      RRTPI = 1.D0/SQRT(PI)
      FVINV = 1.D0/XWC**2
!
!     DMK MOD 03/05/2011
!
!     MODIFYING SUSPENDED LOAD TO BE LIKE SANDFLOW AND ALSO TO TAKE
!     LAG INTO CONSIDERATION VIA A BETA COMPUTATION.
!
      DO I=1,NPOIN
!
        IF(FDM.LT.500.E-6) THEN
!         UCR = 0.19D0*(D50**0.1D0)*LOG10(4.D0*(HN%R(I)/FDM))    ! SANDFLOW
          UCR = 0.19D0*(FDM**0.1D0)*LOG10(4.D0*HN%R(I)/FD90)  ! CORRECT
        ELSE
!         UCR = 8.5D0*(D50**0.6D0)*LOG10(4.D0*(HN%R(I)/D50))      ! SANDFLOW
          UCR = 8.5D0*(FDM**0.6D0)*LOG10(4.D0*HN%R(I)/FD90)    ! CORRECT
        ENDIF
!
        UB = SQRT(U2D%R(I)**2+V2D%R(I)**2)
!
        IF(HN%R(I).GT.HMIN.AND.UB.GT.UCR) THEN ! STOP PROBLEMS WITH LOW VELOCITIES
          USTARS = 1.3D0*UB*SQRT(KS%R(I)/8.D0)
          RB     = XWC*15.D0/USTARS
!         THIS IS DIFFERENT BETWEEN MILES’ PAPERS DUE TO TYPO IN DEF OF R
          BETAS  = RB/(1.D0-EXP(-RB))
!         ACTUAL EQUATION FROM MILES (1981)  [1]
          DZ     = (1.D0/6.D0)*0.4D0*HN%R(I)*USTARS
!         USE WITH [1]
          TAU    = XWC*SQRT(DT/(4.D0*DZ))
          TAU_SQ = TAU**2
!         EQ. (27) NOTE THIS IS ALREADY INTEGRATED WITH RESPECT TO TIME (DTS)
!
!         IF COMPILER ALOWS...
!         AUX=ERF(TAU)
!         AND IF NOT...
          AUX=BIEF_ERF(TAU)
!
          SFBETA(I)=
     &      DZ*FVINV*BETAS*(4.D0*TAU_SQ*(1.D0+TAU_SQ)*(1.D0-AUX)
     &    + AUX - 2.D0*TAU*(1.D0+2.D0*TAU_SQ)*EXP(-TAU_SQ)*RRTPI)
!
          IF(SFBETA(I)*XWC/HN%R(I).GT.1.D0) SFBETA(I)=1.D0/(XWC*HN%R(I))
!         DIVIDE BACK THROUGH BY DT AS WE WILL INTEGRATE UP  WRTT LATER
          CSRATIO%R(I)=SFBETA(I)/DT ! FOR USE WITH EQ. (27)
!         NOTE WE RECORD BETA_S (PROFILE PARAMETER) I.E. THE RATIO OF REF LEVEL CONC
!         TO DEPTH AVERAGED CONC THIS WILL THEN BE USED IN SUSPENSION_SANDFLOW.F:
!         NB: STORED IN T14
        ELSE
          SFBETA(I) = 1.D0
          CSRATIO%R(I) = 1.D0
        ENDIF
!
      ENDDO
!
!     END BETA FACTOR COMPUTATION
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
