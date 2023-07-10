!                   ********************
                    SUBROUTINE RIDE_GAIA
!                   ********************
!
     &(KS,TW,UW,UNORM,GRAV,XMVE,XMVS,VCE,NPOIN,KSPRATIO,ACLADM)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the dimensions of equilibrium ripples.
!
!>@reference  "Ripple geometry in wave-dominated environments",
!!              WIBERG, P.L. & C.K. HARRIS. 1994. Journal of
!!              geophysical research, 99 (C1): 775-789.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     ACLADM   Mean diameter of active layer
!>@param[in]     GRAV     Acceleration of gravity
!>@param[in,out] KS       Wave induce ripple roughness
!>@param[in]     KSPRATIO Ratio between skin bed roughess and grain size
!>@param[in]     NPOIN    Number of points
!>@param[in]     TW       Wave period
!>@param[in]     UNORM    Norm of the mean flow velocity
!>@param[in]     UW       Orbital wave velocity
!>@param[in]     VCE      Water viscosity
!>@param[in]     XMVE     Fluid density
!>@param[in]     XMVS     Sediment density
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER I,NPOIN
      DOUBLE PRECISION, INTENT (INOUT) :: KS(NPOIN)
!
      DOUBLE PRECISION, INTENT (IN) :: GRAV,XMVE,XMVS, VCE
      DOUBLE PRECISION, INTENT (IN) :: UNORM(NPOIN),UW(NPOIN),TW(NPOIN)
!
      DOUBLE PRECISION, INTENT(IN) :: KSPRATIO
      DOUBLE PRECISION, INTENT(IN) :: ACLADM(NPOIN)
!
!---------------------------------------------------------------------
! LOCAL VARIABLES
      DOUBLE PRECISION PI, ZERO,AI
!
      DOUBLE PRECISION ETA, LAMBDA
!
      DOUBLE PRECISION AA,BB,CC,DD
      DOUBLE PRECISION ALPHA,S,M,A0
      DOUBLE PRECISION WH1,WH2,WH3
      DOUBLE PRECISION VAR1,DHRA,LRA,HRA,LRO
      DOUBLE PRECISION UC,KSP
!---------------------------------------------------------------------
!
      PI=4.D0*ATAN(1.D0)
      ZERO=1.D-6
!
!     COEFFICIENTS
!
      WH1=0.095D0
      WH2=0.442D0
      WH3=2.28D0
      AA=(WH2+1.D0)/2.D0/WH1
      BB=AA*AA-WH3/WH1
      CC=1.D0/WH1
!
!     LOOP ON THE NODES
!
      DO I=1,NPOIN
!
!       SKIN FRICTION
!
        KSP = KSPRATIO * ACLADM(I)
        AI  = ACLADM(I)*GRAV*(XMVS-XMVE)/XMVE
!
!       MOBILITY NUMBER
!
        M=UW(I)**2/AI
!
        IF(M.LT.1.69D0) THEN
!
          KS(I)=KSP
!
        ELSE
!
!         WIBERG AND HARRIS
!
          A0=UW(I)*TW(I)/(2.D0*PI)
          S=ACLADM(I)*SQRT(AI)/4.D0/VCE
          LRA=535.D0*ACLADM(I)
!JMB***************************************************
!JMB LINE OF CODE MOVED SO ALPHA COMPUTED BEFORE VAR1
!JMB TANNAKA AND DANG (1996)
          UC=UNORM(I)
          IF(UW(I).GT.ZERO) THEN
            ALPHA=(TANH(0.3D0*S**(2.D0/3.D0)))**2.5D0
            ALPHA=1.D0+0.81D0*ALPHA*(UC/UW(I))**1.9D0
          ELSE
            ALPHA=1.D0
          ENDIF
!JMB*******************************************************
          VAR1=LOG(ALPHA*2.D0*A0/LRA)
          DD=MAX((BB-CC*VAR1),0.D0)
          DHRA=EXP(AA-SQRT(DD))
          HRA=ALPHA*2.D0*A0/DHRA
!
          IF(DHRA.LE.20.D0) THEN
!           ORBITAL RIPPLES DHRA
            LRO=0.62D0*2.D0*A0*ALPHA
            LAMBDA=LRO
            ETA=0.17D0*LAMBDA
          ELSEIF(DHRA.LE.100.D0) THEN
!           SUB ORBITAL RIPPLES 20
            LRO=0.62D0*2.D0*A0*ALPHA
            VAR1=(LOG(DHRA)-LOG(100.D0))/(LOG(20.D0)-LOG(100.D0))
            VAR1=LOG(LRA)+VAR1*(LOG(LRO)-LOG(LRA))
            LAMBDA=EXP(VAR1)
            VAR1=LOG(ALPHA*2.D0*A0/LAMBDA)
!CV 25/05               ETA=ALPHA*2.D0*A0/EXP(AA-SQRT(BB-CC*VAR1))
            DD=MAX((BB-CC*VAR1),0.D0)
            ETA=ALPHA*2.D0*A0/EXP(AA-SQRT(DD))
          ELSE
!           ANORBITAL RIPPLES DHRA>100
!           LAMBDA NOT USED HERE BUT KEPT FOR OTHER FORMULATIONS
!           LAMBDA=LRA
            ETA=HRA
          ENDIF
!
          KS(I)=MAX(ETA,KSP)
!
        ENDIF
!
      ENDDO
!
!---------------------------------------------------------------------
!
      RETURN
      END
