!                   ***************
                    SUBROUTINE RIDE
!                   ***************
!
     &(KS,TW,UW,UNORM,GRAV,XMVE,XMVS,VCE,NPOIN,KSPRATIO,ACLADM)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE DIMENSIONS OF EQUILIBRIUM RIPPLES.
!
!reference  "RIPPLE GEOMETRY IN WAVE-DOMINATED ENVIRONMENTS",
!+              WIBERG, P.L. & C.K. HARRIS. 1994. JOURNAL OF
!+              GEOPHYSICAL RESEARCH, 99 (C1): 775-789.
!
!history  C. VILLARET (LNHE)
!+        01/10/2003
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| KS             |<--| WAVE INDUCE RIPPLE ROUGHNESS
!| KSPRATIO       |-->| RATIO BETWEEN SKIN BED ROUGHESS AND GRAIN SIZE
!| NPOIN          |-->| NUMBER OF POINTS
!| TW             |-->| WAVE PERIOD
!| UNORM          |-->| NORM OF THE MEAN FLOW VELOCITY
!| UW             |-->| ORBITAL WAVE VELOCITY
!| VCE            |-->| WATER VISCOSITY
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
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
!         LINE OF CODE MOVED SO ALPHA COMPUTED BEFORE VAR1
!         TANNAKA AND DANG (1996)
          UC=UNORM(I)
          IF(UW(I).GT.ZERO) THEN
            ALPHA=(TANH(0.3D0*S**(2.D0/3.D0)))**2.5D0
            ALPHA=1.D0+0.81D0*ALPHA*(UC/UW(I))**1.9D0
          ELSE
            ALPHA=1.D0
          ENDIF
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
