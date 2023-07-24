!                   *******************
                    SUBROUTINE TOB_GAIA
!                   *******************
!
     & (TOB, TOBW, TOBCW_MEAN, TOBCW_MAX, THETAC, THETAW, MU,
     &  KS,KSP, KSR,CF,FW,UETCAR,CF_TEL,KS_TEL,CODE,
     &  ICR, KSPRATIO, HOULE,GRAV,XMVE,XMVS, VCE, KARMAN,
     &  ZERO,HMIN,HN, ACLADM, UNORM,UW, TW, NPOIN,KSCALC,IKS,
     &  DELTAR, H_TEL)
!
!***********************************************************************
! GAIA
!***********************************************************************
!
!>@brief Computes the total stress at the bottom
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in]     ACLADM     Mean diameter of active layer
!>@param[in,out] CF         Quadratic friction coefficient
!>@param[in]     CF_TEL     Quadratic friction coefficient (coupled t2d)
!>@param[in]     CODE       Calling program in coupling
!>@param[in]     DELTAR     Relative density = (rho-rho0)/rho0 (from t3d)
!>@param[in,out] FW         Quadratic friction coefficient (wave)
!>@param[in]     GRAV       Acceleration of gravity
!>@param[in]     HMIN       Minimum value of water depth
!>@param[in]     H_TEL      Water depth from tel h (n+1)
!>@param[in]     HN         Water depth
!>@param[in]     HOULE      Wave effect
!>@param[in]     ICR        Skin friction correction
!!                          Icr=0: mu=1
!!                          Icr=1: skin friction correction use ksp
!!                          Icr=2: ripple roughness use ksr, ksr
!>@param[in]     IKS        Bed roughness predictor option
!>@param[in]     KARMAN     Von karman constant
!>@param[in,out] KS         Total bed roughness
!>@param[in,out] KSP        Bed skin roughness
!>@param[in]     KSPRATIO   Ratio between skin bed roughness and grain diameter
!>@param[in]     KSCALC     Bed roughness prediction
!>@param[in,out] KSR        Ripple bed roughness
!>@param[in,out] KS_TEL     Total bed roughness sent to telemac
!>@param[in,out] MU         Correction factor for bed roughness
!>@param[in]     NPOIN      Number of points
!>@param[in]     THETAC     Current direction
!>@param[in]     THETAW     Waves direction (deg wrt ox axis)
!>@param[in,out] TOB        Bed shear stress (total friction)
!>@param[in,out] TOBW       Wave induced shear stress
!>@param[in,out] TOBCW_MEAN Wave + current mean shear stress
!>@param[in,out] TOBCW_MAX  Wave + current maximum shear stress
!>@param[in]     TW         Wave period wave velocity
!>@param[in]     UW         Wave orbital wave velocity
!>@param[in]     UETCAR     Square of the friction velocity (coupled t3d)
!>@param[in]     UNORM      Norm of the mean flow velocity
!>@param[in]     VCE        Water viscosity
!>@param[in]     XMVE       Fluid density (mass)
!>@param[in]     XMVS       Sediment density (mass)
!>@param[in]     ZERO       Zero
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA, ONLY: NSAND
      USE INTERFACE_GAIA, EX_TOB_GAIA=>TOB_GAIA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,            INTENT(IN)  :: NPOIN,ICR, IKS
      LOGICAL,            INTENT(IN)  :: KSCALC
      LOGICAL,            INTENT(IN)  :: HOULE
      CHARACTER(LEN=24),  INTENT(IN)  :: CODE
      DOUBLE PRECISION,   INTENT(IN)  :: XMVE,XMVS, VCE,GRAV,KARMAN
      DOUBLE PRECISION,   INTENT(IN)  :: ZERO,HMIN,KSPRATIO
      TYPE(BIEF_OBJ), INTENT(IN)      :: UETCAR
      TYPE(BIEF_OBJ), INTENT(IN)      :: DELTAR
      TYPE(BIEF_OBJ), INTENT(IN)      :: HN,UNORM
      TYPE(BIEF_OBJ), INTENT(IN)      :: TW,UW
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: KS,KSP,KSR,KS_TEL
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MU
      TYPE(BIEF_OBJ), INTENT(IN)      :: ACLADM
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: CF
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FW
      TYPE(BIEF_OBJ), INTENT(IN  )    :: THETAC, THETAW
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TOB,TOBW
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TOBCW_MEAN,TOBCW_MAX
      TYPE(BIEF_OBJ), INTENT(IN)      :: CF_TEL
      TYPE(BIEF_OBJ), INTENT(IN)      :: H_TEL
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                     :: I
      DOUBLE PRECISION            :: A,B,C, HCLIP,KSMAX
!
!     12.D0 WAS EXP(8.5*0.41)/EXP(1.D0)
!     11.036D0 IS EXP(8.5*0.40)/EXP(1.D0)
!     CONSIDERING THAT EXP(8.5*0.40) IS 30 INSTEAD OF 29.9641...
!     0.40 IS THE KARMAN CONSTANT THAT SHOULD BE PARAMETERISED SOMEWHERE
      DOUBLE PRECISION, PARAMETER :: CSTE=11.036D0
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ----------------------------------------------------------------------------------------------
!  QUADRATIC FRICTION COEFICIENT       :  ---> CF
!-----------------------------------------------------------------------
!
!     INTERNAL COUPLING WITH TELEMAC2D OR 3D
!               UETCAR IS CF IN TELEMAC-2D
!               UETCAR IS UETCAR IN TELEMAC3D ?
!  KSP : skin friction
!  KSR: ripple roughness
!  KS : total bed roughness
!  initialisation
!
      IF(NSAND.GT.0) THEN
        CALL OS('X=CY    ', X=KSP, Y=ACLADM, C=KSPRATIO)
        CALL OS('X=CY    ', X=KSR, Y=ACLADM, C=KSPRATIO)
!
        IF(KSCALC) THEN
!
!         bed roughness predictor
!
          CALL KS_GAIA(IKS,KS,KSP,KSR,KSPRATIO,HOULE,
     &                    GRAV,XMVE,XMVS,VCE,
     &                    HN,ACLADM,UNORM,UW,TW,NPOIN)
          CALL COEFRO_GAIA(CF,HN,KS,NPOIN,KARMAN)
          CALL OV('X=Y     ', X=KS_TEL%R, Y=KS%R, DIM1=NPOIN)
!
        ELSE
!
!         here the total bed roughness is calculated as a function of friction coefficient
!         issued from Telemac
!
          CALL OV('X=Y     ', X=CF%R, Y=CF_TEL%R, DIM1=CF%DIM1)
          DO I =1,NPOIN
            A = -KARMAN*SQRT(2.D0/MAX(CF%R(I),ZERO))
            KS%R(I)=CSTE*HN%R(I)*EXP(A)
            KS%R(I)=MAX(KS%R(I),KSP%R(I))
          ENDDO
!
        ENDIF
      ELSE
        CALL OV('X=Y     ', X=CF%R, Y=CF_TEL%R, DIM1=CF%DIM1)
      ENDIF
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!      CURRENT SHEAR STRESS
!  --> TOB
!
!     INTERNAL COUPLING WITH TELEMAC3D
!     UETCAR CORRESPONDS TO THE SQUARE OF THE FRICTION VELOCITY
!
      IF(CODE(1:9).EQ.'TELEMAC3D') THEN
        DO I=1,NPOIN
! TOB=0 IF H_TEL < HMIN  (NOT USE HN BECAUSE HN = HMIN IN THIS CASE)
          IF(H_TEL%R(I).GE.HMIN)THEN
            TOB%R(I)=(DELTAR%R(I)+1.D0)*XMVE*UETCAR%R(I)
          ELSE
            TOB%R(I) = 0.D0
          ENDIF
        ENDDO
      ELSE
        DO I=1,NPOIN
! TOB=0 IF H_TEL < HMIN  (NOT USE HN BECAUSE HN = HMIN IN THIS CASE)
          IF(H_TEL%R(I).GE.HMIN)THEN
            TOB%R(I) = XMVE*0.5D0*CF%R(I)*UNORM%R(I)**2
          ELSE
            TOB%R(I) = 0.D0
          ENDIF
        ENDDO
      ENDIF
!
! -----WAVE-SHEAR STRESS -----------------------------
!  --> TOBW
!
      IF(HOULE) THEN
        CALL TOBW_GAIA
     &    (TOBW%R,CF%R,FW%R,UW%R,TW%R,HN%R,NPOIN,XMVE)
          DO I=1,NPOIN
            IF(H_TEL%R(I).LT.HMIN)THEN
              TOBW%R(I) = 0.D0
            ENDIF
          ENDDO
! -----TOTAL CURRENT + WAVE-INDUCED SHEAR STRESS -----------
!  --> TOBCW_MEAN and TOBCW_MAX
        CALL TOBCW_GAIA
     &    (TOB%R,TOBW%R,THETAC%R,THETAW%R,
     &     TOBCW_MEAN%R,TOBCW_MAX%R,NPOIN)
      ELSE
        DO I=1,NPOIN
          TOBW%R(I)=0.D0
          TOBCW_MEAN%R(I)=TOB%R(I)
          TOBCW_MAX%R(I)=TOB%R(I)
        ENDDO
      ENDIF
!
!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! SKIN FRICTION CORRECTOR
!                ---> MU = TOP/TOB
! ICR=0:    MU=1
! ICR=1     : SKIN FRICTION CORRECTION USE KSP
! ICR= 2    : RIPPLE ROUGHNESS USE KSR, KSR
! COUPLED WITH TELEMAC: MU>1 IS ACCEPTABLE
!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      IF(ICR.EQ.0) THEN
        CALL OS('X=C     ', X=MU, C=1.D0)
      ELSEIF(ICR.EQ.1) THEN
        DO I= 1, NPOIN
          IF(CF%R(I).GT.ZERO.AND.HN%R(I).GT.KSP%R(I)) THEN
            HCLIP=MAX(HN%R(I),KSP%R(I))
            A = 2.5D0*LOG(CSTE*HCLIP/KSP%R(I))
            C =2.D0/A**2
            MU%R(I) = C/CF%R(I)
          ELSE
            MU%R(I) = 0.D0
          ENDIF
        ENDDO
      ELSEIF(ICR.EQ.2) THEN
        DO I= 1, NPOIN
          KSMAX=MAX(KSR%R(I),KSP%R(I))
          IF(HN%R(I).GT.KSMAX.AND.CF%R(I).GT.ZERO)THEN
            HCLIP=MAX(HN%R(I),KSMAX)
            A = LOG(CSTE*HCLIP/KSP%R(I))
            B = LOG(CSTE*HCLIP/KSR%R(I))
            C = 0.32D0/CF%R(I)
            MU%R(I) = C/SQRT(B*A**3)
          ELSE
            MU%R(I) = 0.D0
          ENDIF
        ENDDO
      ENDIF
!
!------------------------------------------------------------
!
      RETURN
      END
