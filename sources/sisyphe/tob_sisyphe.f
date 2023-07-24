!                   **********************
                    SUBROUTINE TOB_SISYPHE
!                   **********************
!
     & (TOB, TOBW, MU, KS,KSP, KSR,CF,FW,CHESTR,UETCAR,
     &  CF_TEL,KS_TEL,CODE,
     &  KFROT,ICR, KSPRATIO, HOULE,GRAV,XMVE,XMVS, VCE, KARMAN,
     &  ZERO,HMIN,HN, ACLADM, UNORM,UW, TW, NPOIN,KSPRED,IKS)
!
!***********************************************************************
! SISYPHE   V7P2
!***********************************************************************
!
!brief    COMPUTES THE TOTAL STRESS AT THE BOTTOM DEPENDING
!+                ON WHETHER SISYPHE IS COUPLED OR NOT.
!
!history  CV
!+        **/04/05
!+
!+   CORRECTION WHEN SISYPHE IS RUN ALONE: DO NOT MODIFY
!
!history  C. VILLARET (LNHE)
!+        29/11/06
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
!+  Name of variables.
!+
!
!history  J-M HERVOUET (EDF-LAB, LNHE), ON BEHALF OF CLEMENS DORFMAN
!+        12/02/2016
!+        V7P2
!+  Changing the constant 12.D0 into 11.036D0 because in the rest of the
!+  code the Karman constant has been changed from 0.41 to 0.40.
!
!history  RIADH ATA
!+        12/03/2018
!+        V8P0
!+  Change the call of OS by a call of OV since, when coupling, this
!+  subroutine can be called by vectors and not structures (arguments)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| CF_TEL         |-->| QUADRATIC FRICTION COEFFICIENT (COUPLED T2D)
!| CHESTR         |-->| FRICTION COEFFICIENT (KEYWORD)
!| CODE           |-->| CALLING PROGRAM IN COUPLING
!| FW             |-->| QUADRATIC FRICTION COEFFICIENT (WAVE)
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| HOULE          |-->| INCLUDE WAVES COMPUTATIONS
!| ICR            |-->| ICR=0: MU=1
!|                |   | ICR=1: SKIN FRICTION CORRECTION USE KSP
!|                |   | ICR=2: RIPPLE ROUGHNESS USE KSR, KSR
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KFROT          |-->| FRICTION LAW
!| KS             |<--| RUGOSITE TOTALE
!| KSP            |<--| RUGOSITE DE PEAU
!| KSPRATIO       |-->| RATIO BETWEEN SKIN BED ROUGHNESS AND GRAIN DIAMETER
!| KSR            |<--| RUGOSITE DE RIDE
!| KS_TEL         |<--| RUGOSITE TOTALE
!| MU             |<->| CORRECTION FACTOR FOR BED ROUGHNESS
!| NPOIN          |-->| NUMBER OF POINTS
!| TOB            |<->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOBW           |-->| WAVE INDUCED SHEAR STRESS
!| TW,UW          |-->| WAVE PERIOD AND ORBITAL VELOCITY
!| UETCAR         |-->| SQUARE OF THE FRICTION VELOCITY (COUPLED T3D)
!| UNORM          |-->| INTENSITE DU COURANT
!| VCE            |-->| WATER VISCOSITY
!| XMVE           |-->| FLUID DENSITY (MASS)
!| XMVS           |-->| SEDIMENT DENSITY (MASS)
!| ZERO           |-->| ZERO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_TOB_SISYPHE=>TOB_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,            INTENT(IN)  :: NPOIN,KFROT,ICR, IKS
      LOGICAL,            INTENT(IN)  :: KSPRED
      LOGICAL,            INTENT(IN)  :: HOULE
      CHARACTER(LEN=24),  INTENT(IN)  :: CODE
      DOUBLE PRECISION,   INTENT(IN)  :: XMVE,XMVS, VCE,GRAV,KARMAN
      DOUBLE PRECISION,   INTENT(IN)  :: ZERO,HMIN,KSPRATIO
      TYPE(BIEF_OBJ), INTENT(IN)      :: UETCAR
      TYPE(BIEF_OBJ), INTENT(IN)      :: HN,UNORM
      TYPE(BIEF_OBJ), INTENT(IN)      :: TW,UW
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: KS,KSP,KSR,KS_TEL
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: CHESTR,MU
      TYPE(BIEF_OBJ), INTENT(IN)      :: ACLADM
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: CF,TOB
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FW,TOBW
      TYPE(BIEF_OBJ), INTENT(IN)      :: CF_TEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
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
      CALL OS('X=CY    ', X=KSP, Y=ACLADM, C=KSPRATIO)
      CALL OS('X=CY    ', X=KSR, Y=ACLADM, C=KSPRATIO)
!
      IF(KSPRED) THEN
!
!       bed roughness predictor
!
        CALL KS_SISYPHE(IKS,KS,KSP,KSR,KSPRATIO,HOULE,
     &                  GRAV,XMVE,XMVS,VCE,
     &                  HN,ACLADM,UNORM,UW,TW,NPOIN)
        CALL COEFRO_SISYPHE(CF,HN,KFROT,KS,GRAV,NPOIN,HMIN,KARMAN)
        IF(CODE(1:7).EQ.'TELEMAC')
     &   CALL OV( 'X=Y     ', KS_TEL%R, KS%R, KS%R, 0.D0, NPOIN)
!
      ELSE
!
! here the total bed roughness is calculated as a function of friction coefficient
! -- > issued from Telemac if coupling
! -- > from the steering file of Sisyphe
!
        IF(CODE(1:7).EQ.'TELEMAC') THEN
          CALL OV('X=Y     ', X=CF%R, Y=CF_TEL%R, DIM1=CF%DIM1)
        ELSE
          CALL COEFRO_SISYPHE(CF,HN,KFROT,CHESTR,GRAV,NPOIN,HMIN,KARMAN)
        ENDIF
        DO I =1,NPOIN
          A = -KARMAN*SQRT(2.D0/MAX(CF%R(I),ZERO))
          KS%R(I)=CSTE*HN%R(I)*EXP(A)
          KS%R(I)=MAX(KS%R(I),KSP%R(I))
        ENDDO
!
      ENDIF
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Frottement total: loi quadratique sauf couplage 3D
!  --> TOB
!
!     INTERNAL COUPLING WITH TELEMAC3D
!     UETCAR CORRESPONDS TO THE FRICTION VELOCITY SQUARED
!
      IF(CODE(1:9).EQ.'TELEMAC3D') THEN
        CALL OS( 'X=CY     ',X=TOB,Y=UETCAR,C=XMVE)
      ELSE
        DO I=1,NPOIN
          TOB%R(I) = XMVE*0.5D0*CF%R(I)*UNORM%R(I)**2
        ENDDO
      ENDIF
!
! -----WAVE-INDUCED FRICTION -----------------------------
!  --> TOBW
!
      IF(HOULE) THEN
        CALL TOBW_SISYPHE(TOBW%R,CF%R,FW%R,UW%R,TW%R,HN%R,NPOIN,XMVE)
      ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! SKIN FRICTION CORRECTOR
!                ---> MU = TOP/TOB
! ICR=0:    MU=1
! ICR=1     : SKIN FRICTION CORRECTION USE KSP
! ICR= 2    : RIPPLE ROUGHNESS USE KSR, KSR
! COUPLED WITH TELEMAC: MU>1 IS ACCEPTABLE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
