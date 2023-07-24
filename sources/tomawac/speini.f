!                   *****************
                    SUBROUTINE SPEINI
!                   *****************
!
     &( F     , SPEC  , FRA   , UV    , VV    , FREMAX, FETCH , SIGMAA,
     &  SIGMAB, GAMMA , FPIC  , HM0   , ALPHIL, TETA1 , SPRED1, TETA2 ,
     &  SPRED2, XLAMDA, NPOIN2, NDIRE , NF    , INISPE, DEPTH ,
     &  FRABI )
!
!***********************************************************************
! TOMAWAC   V6P1                                   28/06/2011
!***********************************************************************
!
!brief    INITIALISES THE VARIANCE SPECTRUM.
!+
!+            SEVERAL OPTIONS ARE POSSIBLE DEPENDING ON THE VALUE
!+                TAKEN BY INISPE :
!+
!+            0. ZERO EVERYWHERE
!+
!+            1. JONSWAP-TYPE SPECTRUM AS A FUNCTION OF THE WIND
!+                  (ZERO IF WIND SPEED IS ZERO)
!+
!+            2. JONSWAP-TYPE SPECTRUM AS A FUNCTION OF THE WIND
!+                  (PARAMETRIC IF WIND SPEED IS ZERO)
!+
!+            3. PARAMETRIC JONSWAP-TYPE SPECTRUM
!
!history  M. BENOIT (EDF/DER/LNH)
!+        13/07/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        28/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history T FOUQUET (LNHE)
!+       28/10/2015
!+       V7P0
!+       Modification to initialise spectrum wind is small or null
!+       and speini =1,3,5
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALPHIL         |-->| INITIAL PHILLIPS CONSTANT (ALPHA)
!| DEPTH          |-->| WATER DEPTH
!| F              |<--| VARIANCE DENSITY DIRECTIONAL SPETCRUM
!| FETCH          |-->| INITIAL MEAN FETCH VALUE
!| FPIC           |-->| INITIAL PEAK FREQUENCY
!| FRA            |<--| DIRECTIONAL SPREADING FUNCTION VALUES
!| FRABI          |-->| INITIAL ANGULAR DISTRIBUTION FUNCTION
!| FREMAX         |-->| INITIAL MAXIMUM PEAK FREQUENCY
!| GAMMA          |-->| INITIAL JONSWAP SPECTRUM PEAK FACTOR
!| HM0            |-->| INITIAL SIGNIFICANT WAVE HEIGHT
!| INISPE         |-->| TYPE OF INITIAL DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SIGMAA         |-->| INITIAL VALUE OF SIGMA FOR JONSWAP SPECTRUM
!|                |   | (F<FP)
!| SIGMAB         |-->| INITIAL VALUE OF SIGMA FOR JONSWAP SPECTRUM
!|                |   | (F>FP)
!| SPEC           |<--| VARIANCE DENSITY FREQUENCY SPECTRUM
!| SPRED1         |-->| INITIAL DIRECTIONAL SPREAD 1
!| SPRED2         |-->| INITIAL DIRECTIONAL SPREAD 2
!| TETA1          |-->| MAIN DIRECTION 1
!| TETA2          |-->| MAIN DIRECTION 2
!| UV             |-->| WIND VELOCITY ALONG X AT THE MESH POINTS
!| VV             |-->| WIND VELOCITY ALONG Y AT THE MESH POINTS
!| XLAMDA         |-->| WEIGHTING FACTOR FOR FRA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI, VENT, GRAVIT, FREQ, TETA,
     & SMAX
      USE INTERFACE_TOMAWAC, EX_SPEINI => SPEINI
      USE BIEF, ONLY: OV
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    ::  NPOIN2, NDIRE , NF    , INISPE, FRABI
      DOUBLE PRECISION, INTENT(IN)    :: FREMAX, FETCH , SIGMAA
      DOUBLE PRECISION, INTENT(IN)    :: SIGMAB, GAMMA
      DOUBLE PRECISION, INTENT(IN)    :: FPIC  , HM0   , ALPHIL, TETA1
      DOUBLE PRECISION, INTENT(IN)    :: SPRED1, TETA2
      DOUBLE PRECISION, INTENT(IN)    :: SPRED2, XLAMDA
      DOUBLE PRECISION, INTENT(IN)    :: UV(*) , VV(*)
      DOUBLE PRECISION, INTENT(IN)    :: DEPTH(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FRA(NDIRE), SPEC(NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  NPOIN4, IP    , JF    , JP
      DOUBLE PRECISION GX    , GXU   , UG    , AL    , FP
      DOUBLE PRECISION UVMIN , COEFA , COEFB , COEFD
      DOUBLE PRECISION COEFE , UVENT , FPMIN , SPR1  , SPR2  , XLAM
      DOUBLE PRECISION TET1  , TET2  , COEF
!     VARIABLE FOR GODA SPREADING DEPENDING ON FREQUENCY
      DOUBLE PRECISION COEF1, DELT, ARGUM, DTETA
!      DOUBLE PRECISION SMAX
!      SMAX=35
!
      NPOIN4= NPOIN2*NDIRE*NF
      UVMIN = 1.D-6
      COEFA = 2.84D0
      COEFB = 0.033D0
      COEFD =-3.D0/10.D0
      COEFE = 2.D0/3.D0
      GX    = GRAVIT*FETCH
      FPMIN = 1.D-4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Smax recomendations (GODA): 10(Wind waves), 25 swell short decay, 75 long decay
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     ===========================================================
!     INITIAL SPECTRUM IS ZERO EVERYWHERE (INISPE=0)
!     (ALSO WORKS TO INITIALISE TO OTHER VALUES)
!     ===========================================================
!
      IF(INISPE.EQ.0) THEN
!
        CALL OV('X=0     ', X=F, DIM1=NPOIN4)
!
!     ==/ INISPE = 1 /===========================================
!     IF NON ZERO WIND -E(F): JONSWAP FUNCTION OF THE WIND (AL,FP)
!                      -FRA : UNIMODAL ABOUT TETA(WIND)
!     IF ZERO WIND     -E(F): ZERO
!                      -FRA : ZERO
!     ===========================================================
!
      ELSEIF(INISPE.EQ.1) THEN
!
        UVENT =0
        DO IP=1,NPOIN2
          IF(VENT) UVENT=SQRT(UV(IP)**2+VV(IP)**2)
          IF (UVENT.GT.UVMIN) THEN
!
!           COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!
            GXU=GX/(UVENT*UVENT)
            UG = UVENT/GRAVIT
            FP = MAX(0.13D0,COEFA*GXU**COEFD)
            FP = MIN(FP,FREMAX*UG)
            AL = MAX(0.0081D0, COEFB*FP**COEFE)
            FP = FP/UG
            CALL SPEJON
     &( SPEC  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  FPMIN )
!
!           COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!
            SPR1=SPRED1
            TET1=ATAN2(UV(IP),VV(IP))
            SPR2=1.D0
            TET2=0.D0
            XLAM=1.D0
            IF(FRABI.EQ.2) THEN
              CALL FSPRD2(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
            ELSEIF(FRABI.EQ.3) THEN
              CALL FSPRD3(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
            ELSEIF(FRABI.EQ.1) THEN
              CALL FSPRD1(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
            ENDIF
!
!           COMPUTES THE DIRECTIONAL SPECTRUM
!
            IF (FRABI.LE.3) THEN
              DO JF=1,NF
                DO JP=1,NDIRE
                  F(IP,JP,JF)=SPEC(JF)*FRA(JP)
                ENDDO
              ENDDO
            ELSEIF(FRABI.EQ.4) THEN
! DIRECTION DEPENDS ON FREQUENCY
              DO JF=1,NF
                IF(FREQ(JF).LT.FPIC) THEN
                  COEF1=SMAX*(FREQ(JF)/FPIC)**(5.0D0)
                  DELT=0.5D0/DELFRA(COEF1)
                ELSE
                  COEF1=SMAX*(FREQ(JF)/FPIC)**(-2.5D0)
                  DELT = 0.5D0/DELFRA(COEF1)
                ENDIF
                DO JP=1,NDIRE
                  DTETA = TETA(JP)-TETA1
                  ARGUM = ABS(COS(0.5D0*(DTETA)))
                  FRA(JP)=DELT*ARGUM**(2.D0*COEF1)
                  F(IP,JP,JF)=SPEC(JF)*FRA(JP)
                ENDDO
              ENDDO
            ELSE
              WRITE(LU,*)'WRONG VALUE FOR ANGULAR DISTRIBUTION FUNCTION'
              CALL PLANTE(1)
            ENDIF
          ELSE
            DO JF=1,NF
              DO JP=1,NDIRE
                F(IP,JP,JF)=0.D0
              ENDDO
            ENDDO
          ENDIF
!
        ENDDO ! IP
!
!     ==/ INISPE = 2 /===========================================
!     IF NON ZERO WIND -E(F): JONSWAP AS A FUNCTION OF THE WIND (AL,FP)
!                      -FRA : UNIMODAL ABOUT TETA(WIND)
!     IF ZERO WIND     -E(F): PARAMETERISED JONSWAP (AL,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     ===========================================================
!
      ELSEIF (INISPE.EQ.2) THEN
!
        UVENT = 0
        DO IP=1,NPOIN2
          IF(VENT) UVENT=SQRT(UV(IP)**2+VV(IP)**2)
!
!         COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!
          IF(UVENT.GT.UVMIN) THEN
            GXU=GX/UVENT**2
            UG = UVENT/GRAVIT
            FP = MAX(0.13D0,COEFA*GXU**COEFD)
            FP = MIN(FP,FREMAX*UG)
            AL = MAX(0.0081D0, COEFB*FP**COEFE)
            FP = FP/UG
          ELSE
            AL=ALPHIL
            FP=FPIC
          ENDIF
          CALL SPEJON
     &( SPEC  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  FPMIN )
!
!         COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!
          IF (UVENT.GT.UVMIN) THEN
            TET1=ATAN2(UV(IP),VV(IP))
          ELSE
            TET1=TETA1
          ENDIF
          SPR1=SPRED1
          SPR2=1.D0
          TET2=0.D0
          XLAM=1.D0
          IF(FRABI.EQ.2) THEN
            CALL FSPRD2(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ELSEIF(FRABI.EQ.3) THEN
            CALL FSPRD3(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ELSEIF(FRABI.EQ.1) THEN
            CALL FSPRD1(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ENDIF
!
!         COMPUTES THE DIRECTIONAL SPECTRUM
!
          IF(FRABI.LE.3) THEN
            DO JF=1,NF
              DO JP=1,NDIRE
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
              ENDDO
            ENDDO
          ELSEIF(FRABI.EQ.4) THEN
! DIRECTION DEPENDS ON FREQUENCY
            DO JF=1,NF
              IF(FREQ(JF).LT.FPIC) THEN
                COEF1=SMAX*(FREQ(JF)/FPIC)**(5.0D0)
                DELT=0.5D0/DELFRA(COEF1)
              ELSE
                COEF1=SMAX*(FREQ(JF)/FPIC)**(-2.5D0)
                DELT = 0.5D0/DELFRA(COEF1)
              ENDIF
              DO JP=1,NDIRE
                DTETA = TETA(JP)-TETA1
                ARGUM = ABS(COS(0.5D0*(DTETA)))
                FRA(JP)=DELT*ARGUM**(2.D0*COEF1)
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
              ENDDO
            ENDDO
          ELSE
            WRITE(LU,*)'WRONG VALUE FOR ANGULAR DISTRIBUTION FUNCTION'
            CALL PLANTE(1)
          ENDIF
!
        ENDDO ! IP
!
!     ==/ INISPE = 3 /===========================================
!     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (AL,FP)
!                      -FRA : UNIMODAL ABOUT TETA(WIND)
!     IF ZERO WIND     -E(F): ZERO
!                      -FRA : ZERO
!     ===========================================================
!
      ELSEIF (INISPE.EQ.3) THEN
!
        UVENT=0
        DO IP=1,NPOIN2
          IF (VENT) UVENT=SQRT(UV(IP)**2+VV(IP)**2)
          IF(UVENT.GT.UVMIN) THEN
!
!...........COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!           """""""""""""""""""""""""""""""""""""""""
            AL = ALPHIL
            FP = FPIC
            CALL SPEJON
     &( SPEC  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  FPMIN )
!
!           COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!
            SPR1=SPRED1
            TET1=ATAN2(UV(IP),VV(IP))
            SPR2=1.D0
            TET2=0.D0
            XLAM=1.D0
            IF(FRABI.EQ.2) THEN
              CALL FSPRD2(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
            ELSEIF(FRABI.EQ.3) THEN
              CALL FSPRD3(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
            ELSEIF(FRABI.EQ.1) THEN
              CALL FSPRD1(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
            ENDIF
!
!           COMPUTES THE DIRECTIONAL SPECTRUM
!
            IF(FRABI.LE.3) THEN
              DO JF=1,NF
                DO JP=1,NDIRE
                  F(IP,JP,JF)=SPEC(JF)*FRA(JP)
                ENDDO
              ENDDO
            ELSEIF(FRABI.EQ.4) THEN
! DIRECTION DEPENDS ON FREQUENCY
              DO JF=1,NF
                IF(FREQ(JF).LT.FPIC) THEN
                  COEF1=SMAX*(FREQ(JF)/FPIC)**(5.0D0)
                  DELT=0.5D0/DELFRA(COEF1)
                ELSE
                  COEF1=SMAX*(FREQ(JF)/FPIC)**(-2.5D0)
                  DELT = 0.5D0/DELFRA(COEF1)
                ENDIF
                DO JP=1,NDIRE
                  DTETA = TETA(JP)-TETA1
                  ARGUM = ABS(COS(0.5D0*(DTETA)))
                  FRA(JP)=DELT*ARGUM**(2.D0*COEF1)
                  F(IP,JP,JF)=SPEC(JF)*FRA(JP)
                ENDDO
              ENDDO
            ELSE
              WRITE(LU,*)'WRONG VALUE FOR ANGULAR DISTRIBUTION FUNCTION'
              CALL PLANTE(1)
            ENDIF
          ELSE
            DO JF=1,NF
              DO JP=1,NDIRE
                F(IP,JP,JF)=0.D0
              ENDDO
            ENDDO
          ENDIF
!
        ENDDO ! IP
!
!     ==/ INISPE = 4 /===========================================
!     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (AL,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     IF ZERO WIND     -E(F): PARAMETERISED JONSWAP (AL,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     ===========================================================
!
      ELSEIF (INISPE.EQ.4) THEN
!
        DO IP=1,NPOIN2
!
!         COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!
          AL = ALPHIL
          FP = FPIC
          CALL SPEJON
     &( SPEC  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  FPMIN )
!
!         COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!
          SPR1=SPRED1
          TET1=TETA1
          SPR2=SPRED2
          TET2=TETA2
          XLAM=XLAMDA
          IF(FRABI.EQ.2) THEN
            CALL FSPRD2(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ELSEIF(FRABI.EQ.3) THEN
            CALL FSPRD3(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ELSEIF(FRABI.EQ.1) THEN
            CALL FSPRD1(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ENDIF
!
!         COMPUTES THE DIRECTIONAL SPECTRUM
!

          IF(FRABI.LE.3) THEN
            DO JF=1,NF
              DO JP=1,NDIRE
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
              ENDDO
            ENDDO
          ELSEIF(FRABI.EQ.4) THEN
! DIRECTION DEPENDS ON FREQUENCY
            DO JF=1,NF
              IF(FREQ(JF).LT.FPIC) THEN
                COEF1=SMAX*(FREQ(JF)/FPIC)**(5.0D0)
                DELT=0.5D0/DELFRA(COEF1)
              ELSE
                COEF1=SMAX*(FREQ(JF)/FPIC)**(-2.5D0)
                DELT = 0.5D0/DELFRA(COEF1)
              ENDIF
              DO JP=1,NDIRE
                DTETA = TETA(JP)-TETA1
                ARGUM = ABS(COS(0.5D0*(DTETA)))
                FRA(JP)=DELT*ARGUM**(2.D0*COEF1)
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
              ENDDO
            ENDDO
          ELSE
            WRITE(LU,*)'WRONG VALUE FOR ANGULAR DISTRIBUTION FUNCTION'
            CALL PLANTE(1)
          ENDIF
!
        ENDDO ! IP
!
!     ==/ INISPE = 5 /===========================================
!     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (HM0,FP)
!                      -FRA : UNIMODAL ABOUT TETA(WIND)
!     IF ZERO WIND     -E(F): ZERO
!                      -FRA : ZERO
!     ===========================================================
!
      ELSEIF (INISPE.EQ.5) THEN
!
        COEF=0.0624D0/(0.230D0+0.0336D0*GAMMA-0.185D0/(1.9D0+GAMMA))
     &      *(DEUPI*FPIC)**4*HM0**2/GRAVIT**2
!
        UVENT=0
        DO IP=1,NPOIN2
          IF (VENT) UVENT=SQRT(UV(IP)**2+VV(IP)**2)
          IF (UVENT.GT.UVMIN) THEN
!
!           COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!
            AL=COEF
            FP = FPIC
            CALL SPEJON
     &( SPEC  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  FPMIN )
!
!           COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!
            SPR1=SPRED1
            TET1=ATAN2(UV(IP),VV(IP))
            SPR2=1.D0
            TET2=0.D0
            XLAM=1.D0
            IF(FRABI.EQ.2) THEN
              CALL FSPRD2(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
            ELSEIF(FRABI.EQ.3) THEN
              CALL FSPRD3(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
            ELSEIF(FRABI.EQ.1) THEN
              CALL FSPRD1(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
            ENDIF
!
!           COMPUTES THE DIRECTIONAL SPECTRUM
!
            IF(FRABI.LE.3) THEN
              DO JF=1,NF
                DO JP=1,NDIRE
                  F(IP,JP,JF)=SPEC(JF)*FRA(JP)
                ENDDO
              ENDDO
            ELSEIF(FRABI.EQ.4) THEN
! DIRECTION DEPENDS ON FREQUENCY
              DO JF=1,NF
                IF(FREQ(JF).LT.FPIC) THEN
                  COEF1=SMAX*(FREQ(JF)/FPIC)**(5.0D0)
                  DELT=0.5D0/DELFRA(COEF1)
                ELSE
                  COEF1=SMAX*(FREQ(JF)/FPIC)**(-2.5D0)
                  DELT = 0.5D0/DELFRA(COEF1)
                ENDIF
                DO JP=1,NDIRE
                  DTETA = TETA(JP)-TETA1
                  ARGUM = ABS(COS(0.5D0*(DTETA)))
                  FRA(JP)=DELT*ARGUM**(2.D0*COEF1)
                  F(IP,JP,JF)=SPEC(JF)*FRA(JP)
                ENDDO
              ENDDO
            ELSE
              WRITE(LU,*)'WRONG VALUE FOR ANGULAR DISTRIBUTION FUNCTION'
              CALL PLANTE(1)
            ENDIF
          ELSE
            DO JF=1,NF
              DO JP=1,NDIRE
                F(IP,JP,JF)=0.D0
              ENDDO
            ENDDO
          ENDIF
!
        ENDDO ! IP
!
!     ==/ INISPE = 6 /===========================================
!     IF NON ZERO WIND -E(F): PARAMETERISED JONSWAP (HM0,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     IF ZERO WIND     -E(F): PARAMETERISED JONSWAP (HM0,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     ===========================================================
!
      ELSEIF (INISPE.EQ.6) THEN
!
        COEF=0.0624D0/(0.230D0+0.0336D0*GAMMA-0.185D0/(1.9D0+GAMMA))
     &      *(DEUPI*FPIC)**4*HM0**2/GRAVIT**2
!
        DO IP=1,NPOIN2
!
!         COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!
          AL = COEF
          FP = FPIC
          CALL SPEJON
     &( SPEC  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  FPMIN )
!
!         COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!
          SPR1=SPRED1
          TET1=TETA1
          SPR2=SPRED2
          TET2=TETA2
          XLAM=XLAMDA
          IF(FRABI.EQ.2) THEN
            CALL FSPRD2(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ELSEIF(FRABI.EQ.3) THEN
            CALL FSPRD3(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ELSEIF(FRABI.EQ.1) THEN
            CALL FSPRD1(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ENDIF

!
!         COMPUTES THE DIRECTIONAL SPECTRUM
!
          IF(FRABI.LE.3) THEN
            DO JF=1,NF
              DO JP=1,NDIRE
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
              ENDDO
            ENDDO
          ELSEIF(FRABI.EQ.4) THEN
! DIRECTION DEPENDS ON FREQUENCY
            DO JF=1,NF
              IF(FREQ(JF).LT.FPIC) THEN
                COEF1=SMAX*(FREQ(JF)/FPIC)**(5.0D0)
                DELT=0.5D0/DELFRA(COEF1)
              ELSE
                COEF1=SMAX*(FREQ(JF)/FPIC)**(-2.5D0)
                DELT = 0.5D0/DELFRA(COEF1)
              ENDIF
              DO JP=1,NDIRE
                DTETA = TETA(JP)-TETA1
                ARGUM = ABS(COS(0.5D0*(DTETA)))
                FRA(JP)=DELT*ARGUM**(2.D0*COEF1)
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
              ENDDO
            ENDDO
          ELSE
            WRITE(LU,*)'WRONG VALUE FOR ANGULAR DISTRIBUTION FUNCTION'
            CALL PLANTE(1)
          ENDIF
!
        ENDDO ! IP
!
!     ==/ INISPE = 7 /===========================================
!     IF NON ZERO WIND -E(F): PARAMETERISED TMA (HM0,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     IF ZERO WIND     -E(F): PARAMETERISED TMA (HM0,FP)
!                      -FRA : PARAMETERISED UNIMODAL
!     ===========================================================
!
      ELSEIF (INISPE.EQ.7) THEN
!
        COEF=0.0624D0/(0.230D0+0.0336D0*GAMMA-0.185D0/(1.9D0+GAMMA))
     &      *(DEUPI*FPIC)**4*HM0**2/GRAVIT**2
!
        DO IP=1,NPOIN2
!
!         COMPUTES THE FREQUENCY SPECTRUM (JONSWAP)
!
          AL = COEF
          FP = FPIC
!
          CALL SPETMA
     &( SPEC  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  FPMIN , DEPTH(IP) )
!
!         COMPUTES THE DIRECTIONAL SPREADING FUNCTION
!
          SPR1=SPRED1
          TET1=TETA1
          SPR2=SPRED2
          TET2=TETA2
          XLAM=XLAMDA
          IF(FRABI.EQ.2) THEN
            CALL FSPRD2(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ELSEIF(FRABI.EQ.3) THEN
            CALL FSPRD3(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ELSEIF(FRABI.EQ.1) THEN
            CALL FSPRD1(FRA,NDIRE,SPR1,TET1,SPR2,TET2,XLAM)
          ENDIF
!
!         COMPUTES THE THE DIRECTIONAL SPECTRUM
!
          IF(FRABI.LE.3) THEN
            DO JF=1,NF
              DO JP=1,NDIRE
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
              ENDDO
            ENDDO
          ELSEIF(FRABI.EQ.4) THEN
! DIRECTION DEPENDS ON FREQUENCY
            DO JF=1,NF
              IF(FREQ(JF).LT.FPIC) THEN
                COEF1=SMAX*(FREQ(JF)/FPIC)**(5.0D0)
                DELT=0.5D0/DELFRA(COEF1)
              ELSE
                COEF1=SMAX*(FREQ(JF)/FPIC)**(-2.5D0)
                DELT = 0.5D0/DELFRA(COEF1)
              ENDIF
              DO JP=1,NDIRE
                DTETA = TETA(JP)-TETA1
                ARGUM = ABS(COS(0.5D0*(DTETA)))
                FRA(JP)=DELT*ARGUM**(2.D0*COEF1)
                F(IP,JP,JF)=SPEC(JF)*FRA(JP)
              ENDDO
            ENDDO
          ELSE
            WRITE(LU,*)'WRONG VALUE FOR ANGULAR DISTRIBUTION FUNCTION'
            CALL PLANTE(1)
          ENDIF
!
        ENDDO ! IP
!
      ELSE
        WRITE(LU,*) 'SPEINI: UNKNOWN OPTION: ',INISPE
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!

      RETURN
      END
