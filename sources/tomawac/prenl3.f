!                       *****************
                        SUBROUTINE PRENL3
!                       *****************
!
!
!***********************************************************************
! TOMAWAC   V6P1                                   24/06/2011
!***********************************************************************
!
!brief   PREPARES THE COMPUTATION FOR THE NON-LINEAR INTERACTION
!+                SOURCE TERM BETWEEN QUADRUPLETS USING THE GQM METHOD
!+                ("GAUSSIAN QUADRATURE METHOD") PROPOSED BY LAVRENOV
!+                (2001)
!
!+            PROCEDURE SPECIFIC TO THE CASE WHERE THE FREQUENCIES
!+                FOLLOW A GEOMETRICAL PROGRESSION AND THE DIRECTIONS
!+                ARE EVENLY DISTRIBUTED OVER [0;2.PI].
!
!+note    THIS SUBROUTINE IS TO BE USED IN CONJONCT
!+          SUBROUTINE QNLIN3, WHICH IT OPTIMISES.
!
!reference  LAVRENOV, I.V. (2001):
!+           "EFFECT OF WIND WAVE PARAMETER FLUCTUATION ON THE NONLINEAR
!+           SPECTRUM EVOLUTION". J. PHYS. OCEANOGR. 31, 861-873.
!
!history  E. GAGNAIRE-RENOU
!+        04/2011
!+        V6P1
!+   CREATED
!
!history  G.MATTAROLO (EDF - LNHE)
!+        24/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMBUF         |-->| VARIABLE FOR SPECTRUM INTERPOLATION
!| ELIM           |<--| FRACTION OF ELIMINATED CONFIGURATIONS
!| F_COEF         |<--| WORK TABLE FOR SPECTRUM INTERPOLATION
!| F_POIN         |<--| WORK TABLE FOR SPECTRUM INTERPOLATION
!| F_PROJ         |<--| WORK TABLE FOR SPECTRUM INTERPOLATION
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| IDCONF         |<--| WORK TABLE
!| IQ_OM1         |<--| NUMBER OF INTEGRATION POINT ON OMEGA1
!| K_IF1          |<--| WORK TABLE
!| K_IF2          |<--| WORK TABLE
!| K_IF3          |<--| WORK TABLE
!| K_1M           |<--| WORK TABLE
!| K_1M2M         |<--| WORK TABLE
!| K_1M2P         |<--| WORK TABLE
!| K_1M3M         |<--| WORK TABLE
!| K_1M3P         |<--| WORK TABLE
!| K_1P           |<--| WORK TABLE
!| K_1P2M         |<--| WORK TABLE
!| K_1P2P         |<--| WORK TABLE
!| K_1P3M         |<--| WORK TABLE
!| K_1P3P         |<--| WORK TABLE
!| LBUF           |-->| VARIABLE FOR SPECTRUM INTERPOLATION
!| NCONF          |<--| NUMBER OF RETAINED CONFIGURATIONS
!| NCONFM         |-->| MAXIMUM NUMBER OF CONFIGURATIONS
!| NF             |-->| NUMBER OF FREQUENCIES
!| NF1            |-->| NUMBER OF INTEGRATION POINT ON OMEGA1
!| NQ_OM2         |-->| NUMBER OF INTEGRATION POINT ON OMEGA2
!| NQ_TE1         |-->| SETTING FOR INTEGRATION ON THETA1
!| NT             |-->| NUMBER OF DIRECTIONS
!| NT1            |-->| NUMBER OF INTEGRATION POINT ON THETA1
!| RAISF          |-->| FREQUENTIAL RATIO
!| SEUIL1         |-->| THRESHOLD1 FOR CONFIGURATIONS ELIMINATION
!| SEUIL2         |-->| THRESHOLD2 FOR CONFIGURATIONS ELIMINATION
!| T_POIN         |<--| WORK TABLE FOR SPECTRUM INTERPOLATION
!| TAILF          |-->| SPECTRUM QUEUE FACTOR
!| TB_FAC         |<--| WORK TABLE
!| TB_SCA         |<--| SCALE COEFFICIENT
!| TB_TMP         |<--| WORK TABLE
!| TB_TPM         |<--| WORK TABLE
!| TB_V14         |<--| WORK TABLE for F1
!| TB_V24         |<--| WORK TABLE
!| TB_V34         |<--| WORK TABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!Missing the description of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_PRENL3 => PRENL3
      IMPLICIT NONE
!
!
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER           JF    , JT    , JF1   , JT1   , NF1P1 , IAUX
      INTEGER           IQ_TE1, IQ_OM2
      DOUBLE PRECISION  EPSI_A, AUX   , CCC   , DENO  , AAA   , DP2SG
      DOUBLE PRECISION  V1    , V1_4  , DV1   , DTETAR
      DOUBLE PRECISION  V2    , V2_4  , V3    , V3_4
      DOUBLE PRECISION  W2    , W2_M  , W2_1  , W_MIL , W_RAD 
      DOUBLE PRECISION  RK0   , XK0   , YK0   , RK1   , XK1   , YK1
      DOUBLE PRECISION  RK2   , XK2P  , YK2P  , XK2M  , YK2M
      DOUBLE PRECISION  RK3   , XK3P  , YK3P  , XK3M  , YK3M
      DOUBLE PRECISION  D01P  , C_D01P, S_D01P, D0AP  , C_D0AP, S_D0AP
      DOUBLE PRECISION  GA2P  , C_GA2P, S_GA2P, GA3P  , C_GA3P, S_GA3P
      DOUBLE PRECISION, ALLOCATABLE :: F1SF(:)
!
!.....Variables related to the Gaussian quadratures
      DOUBLE PRECISION  W_CHE_TE1, W_CHE_OM2, C_LEG_OM2
      DOUBLE PRECISION, ALLOCATABLE :: X_CHE_TE1(:),X_CHE_OM2(:),
     &                                 X_LEG_OM2(:),W_LEG_OM2(:)
!
!.....Variables related to the configuration selection
      DOUBLE PRECISION  TEST1 , TEST2
      DOUBLE PRECISION, ALLOCATABLE :: MAXCLA(:)
      INTEGER NT 
!
!.....External functions (USE INTERFACE)
!      DOUBLE PRECISION  COUPLE
!      EXTERNAL          COUPLE
!
      NT=NDIRE
      DTETAR=DEUPI/DBLE(NT)
!
!=======================================================================
!     INITIALISATION OF AUXILIAIRY TABLES FOR SPECTRUM INTERPOLATION
!=======================================================================
      DO JF=1,DIMBUF
        F_POIN(JF)=0
        T_POIN(JF)=0
        F_COEF(JF)=0.D0
        F_PROJ(JF)=0.D0
        TB_SCA(JF)=0.0D0
      ENDDO
      DO JF=1,LBUF
        F_POIN(JF)=1
        F_COEF(JF)=0.0D0
        F_PROJ(JF)=0.0D0
      ENDDO
      DO JF=1,NF
        IAUX=LBUF+JF
        F_POIN(IAUX)=JF
        F_COEF(IAUX)=1.0D0
        F_PROJ(IAUX)=1.0D0
      ENDDO
      AUX=1.D0/RAISF**TAILF
      DO JF=1,LBUF
        IAUX=LBUF+NF+JF
        F_POIN(IAUX)=NF
        F_COEF(IAUX)=AUX**JF
        F_PROJ(IAUX)=0.0D0
      ENDDO
!
      DO JT=LBUF,1,-1
        T_POIN(JT)=NT-MOD(LBUF-JT,NT)
      ENDDO
      DO JT=1,NT
        T_POIN(LBUF+JT)=JT
      ENDDO
      DO JT=1,LBUF
        T_POIN(LBUF+NT+JT)=MOD(JT-1,NT)+1
      ENDDO
!======================================================================
!
!=======================================================================
!     COMPUTES SCALE COEFFICIENTS FOR THE COUPLING COEFFICIENT
!=======================================================================
      DP2SG=DEUPI*DEUPI/GRAVIT
      DO JF=1,LBUF
        AUX=FREQ(1)/RAISF**(LBUF-JF+1)
        TB_SCA(JF)=(DP2SG*AUX**2)**6/(DEUPI**3*AUX)
      ENDDO
      DO JF=1,NF
        TB_SCA(LBUF+JF)=(DP2SG*FREQ(JF)**2)**6/(DEUPI**3*FREQ(JF))
      ENDDO
      DO JF=1,LBUF
        IAUX=LBUF+NF+JF
        AUX=FREQ(NF)*RAISF**JF
        TB_SCA(IAUX)=(DP2SG*AUX**2)**6/(DEUPI**3*AUX)
      ENDDO
!=======================================================================
!
!=======================================================================
!     COMPUTES VALUES FOR GAUSSIAN QUADRATURES
!=======================================================================
      ALLOCATE(X_CHE_TE1(1:NQ_TE1),X_CHE_OM2(1:NQ_OM2))
      ALLOCATE(X_LEG_OM2(1:NQ_OM2),W_LEG_OM2(1:NQ_OM2))
!
!.....Abscissa and weight (constant) for Gauss-Chebyshev
      DO IQ_TE1=1,NQ_TE1
        X_CHE_TE1(IQ_TE1)=COS(PI*(DBLE(IQ_TE1)-0.5D0)/DBLE(NQ_TE1))
      ENDDO
      W_CHE_TE1=PI/DBLE(NQ_TE1)
      DO IQ_OM2=1,NQ_OM2
        X_CHE_OM2(IQ_OM2)=COS(PI*(DBLE(IQ_OM2)-0.5D0)/DBLE(NQ_OM2))
      ENDDO
      W_CHE_OM2=PI/DBLE(NQ_OM2)
!
!.....Abscissa et weight for Gauss-Legendre
      CALL GAULEG
     &( W_LEG_OM2 , X_LEG_OM2 , NQ_OM2 )
      DO IQ_OM2=1,NQ_OM2
        X_LEG_OM2(IQ_OM2)=0.25D0*(1.D0+X_LEG_OM2(IQ_OM2))**2
      ENDDO
!=======================================================================
!
!
!=======================================================================
!     COMPUTES VALUES OF RATIO F1/F AS FUNCTION OF THE IQ_OM1 INDICATOR
!=======================================================================
      NF1P1=NF1+1
      ALLOCATE(F1SF(1:NF1P1))
      CALL F1F1F1 ( F1SF  , NF1   , IQ_OM1)
!=======================================================================
!
!     ==================================================
!     STARTS LOOP 1 OVER THE RATIOS F1/F0
!     ==================================================
      DO JF1=1,NF1
!       ---------Computes and stores v1=f1/f0 and v1**4
        V1=(F1SF(JF1+1)+F1SF(JF1))/2.D0
          K_IF1(JF1)=NINT(DBLE(LBUF)+LOG(V1)/LOG(RAISF))
        V1_4=V1**4
        TB_V14(JF1)=V1_4
!       ---------Computes and stores dv1=df1/f0
        DV1=F1SF(JF1+1)-F1SF(JF1)
!       ---------Computes the A parameter
        AAA=((1.D0+V1)**4-4.D0*(1.D0+V1_4))/(8.D0*V1**2)
!
!
!
!       =================================================
!       STARTS LOOP 2 OVER THE DELTA_1+ VALUES
!       =================================================
        DO JT1=1,NT1
!
!......Computes the Delta1+ values (=Theta_1-Theta_0) between 0 and Pi.
          IF (JT1.LE.NQ_TE1) THEN
!           ---------First interval : X from -1 to A
            IQ_TE1=JT1
            C_D01P=(-1.D0+AAA)/2.D0+(1.D0+AAA)/2.D0*X_CHE_TE1(IQ_TE1)
            CCC=DV1*SQRT((AAA-C_D01P)/(1.D0-C_D01P))*W_CHE_TE1
          ELSE
!           ---------Second interval : X from A to 1
            IQ_TE1=JT1-NQ_TE1
            C_D01P=( 1.D0+AAA)/2.D0+(1.D0-AAA)/2.D0*X_CHE_TE1(IQ_TE1)
            CCC=DV1*SQRT((C_D01P-AAA)/(1.D0+C_D01P))*W_CHE_TE1
          ENDIF
          S_D01P=SQRT(1.D0-C_D01P*C_D01P)
          D01P  =ACOS(C_D01P)
          K_1P(JT1,JF1)=LBUF+NINT(D01P/DTETAR)
          K_1M(JT1,JF1)=LBUF-NINT(D01P/DTETAR)
!
!         ---------Computes Epsilon_a
          EPSI_A=2.D0*SQRT(1.D0+V1_4+2.D0*V1*V1*C_D01P)/(1.D0+V1)**2
!         ---------Computes Delta_A+ and its cosinus
          C_D0AP=(1.D0-V1_4+0.25D0*EPSI_A**2*(1.D0+V1)**4)
     &           /(EPSI_A*(1.D0+V1)**2)
          S_D0AP=SQRT(1.0D0-C_D0AP*C_D0AP)
          D0AP  = ACOS(C_D0AP)
!
!.......Integration over OMEGA2 depending on EPS_A
          IF (EPSI_A.LT.1.D0) THEN
!        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!........Case of a single singularity (in OMEGA2-)
!        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            W2_M=0.5D0*(1.D0-EPSI_A/2.D0)
            W2_1=0.5D0
!
            W_RAD=W2_1-W2_M
            C_LEG_OM2=SQRT(W_RAD)
!
!        ----------------------------------------------------
!........STARTS LOOP 3 OVER OMEGA_2 (CASE Epsilon_A < 1)
!........Case of a single singularity (in OMEGA2-)
!........Integration over OMEGA2 via GAUSS-LEGENDRE quadrature
!        ----------------------------------------------------
            DO IQ_OM2=1,NQ_OM2
!             ---------Computes W2, V2, and V3
              W2=W2_M+W_RAD*X_LEG_OM2(IQ_OM2)
              V2=W2*(1.D0+V1)
              V2_4=V2**4
              TB_V24(IQ_OM2,JT1,JF1)=V2_4
              K_IF2 (IQ_OM2,JT1,JF1) = NINT(DBLE(LBUF)
     &                               + LOG(V2)/LOG(RAISF))
              V3=1.D0+V1-V2
              V3_4=V3**4
              TB_V34(IQ_OM2,JT1,JF1)=V3_4
              K_IF3 (IQ_OM2,JT1,JF1) = NINT(DBLE(LBUF)
     &                               + LOG(V3)/LOG(RAISF))
!             ---------Computes Gamma_2+ et Gamma_3+ angles
              C_GA2P=(EPSI_A**2/4.D0+W2**4-(1.D0-W2)**4)/(EPSI_A*W2*W2)
              C_GA2P=MAX(MIN(C_GA2P,1.D0),-1.D0)
              S_GA2P=SQRT(1.D0-C_GA2P*C_GA2P)
              GA2P  =ACOS(C_GA2P)
              C_GA3P=(EPSI_A**2/4.D0-W2**4+(1.D0-W2)**4)/EPSI_A
     &              /(1.D0-W2)**2
              C_GA3P=MAX(MIN(C_GA3P,1.D0),-1.D0)
              S_GA3P=SQRT(1.D0-C_GA3P*C_GA3P)
              GA3P  =ACOS(C_GA3P)
!             Shifting of the direction indexes - Config. +Delta1 (SIG=1)
              K_1P2P(IQ_OM2,JT1,JF1)=NINT(( D0AP+GA2P)/DTETAR
     &                              +DBLE(LBUF))
              K_1P3M(IQ_OM2,JT1,JF1)=NINT(( D0AP-GA3P)/DTETAR
     &                              +DBLE(LBUF))
              K_1P2M(IQ_OM2,JT1,JF1)=NINT(( D0AP-GA2P)/DTETAR
     &                              +DBLE(LBUF))
              K_1P3P(IQ_OM2,JT1,JF1)=NINT(( D0AP+GA3P)/DTETAR
     &                              +DBLE(LBUF))
!             Shifting of the direction indexes - Config. -Delta1 (SIG=-1)
              K_1M2P(IQ_OM2,JT1,JF1)=NINT((-D0AP+GA2P)/DTETAR
     &                              +DBLE(LBUF))
              K_1M3M(IQ_OM2,JT1,JF1)=NINT((-D0AP-GA3P)/DTETAR
     &                              +DBLE(LBUF))
              K_1M2M(IQ_OM2,JT1,JF1)=NINT((-D0AP-GA2P)/DTETAR
     &                              +DBLE(LBUF))
              K_1M3P(IQ_OM2,JT1,JF1)=NINT((-D0AP+GA3P)/DTETAR
     &                              +DBLE(LBUF))
!
!.........Computes the coupling coefficients (only for Delta_1+ )
              RK0=1.D0
              RK1=V1*V1
              RK2=V2*V2
              RK3=(1.D0+V1-V2)**2
              XK0  = RK0
              YK0  = 0.0D0
              XK1  = RK1*C_D01P
              YK1  = RK1*S_D01P
              XK2P = RK2*(C_D0AP*C_GA2P-S_D0AP*S_GA2P)
              YK2P = RK2*(S_D0AP*C_GA2P+C_D0AP*S_GA2P)
              XK2M = RK2*(C_D0AP*C_GA2P+S_D0AP*S_GA2P)
              YK2M = RK2*(S_D0AP*C_GA2P-C_D0AP*S_GA2P)
              XK3P = RK3*(C_D0AP*C_GA3P-S_D0AP*S_GA3P)
              YK3P = RK3*(S_D0AP*C_GA3P+C_D0AP*S_GA3P)
              XK3M = RK3*(C_D0AP*C_GA3P+S_D0AP*S_GA3P)
              YK3M = RK3*(S_D0AP*C_GA3P-C_D0AP*S_GA3P)
              TB_TPM(IQ_OM2,JT1,JF1)=COUPLE
     &( XK0   , YK0   , XK1   , YK1   , XK2P  , YK2P  , XK3M  , YK3M  )
              TB_TMP(IQ_OM2,JT1,JF1)=COUPLE
     &( XK0   , YK0   , XK1   , YK1   , XK2M  , YK2M  , XK3P  , YK3P  )
!
!.........Computes the multiplicative coefficient for QNL4
              DENO=2.D0*SQRT( (0.5D0*(1.D0+EPSI_A/2.D0)-W2)
     &                       *((W2-0.5D0)**2+0.25D0*(1.D0+EPSI_A))
     &                       *((W2-0.5D0)**2+0.25D0*(1.D0-EPSI_A)) )
              TB_FAC(IQ_OM2,JT1,JF1)=1.D0/(DENO*V1*W2*(1.D0-W2))
     &                  /(1.D0+V1)**5 * W_LEG_OM2(IQ_OM2)*C_LEG_OM2* CCC
            ENDDO
!        -----------------------------------------------
!........END OF THE LOOP 3 OVER OMEGA_2 (CASE Epsilon_A < 1)
!        -----------------------------------------------
!
          ELSE
!        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!........STARTS LOOP 3 OVER OMEGA_2 (CASE Epsilon_A > 1)
!........Case of two singularities (in OMEGA2- and OMEGA2_1)
!........Integration over OMEGA2 via GAUSS-CHEBYSCHEV quadrature
!        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            W2_M=0.5D0*(1.D0-EPSI_A/2.D0)
            W2_1=0.5D0*(1.D0-SQRT(EPSI_A-1.D0))
!
            W_MIL=(W2_M+W2_1)/2.D0
            W_RAD=(W2_1-W2_M)/2.D0
!
            DO IQ_OM2=1,NQ_OM2
!             ---------Computes W2, V2, and V3
              W2=W_MIL+W_RAD*X_CHE_OM2(IQ_OM2)
              V2=W2*(1.D0+V1)
              V2_4=V2**4
              TB_V24(IQ_OM2,JT1,JF1)=V2_4
                K_IF2 (IQ_OM2,JT1,JF1)=NINT(DBLE(LBUF)
     &                                +LOG(V2)/LOG(RAISF))
              V3=1.D0+V1-V2
              V3_4=V3**4
              TB_V34(IQ_OM2,JT1,JF1)=V3_4
              K_IF3 (IQ_OM2,JT1,JF1)=NINT(DBLE(LBUF)
     &                              +LOG(V3)/LOG(RAISF))
!             ---------Computes Gamma_2+ et Gamma_3+ angles
              C_GA2P=(EPSI_A**2/4.D0+W2**4-(1.D0-W2)**4)/(EPSI_A*W2*W2)
              C_GA2P=MAX(MIN(C_GA2P,1.D0),-1.D0)
              S_GA2P=SQRT(1.D0-C_GA2P*C_GA2P)
              GA2P  =ACOS(C_GA2P)
              C_GA3P=(EPSI_A**2/4.D0-W2**4+(1.D0-W2)**4)/EPSI_A
     &              /(1.D0-W2)**2
              C_GA3P=MAX(MIN(C_GA3P,1.D0),-1.D0)
              S_GA3P=SQRT(1.D0-C_GA3P*C_GA3P)
              GA3P  =ACOS(C_GA3P)
!             Shifts the direction indexes - Config. +Delta1 (SIG=1)
              K_1P2P(IQ_OM2,JT1,JF1)=NINT(( D0AP+GA2P)/DTETAR
     &                              +DBLE(LBUF))
              K_1P3M(IQ_OM2,JT1,JF1)=NINT(( D0AP-GA3P)/DTETAR
     &                              +DBLE(LBUF))
              K_1P2M(IQ_OM2,JT1,JF1)=NINT(( D0AP-GA2P)/DTETAR
     &                              +DBLE(LBUF))
              K_1P3P(IQ_OM2,JT1,JF1)=NINT(( D0AP+GA3P)/DTETAR
     &                              +DBLE(LBUF))
!             Shifts the direction indexes - Config. -Delta1 (SIG=-1)
              K_1M2P(IQ_OM2,JT1,JF1)=NINT((-D0AP+GA2P)/DTETAR
     &                              +DBLE(LBUF))
              K_1M3M(IQ_OM2,JT1,JF1)=NINT((-D0AP-GA3P)/DTETAR
     &                              +DBLE(LBUF))
              K_1M2M(IQ_OM2,JT1,JF1)=NINT((-D0AP-GA2P)/DTETAR
     &                              +DBLE(LBUF))
              K_1M3P(IQ_OM2,JT1,JF1)=NINT((-D0AP+GA3P)/DTETAR
     &                              +DBLE(LBUF))
!
!.........Computes the coupling coefficients (only for Delta_1+ )
              RK0=1.D0
              RK1=V1*V1
              RK2=V2*V2
              RK3=(1.D0+V1-V2)**2
              XK0  = RK0
              YK0  = 0.0D0
              XK1  = RK1*C_D01P
              YK1  = RK1*S_D01P
              XK2P = RK2*(C_D0AP*C_GA2P-S_D0AP*S_GA2P)
              YK2P = RK2*(S_D0AP*C_GA2P+C_D0AP*S_GA2P)
              XK2M = RK2*(C_D0AP*C_GA2P+S_D0AP*S_GA2P)
              YK2M = RK2*(S_D0AP*C_GA2P-C_D0AP*S_GA2P)
              XK3P = RK3*(C_D0AP*C_GA3P-S_D0AP*S_GA3P)
              YK3P = RK3*(S_D0AP*C_GA3P+C_D0AP*S_GA3P)
              XK3M = RK3*(C_D0AP*C_GA3P+S_D0AP*S_GA3P)
              YK3M = RK3*(S_D0AP*C_GA3P-C_D0AP*S_GA3P)
              TB_TPM(IQ_OM2,JT1,JF1)=COUPLE
     &( XK0   , YK0   , XK1   , YK1   , XK2P  , YK2P  , XK3M  , YK3M  )
              TB_TMP(IQ_OM2,JT1,JF1)=COUPLE
     &( XK0   , YK0   , XK1   , YK1   , XK2M  , YK2M  , XK3P  , YK3P  )
!
!.........Computes the multiplicative coefficient for QNL4
              DENO=2.D0*SQRT( (0.5D0*(1.D0+EPSI_A/2.D0)-W2)
     &                       *((W2-0.5D0)**2+0.25D0*(1.D0+EPSI_A))
     &                       *(0.5D0*(1.D0+SQRT(EPSI_A-1.D0))-W2) )
              TB_FAC(IQ_OM2,JT1,JF1)=1.D0/(DENO*V1*W2*(1.D0-W2))
     &              /(1.D0+V1)**5 * W_CHE_OM2* CCC
!
            ENDDO
!        -----------------------------------------------
!........END OF LOOP 3 OVER OMEGA_2 (CASE Epsilon_A > 1)
!        -----------------------------------------------
!
          ENDIF
!
!
        ENDDO
!       =================================================
!       END OF LOOP 2 OVER THE DELTA_1+ VALUES
!       =================================================
!
      ENDDO
!     ==================================================
!     END OF LOOP 1 OVER THE F1/F0 RATIOS
!     ==================================================
      DEALLOCATE(F1SF)
      DEALLOCATE(X_CHE_TE1)
      DEALLOCATE(X_CHE_OM2)
      DEALLOCATE(X_LEG_OM2)
      DEALLOCATE(W_LEG_OM2)
!
!
!
!
!     ===========================================================
!     POST-PROCESSING TO ELIMINATE PART OF THE CONFIGURATIONS
!     ===========================================================
!
!.....It looks, for every value of the ratio V1, for the maximum value
!.....of FACTOR*COUPLING : it is stored in the local table NAXCLA(.)
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      ALLOCATE(MAXCLA(1:NF1))
      DO JF1=1,NF1
        AUX=0.0D0
        DO JT1=1,NT1
          DO IQ_OM2=1,NQ_OM2
            AAA=TB_FAC(IQ_OM2,JT1,JF1)*TB_TPM(IQ_OM2,JT1,JF1)
            IF (AAA.GT.AUX) AUX=AAA
            CCC=TB_FAC(IQ_OM2,JT1,JF1)*TB_TMP(IQ_OM2,JT1,JF1)
            IF (CCC.GT.AUX) AUX=CCC
          ENDDO
        ENDDO
        MAXCLA(JF1)=AUX
      ENDDO
!
!.....It looks for the max V1 value
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      AUX=0.0D0
      DO JF1=1,NF1
        IF (MAXCLA(JF1).GT.AUX) AUX=MAXCLA(JF1)
      ENDDO
      TEST1=SEUIL1*AUX
!
!.....Set to zero the coupling coefficients not used
!     """""""""""""""""""""""""""""""""""""""""""""""""""""
      NCONF=0
      DO JF1=1,NF1
        TEST2 =SEUIL2*MAXCLA(JF1)
        DO JT1=1,NT1
          DO IQ_OM2=1,NQ_OM2
            AAA=TB_FAC(IQ_OM2,JT1,JF1)*TB_TPM(IQ_OM2,JT1,JF1)
            CCC=TB_FAC(IQ_OM2,JT1,JF1)*TB_TMP(IQ_OM2,JT1,JF1)
            IF ((AAA.GT.TEST1.OR.AAA.GT.TEST2).OR.
     &          (CCC.GT.TEST1.OR.CCC.GT.TEST2)) THEN
              NCONF=NCONF+1
              IDCONF(NCONF,1)=JF1
              IDCONF(NCONF,2)=JT1
              IDCONF(NCONF,3)=IQ_OM2
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(MAXCLA)
!
!.....It counts the fraction of the eliminated configurations
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""
      ELIM=(1.D0-DBLE(NCONF)/DBLE(NCONFM))*100.D0
      WRITE(LU,*) 'PART DE CONFIGURATIONS ANNULEES : ',ELIM,' %'
!
      RETURN
      END
