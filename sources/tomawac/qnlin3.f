!                       *****************
                        SUBROUTINE QNLIN3
!                       *****************
!
     &( T1TOT,T1DER, F2, N1POIN2, N1PLAN, N1F, FSEUIL )
!
!***********************************************************************
! TOMAWAC   V6P1                                   24/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE NON-LINEAR INTERACTIONS
!+                SOURCE TERM BETWEEN QUADRUPLETS USING THE GQM METHOD
!+                ("GAUSSIAN QUADRATURE METHOD") PROPOSED BY LAVRENOV
!+                (2001)
!+
!+            PROCEDURE SPECIFIC TO THE CASE WHERE THE FREQUENCIES
!+                FOLLOW A GEOMETRICAL PROGRESSION AND THE DIRECTIONS
!+                ARE EVENLY DISTRIBUTED OVER [0;2.PI].
!
!note     THIS SUBROUTINE USES THE OUTPUT FROM 'PRENL3' TO OPTIMISE
!+          THE COMPUTATIONS FOR DIA.
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMBUF         |-->| VARIABLE FOR SPECTRUM INTERPOLATION
!| F              |-->| DIRECTIONAL SPECTRUM
!| F_COEF         |-->| WORK TABLE FOR SPECTRUM INTERPOLATION
!| F_POIN         |-->| WORK TABLE FOR SPECTRUM INTERPOLATION
!| F_PROJ         |-->| WORK TABLE FOR SPECTRUM INTERPOLATION
!| FSEUIL         |-->| WORK TABLE
!| K_IF1          |-->| WORK TABLE
!| K_IF2          |-->| WORK TABLE
!| K_IF3          |-->| WORK TABLE
!| K_1M           |-->| WORK TABLE
!| K_1M2M         |-->| WORK TABLE
!| K_1M2P         |-->| WORK TABLE
!| K_1M3M         |-->| WORK TABLE
!| K_1M3P         |-->| WORK TABLE
!| K_1P           |-->| WORK TABLE
!| K_1P2M         |-->| WORK TABLE
!| K_1P2P         |-->| WORK TABLE
!| K_1P3M         |-->| WORK TABLE
!| K_1P3P         |-->| WORK TABLE
!| IDCONF         |-->| WORK TABLE
!| LBUF           |-->| VARIABLE FOR SPECTRUM INTERPOLATION
!| NB_NOD         |-->| NUMBER OF POINTS IN 2D MESH
!| NCONF          |-->| NUMBER OF RETAINED CONFIGURATIONS
!| NCONFM         |-->| MAXIMUM NUMBER OF CONFIGURATIONS
!| NF             |-->| NUMBER OF FREQUENCIES
!| NF1            |-->| NUMBER OF INTEGRATION POINT ON OMEGA1
!| NQ_OM2         |-->| NUMBER OF INTEGRATION POINT ON OMEGA2
!| NT             |-->| NUMBER OF DIRECTIONS
!| NT1            |-->| NUMBER OF INTEGRATION POINT ON TETA1
!| RAISF          |-->| FREQUENTIAL RATIO
!| SEUIL          |-->| THRESHOLD0 FOR CONFIGURATIONS ELIMINATION (GQM)
!| T_POIN         |-->| WORK TABLE FOR SPECTRUM INTERPOLATION
!| TB_FAC         |-->| WORK TABLE
!| TB_SCA         |-->| SCALE COEFFICIENT
!| TB_TMP         |-->| WORK TABLE
!| TB_TPM         |-->| WORK TABLE
!| TB_V14         |-->| WORK TABLE
!| TB_V24         |-->| WORK TABLE
!| TB_V34         |-->| WORK TABLE
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_QNLIN3 => QNLIN3
      USE DECLARATIONS_TOMAWAC
!     
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
!     A WAY TO CHANGE DIMENSION OF ARRAY
!     TSTOT,TSDER AND F HAVE ONLY ONE DIMENSION
      
      INTEGER, INTENT(IN)    :: N1POIN2,N1PLAN,N1F
      DOUBLE PRECISION, INTENT(INOUT) :: T1TOT( N1POIN2,N1PLAN,N1F)
      DOUBLE PRECISION, INTENT(INOUT) :: T1DER( N1POIN2,N1PLAN,N1F)
      DOUBLE PRECISION, INTENT(INOUT) :: F2(N1POIN2,N1PLAN,N1F)
      DOUBLE PRECISION, INTENT(INOUT) :: FSEUIL(N1POIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER           IP    , JF    , JT    , JF1   , JT1   , IQ_OM2,
     &                  JFM0  , JFM1  , JFM2  , JFM3  , IXF1  , IXF2  ,
     &                  IXF3  , JFMIN , JFMAX , ICONF
      INTEGER           KT1P  , KT1M  , JT1P  , JT1M  , KT1P2P, KT1P2M,
     &                  KT1P3P, KT1P3M, KT1M2P, KT1M2M, KT1M3P, KT1M3M,
     &                  JT1P2P, JT1P2M, JT1P3P, JT1P3M, JT1M2P, JT1M2M,
     &                  JT1M3P, JT1M3M
      DOUBLE PRECISION  V1_4  , V2_4  , V3_4  , Q_2P3M, Q_2M3P, FACTOR,
     &                  T_2P3M, T_2M3P, S_2P3M, S_2M3P, SCAL_T, T2P3M ,
     &                  T2M3P , SP0   , SP1P  , SP1M  , SP1P2P, SP1P2M,
     &                  SP1P3P, SP1P3M, SP1M2P, SP1M2M, SP1M3P, SP1M3M,
     &                  CF0   , CP0   , CF1   , CP1   , CF2   , CP2   ,
     &                  CF3   , CP3   , Q2PD0 , Q2PD1 , Q2PD2P, Q2PD3M,
     &                  Q2MD0 , Q2MD1 , Q2MD2M, Q2MD3P,
     &                  AUX00 , AUX01 , AUX02 , AUX03 , AUX04 , AUX05 ,
     &                  AUX06 , AUX07 , AUX08 , AUX09 , AUX10
!
!=======================================================================
!     COMPUTES THE GENERALIZED MIN AND MAX FREQUENCIES : INSTEAD OF GOING
!     FROM 1 TO NF IN FREQ(JF) FOR THE MAIN FREQUENCY, IT GOES FROM JFMIN
!     TO JFMAX
!     JFMIN IS GIVEN BY Fmin=FREQ(1) /Gamma_min
!     JFMAX IS GIVEN BY Fmax=FREQ(NF)*Gamma_max
!     TESTS HAVE SHOWN THAT IT CAN BE ASSUMED Gamma_min=1. (JFMIN=1) AND
!     Gamma_max=1.3 (JFMAX>NF) TO OBTAIN IMPROVED RESULTS
!=======================================================================
      JFMIN= 1-INT(LOG(1.0D0)/LOG(RAISF))
      JFMAX=NF+INT(LOG(1.3D0)/LOG(RAISF))
!
!=======================================================================
!     COMPUTES THE SPECTRUM THRESHOLD VALUES (BELOW WHICH QNL4 IS NOT
!     CALCULATED). THE THRESHOLD IS SET WITHIN 0 AND 1.
!=======================================================================
      DO IP=1,NPOIN2
        AUX00=0.0D0
        DO JF=1,NF
          DO JT=1,NDIRE
            IF (F2(IP,JT,JF).GT.AUX00) AUX00=F2(IP,JT,JF)
          ENDDO
        ENDDO
        FSEUIL(IP)=AUX00*SEUIL
      ENDDO
!=======================================================================
!
!
!
!
!     ==================================================
!     STARTS LOOP 1 OVER THE SELECTED CONFIGURATIONS
!     ==================================================
      DO ICONF=1,NCONF
!       ---------selected configuration characteristics
        JF1   =IDCONF(ICONF,1)
        JT1   =IDCONF(ICONF,2)
        IQ_OM2=IDCONF(ICONF,3)
!
!       ---------Recovers V1**4=(f1/f0)**4
        V1_4  =TB_V14(JF1)
!       ---------Recovers the shift of the frequency index on f1
        IXF1  =K_IF1(JF1)
!       ---------Recovers the direction indexes for Delat1
        KT1P  =K_1P(JT1,JF1)
        KT1M  =K_1M(JT1,JF1)
!       ---------Recovers V2**4=(f2/f0)**4 and V3**4=(f3/f0)**4
        V2_4  =TB_V24(IQ_OM2,JT1,JF1)
        V3_4  =TB_V34(IQ_OM2,JT1,JF1)
!       ---------Recovers the frequency indexes shift on f2 and f3
        IXF2  =K_IF2 (IQ_OM2,JT1,JF1)
        IXF3  =K_IF3 (IQ_OM2,JT1,JF1)
!       ---------Recovers the direction indexes shift
        KT1P2P=K_1P2P(IQ_OM2,JT1,JF1)
        KT1P2M=K_1P2M(IQ_OM2,JT1,JF1)
        KT1P3P=K_1P3P(IQ_OM2,JT1,JF1)
        KT1P3M=K_1P3M(IQ_OM2,JT1,JF1)
        KT1M2P=K_1M2P(IQ_OM2,JT1,JF1)
        KT1M2M=K_1M2M(IQ_OM2,JT1,JF1)
        KT1M3P=K_1M3P(IQ_OM2,JT1,JF1)
        KT1M3M=K_1M3M(IQ_OM2,JT1,JF1)
!       ---------Recovers the coupling coefficients
        T2P3M =TB_TPM(IQ_OM2,JT1,JF1)
        T2M3P =TB_TMP(IQ_OM2,JT1,JF1)
!       ---------Recovers the multiplicative factor of QNL4
        FACTOR=TB_FAC(IQ_OM2,JT1,JF1)
!
!       = = = = = = = = = = = = = = = = = = = = = = = = =
!       STARTS LOOP 2 OVER THE SPECTRUM FREQUENCIES
!       = = = = = = = = = = = = = = = = = = = = = = = = =
        DO JF=JFMIN,JFMAX
!
!.........Recovers the coefficient for the coupling factor
!.........Computes the coupling coefficients for the case +Delta1 (SIG=1)
          SCAL_T=TB_SCA(LBUF+JF)*FACTOR
          T_2P3M=T2P3M*SCAL_T
          T_2M3P=T2M3P*SCAL_T
!
!.........Frequency indexes and coefficients
          JFM0=F_POIN(JF+LBUF)
          CF0 =F_COEF(JF+LBUF)
          CP0 =F_PROJ(JF+LBUF)
          JFM1=F_POIN(JF+IXF1)
          CF1 =F_COEF(JF+IXF1)
          CP1 =F_PROJ(JF+IXF1)
          JFM2=F_POIN(JF+IXF2)
          CF2 =F_COEF(JF+IXF2)
          CP2 =F_PROJ(JF+IXF2)
          JFM3=F_POIN(JF+IXF3)
          CF3 =F_COEF(JF+IXF3)
          CP3 =F_PROJ(JF+IXF3)
!
!         -------------------------------------------------
!         STARTS LOOP 3 OVER THE SPECTRUM DIRECTIONS
!         -------------------------------------------------
          DO JT=1,NDIRE
!
!...........Direction indexes
!           direct config (+delta1) (sig =1)
            JT1P  =T_POIN(JT+KT1P)
            JT1P2P=T_POIN(JT+KT1P2P)
            JT1P2M=T_POIN(JT+KT1P2M)
            JT1P3P=T_POIN(JT+KT1P3P)
            JT1P3M=T_POIN(JT+KT1P3M)
!           image config (-delta1)
            JT1M  =T_POIN(JT+KT1M)
            JT1M2P=T_POIN(JT+KT1M2P)
            JT1M2M=T_POIN(JT+KT1M2M)
            JT1M3P=T_POIN(JT+KT1M3P)
            JT1M3M=T_POIN(JT+KT1M3M)
!
!           - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!           STARTS LOOP 4 OVER THE MESH NODES
!           - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            DO IP=1,NPOIN2
!
              SP0=F2(IP,JT,JFM0)*CF0
!
              IF (SP0.GT.FSEUIL(IP)) THEN
!
!               Config. +Delta1 (SIG=1)
!               =======================
!...............Computes the spectrum values in 1, 2, 3
                SP1P  =F2(IP,JT1P  ,JFM1)*CF1
                SP1P2P=F2(IP,JT1P2P,JFM2)*CF2
                SP1P3M=F2(IP,JT1P3M,JFM3)*CF3
                SP1P2M=F2(IP,JT1P2M,JFM2)*CF2
                SP1P3P=F2(IP,JT1P3P,JFM3)*CF3
!
!...............Computes auxiliary products and variables
                AUX01=SP0*V1_4+SP1P
                AUX02=SP0*SP1P
                AUX03=SP1P2P*SP1P3M
                AUX04=SP1P2P*V3_4+SP1P3M*V2_4
                AUX05=SP1P2M*SP1P3P
                AUX06=SP1P2M*V3_4+SP1P3P*V2_4
                AUX07=AUX02*V3_4
                AUX08=AUX02*V2_4
!
!...............Computes the components of the transfer term
                S_2P3M=AUX03*AUX01-AUX02*AUX04
                S_2M3P=AUX05*AUX01-AUX02*AUX06
                Q_2P3M=T_2P3M*S_2P3M
                Q_2M3P=T_2M3P*S_2M3P
                AUX00 =Q_2P3M+Q_2M3P
!
!...............Computes the components of the derived terms (dQ/dF)
                Q2PD0 =T_2P3M*(AUX03*V1_4   - SP1P*AUX04)*CF0
                Q2PD1 =T_2P3M*(AUX03        - SP0 *AUX04)*CF1
                Q2PD2P=T_2P3M*(AUX01*SP1P3M - AUX07     )*CF2
                Q2PD3M=T_2P3M*(AUX01*SP1P2P - AUX08     )*CF3
                  Q2MD0 =T_2M3P*(AUX05*V1_4   - SP1P*AUX06)*CF0
                  Q2MD1 =T_2M3P*(AUX03        - SP0 *AUX06)*CF1
                  Q2MD2M=T_2M3P*(AUX01*SP1P3P - AUX07     )*CF2
                  Q2MD3P=T_2M3P*(AUX01*SP1P2M - AUX08     )*CF3
                AUX09=Q2PD0+Q2MD0
                AUX10=Q2PD1+Q2MD1
!
!...............Sum of Qnl4 term in the table TSTOT
                T1TOT(IP,JT    ,JFM0)=T1TOT(IP,JT    ,JFM0)+AUX00 *CP0
                T1TOT(IP,JT1P  ,JFM1)=T1TOT(IP,JT1P  ,JFM1)+AUX00 *CP1
                T1TOT(IP,JT1P2P,JFM2)=T1TOT(IP,JT1P2P,JFM2)-Q_2P3M*CP2
                T1TOT(IP,JT1P2M,JFM2)=T1TOT(IP,JT1P2M,JFM2)-Q_2M3P*CP2
                T1TOT(IP,JT1P3M,JFM3)=T1TOT(IP,JT1P3M,JFM3)-Q_2P3M*CP3
                T1TOT(IP,JT1P3P,JFM3)=T1TOT(IP,JT1P3P,JFM3)-Q_2M3P*CP3
!
!...............Sum of the term dQnl4/dF in the table T1DER
                T1DER(IP,JT    ,JFM0)=T1DER(IP,JT    ,JFM0)+AUX09 *CP0
                T1DER(IP,JT1P  ,JFM1)=T1DER(IP,JT1P  ,JFM1)+AUX10 *CP1
                T1DER(IP,JT1P2P,JFM2)=T1DER(IP,JT1P2P,JFM2)-Q2PD2P*CP2
                T1DER(IP,JT1P2M,JFM2)=T1DER(IP,JT1P2M,JFM2)-Q2MD2M*CP2
                T1DER(IP,JT1P3M,JFM3)=T1DER(IP,JT1P3M,JFM3)-Q2PD3M*CP3
                T1DER(IP,JT1P3P,JFM3)=T1DER(IP,JT1P3P,JFM3)-Q2MD3P*CP3
!
!               Config. -Delta1 (SIG=-1)
!               ========================
!...............Computes the spectrum values in 1, 2, 3
                SP1M  =F2(IP,JT1M  ,JFM1)*CF1
                SP1M2P=F2(IP,JT1M2P,JFM2)*CF2
                SP1M3M=F2(IP,JT1M3M,JFM3)*CF3
                SP1M2M=F2(IP,JT1M2M,JFM2)*CF2
                SP1M3P=F2(IP,JT1M3P,JFM3)*CF3
!
!...............Computes auxiliary products and variables
                AUX01=SP0*V1_4+SP1M
                AUX02=SP0*SP1M
                AUX03=SP1M2P*SP1M3M
                AUX04=SP1M2P*V3_4+SP1M3M*V2_4
                AUX05=SP1M2M*SP1M3P
                AUX06=SP1M2M*V3_4+SP1M3P*V2_4
                AUX07=AUX02*V3_4
                AUX08=AUX02*V2_4
!
!...............Computes the transfer term components
                S_2P3M=AUX03*AUX01-AUX02*AUX04
                S_2M3P=AUX05*AUX01-AUX02*AUX06
                Q_2P3M=T_2M3P*S_2P3M
                Q_2M3P=T_2P3M*S_2M3P
                AUX00 =Q_2P3M+Q_2M3P
!
!...............Computes the derived terms components (dQ/dF)
                Q2PD0 =T_2P3M*(AUX03*V1_4   - SP1M*AUX04)*CF0
                Q2PD1 =T_2P3M*(AUX03        - SP0 *AUX04)*CF1
                Q2PD2P=T_2P3M*(AUX01*SP1M3M - AUX07     )*CF2
                Q2PD3M=T_2P3M*(AUX01*SP1M2P - AUX08     )*CF3
                Q2MD0 =T_2M3P*(AUX05*V1_4   - SP1M*AUX06)*CF0
                Q2MD1 =T_2M3P*(AUX03        - SP0 *AUX06)*CF1
                Q2MD2M=T_2M3P*(AUX01*SP1M3P - AUX07     )*CF2
                Q2MD3P=T_2M3P*(AUX01*SP1M2M - AUX08     )*CF3
                AUX09=Q2PD0+Q2MD0
                AUX10=Q2PD1+Q2MD1
!
!...............Sum of Qnl4 term in the table T1TOT
                T1TOT(IP,JT    ,JFM0)=T1TOT(IP,JT    ,JFM0)+AUX00 *CP0
                T1TOT(IP,JT1M  ,JFM1)=T1TOT(IP,JT1M  ,JFM1)+AUX00 *CP1
                T1TOT(IP,JT1M2P,JFM2)=T1TOT(IP,JT1M2P,JFM2)-Q_2P3M*CP2
                T1TOT(IP,JT1M2M,JFM2)=T1TOT(IP,JT1M2M,JFM2)-Q_2M3P*CP2
                T1TOT(IP,JT1M3M,JFM3)=T1TOT(IP,JT1M3M,JFM3)-Q_2P3M*CP3
                T1TOT(IP,JT1M3P,JFM3)=T1TOT(IP,JT1M3P,JFM3)-Q_2M3P*CP3
!
!...............Sum of the term dQnl4/dF in the table T1DER
                T1DER(IP,JT    ,JFM0)=T1DER(IP,JT    ,JFM0)+AUX09 *CP0
                T1DER(IP,JT1M  ,JFM1)=T1DER(IP,JT1M  ,JFM1)+AUX10 *CP1
                T1DER(IP,JT1M2P,JFM2)=T1DER(IP,JT1M2P,JFM2)-Q2PD2P*CP2
                T1DER(IP,JT1M2M,JFM2)=T1DER(IP,JT1M2M,JFM2)-Q2MD2M*CP2
                T1DER(IP,JT1M3M,JFM3)=T1DER(IP,JT1M3M,JFM3)-Q2PD3M*CP3
                T1DER(IP,JT1M3P,JFM3)=T1DER(IP,JT1M3P,JFM3)-Q2MD3P*CP3
!
              ENDIF
!
            ENDDO
!           - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!           END OF LOOP 4 OVER THE MESH NODES
!           - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
          ENDDO
!         -------------------------------------------------
!         END OF LOOP 3 OVER THE SPECTRUM DIRECTIONS
!         -------------------------------------------------
!
        ENDDO
!       = = = = = = = = = = = = = = = = = = = = = = = = =
!       END OF LOOP 2 OVER THE SPECTRUM FREQUENCIES
!       = = = = = = = = = = = = = = = = = = = = = = = = =
!
      ENDDO
!     ==================================================
!     END OF LOOP 1 OVER THE SELECTED CONFIGURATIONS
!     ==================================================
!
      RETURN
      END
