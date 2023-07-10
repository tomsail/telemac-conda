!                   *****************
                    SUBROUTINE CALCMU
!                   *****************
     &(ITERMU)
!
!
!***********************************************************************
! ARTEMIS   V7P3                                     Aug 2017
!***********************************************************************
!
!brief    COMPUTES DISSIPATION COEFFICIENT FOR RANDOM SEAS
!+               (RANDOM SEAS) :
!+                    MU : DISSIP COEFF
!
!history  C PEYRARD (LNHE)
!+        27/03/2014
!+        V7P0
!+   CREATION OF THE ROUTINE
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  N.DURAND (HRW)
!+        August 2017
!+        V7P3
!+   PI now defined in DECLARATIONS_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ITERMU             |-->| INDICE OF THE CURRENT CALCULATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ITERMU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION HEFF,Q1,Q2,Q3,HM,FFW,HMUE
!
!-----------------------------------------------------------------------
!
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
! K   = MEAN WAVE NUMBER
! C   = MEAN PHASE CELERITY
! CG  = MEAN GROUP CELERITY
! HALE= SIGNIFICANT WAVE HEIGHT
!
!
!     ============================================================
!
!     COMPUTES THE TOTAL DISSIPATION COEFFICIENT MU_DEFERL + MU_FROTTE
!                                                  (MU2)       (T1)
!     IF BREAKING OR BOTTOM FRICTION TAKEN INTO ACCOUNT
!     ============================================================
!
!
!      ECRHMU = 0.D0
!
!     --------------------------------------------
!     INITIALISES MU2 AND T3: SET TO 0
!     MU2: NEW DISSIPATION COEFFICIENT
!     T3: QB FOR THE CURRENT PERIOD
!     --------------------------------------------
!
      CALL OS('X=C     ', X=MU2, C=0.D0)
      CALL OS('X=C     ', X=T3 , C=0.D0)
      CALL OS('X=N(Y,Z)', X=T1 , Y=PHIR , Z=PHII)
!
!        ----------------------------------------------------
!        COMPUTES THE WAVE HEIGHT HMU CORRESPONDING TO
!        THE SOLUTION OF THE SYSTEM
!
      IF ((.NOT. ALEMON).AND.(.NOT. ALEMUL)) THEN
!       REGULAR WAVE HEIGHT GIVEN BY THE POTENTIAL
        IF (COURANT) THEN
!         WE USE WR TO COMPUTE THE FREE SURFACE
          CALL OS( 'X=CY    ', X=T2   ,Y=WR, C=2.D0/GRAV)
          CALL OS( 'X=YZ    ', X=HHO  ,Y=T1, Z=T2 )
        ELSE
!         WE USE OMEGA TO COMPUTE THE FREE SURFACE
          CALL OS( 'X=CY    ', X=HMU , Y=T1   , C=2.D0*OMEGA/GRAV)
        ENDIF
      ELSE
!       IRREGULAR WAVE HEIGHT : HALE=SIGNIFICANT WAVE HEIGHT
        CALL OS( 'X=CY    ', X=HMU , Y=HALE   , C=1.D0)
      ENDIF
!
!     --------------
!     IF BREAKING
!     --------------
!
      IF (DEFERL) THEN
!
!     ------------------------------------------------------
!     TESTS IF HMU > HM (THERE IS BREAKING) OR NOT,
!     AND CALCULATES MU2 ACCORDING TO DALLY OR BATTJES & JANSSEN
!     (IF REGULAR WAVES)
!     ------------------------------------------------------
!
        IF ((.NOT. ALEMON).AND.(.NOT. ALEMUL)) THEN
          DO I = 1,NPOIN
            HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
!
!           HMUE = HMU/SQRT(2)
!           (ENERGETIC WAVE HEIGHT)
            HMUE = HMU%R(I)/1.4142D0
            HEFF=MIN(HMUE,HM)
            HEFF=MAX(HEFF,1.D-5)
            Q1 = 1.D-10
            Q2 = (HEFF/HM)**2
!           ADDED BECAUSE OF THE LOG FUNCTION, LATER ON
            Q2 = MAX(Q2,1.D-9)
!
!           ------------
!           COMPUTES QB
!           ------------
!
            CALL CALCQB(Q1,Q2,Q3)
!
!           ALGORITHM SPECIFIC TO REGULAR WAVES
!           FOR THE COMPUTATION OF THE RATE OF BREAKING
!
            IF (ITERMU.EQ.0) THEN
              IF (Q3.LT.0.19D0) THEN
                T3%R(I) = 0.D0
              ELSE
                T3%R(I) = 1.D0
              ENDIF
!
!             T3 COMPUTED AT ITERMU = 0
!             IS TEMPORARILY STORED IN QB
!
              QB%R(I) = T3%R(I)
            ELSE
              IF (QB%R(I).EQ.1.D0) THEN
                IF (Q3.LT.0.1D0) THEN
                  T3%R(I) = 0.D0
                ELSE
                  T3%R(I) = 1.D0
                ENDIF
              ENDIF
            ENDIF
          ENDDO
!
!
!         --------------------------------
!         DALLY AND AL 1985
!         --------------------------------
!
          IF (IBREAK.EQ.2) THEN
            DO I = 1,NPOIN
              HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
              HEFF=MIN(HMU%R(I),HM)
              HEFF=MAX(HEFF,1.D-5)
              MU2%R(I)=T3%R(I)*KDALLY*
     &              (1.D0-(GDALLY*H%R(I)/HEFF)**2)/H%R(I)
            ENDDO
          ENDIF
!
!     -------------------------------------
!     BATTJES & JANSSEN 1978
!     -------------------------------------
!
          IF (IBREAK.EQ.1) THEN
            DO I = 1,NPOIN
              HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
              HEFF=MIN(HMU%R(I),HM)
              HEFF=MAX(HEFF,1.D-5)
              MU2%R(I) = T3%R(I)*2.D0*HEFF/(H%R(I)*CG%R(I)*PER)
            ENDDO
          ENDIF
!
!     -------------------------------------------------------------
!     IRREGULAR WAVES :
!     COMPUTES FIRST QB=T3, PROPORTION OF BREAKING OR BROKEN WAVES,
!     THEN MU2 ACCORDING TO B&J 78 (RANDOM SEAS)
!     -------------------------------------------------------------
!
        ELSE
          DO I = 1,NPOIN

            HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
!
!           HMUE = HMU/SQRT (2)
!           (ENERGETIC WAVE HEIGHT)
            HMUE = HMU%R(I)/1.4142D0
            HEFF=MIN(HMUE,HM)
            HEFF=MAX(HEFF,1.D-5)
            Q1 = 1.D-10
            Q2 = (HEFF/HM)**2
!           ADDED BECAUSE OF THE LOG FUNCTION, LATER ON
            Q2 = MAX(Q2,1.D-9)
!
!           ------------
!           COMPUTES QB
!           ------------
!
            CALL CALCQB(Q1,Q2,Q3)
            T3%R(I) = Q3
!
!           -------------------------
!           COMPUTES MU2
!           -------------------------
!
            HEFF = MIN((HMU%R(I)/1.4142D0),HM)
            HEFF=MAX(HEFF,1.D-5)
            MU2%R(I)=ALFABJ*OMEGAM%R(I)*T3%R(I)*((HM/HEFF)**2)/
     &             (PI*CG%R(I))
          ENDDO
!
!         --------------------------------
!          STOCK QB FOR IRREGULAR WAVES
!         --------------------------------
          CALL OS( 'X=Y     ', X=QB,Y=T3)
!
        ENDIF
!
!     ------------------
!     END 'IF BREAKING'
!     ------------------
!
      ENDIF
!
!     --------------------------------
!     RE-INITIALISES T1 = 0 BECAUSE
!     T1 REPRESENTS MU_FROTTEMENT IN THE FOLLOWING
!     --------------------------------
!
      CALL OS( 'X=C     ' , X=T1, C=0.D0 )
!
!     ---------------------
!     IF BOTTOM FRICTION
!     ---------------------
!
      IF (FROTTE) THEN
!
!       ------------------------------------------------
!       IF ENTFW=TRUE, THE FRICTION COEFFICIENT FW
!       IS THE SAME EVERYWHERE IN THE DOMAIN
!       ------------------------------------------------
!
        IF (ENTFW) THEN
          CALL FWSPEC(FW%R,FWCOEF,MESH%X%R,MESH%Y%R,
     &                     NPOIN,PRIVE,ZF%R)
        ELSE
          IF ((.NOT. ALEMON).AND.(.NOT. ALEMUL)) THEN
            DO I = 1,NPOIN
              CALL CALCFW
     &                   (I,H%R,K%R,HMU%R,
     &                    NPOIN,OMEGA,GRAV,
     &                    VISCO,DIAM90,DIAM50,MVSED,MVEAU,
     &                    REGIDO,RICOEF,
     &                    ENTREG,ENTRUG,FFW)
              FW%R(I) = FFW
            ENDDO
          ELSE
            DO I = 1,NPOIN
              CALL CALCFW
     &           (I,H%R,K%R,HMU%R,
     &            NPOIN,OMEGAM%R(I),GRAV,
     &            VISCO,DIAM90,DIAM50,MVSED,MVEAU,
     &            REGIDO,RICOEF,
     &            ENTREG,ENTRUG,FFW)
              FW%R(I) = FFW
            ENDDO
          ENDIF
        ENDIF
!
!       -----------------------------------------
!       COMPUTES THE DISSIPATION COEFFICIENT FOR
!       BOTTOM FRICTION
!       -----------------------------------------
!
!
        IF (FORMFR .EQ. 1) THEN
!         ---------------------------------------------------
!         COMPUTES AN EFFECTIVE SPEED
!         UE = 1.2D0*(0.5*((DPHIR/DX)**2 + (DPHIR/DY)**2
!                     +(DPHII/DX)**2 + (DPHII/DY)**2))**0.5
!         UE IS STORED IN T4 HERE
!         ---------------------------------------------------
!
          IF ((.NOT. ALEMON).AND.(.NOT. ALEMUL)) THEN
            CALL CALCUE
!                   WORK TABLE MODIFIED : T1,T2
!                   RESULTS WORK TABLE  : T4
          ELSE
            CALL OS( 'X=Y     ', X=T4 , Y=UEB )
          ENDIF
!
!         ----------------------------------------
!         THE DISSIPATION COEFFICIENT MU FOR
!         FRICTION IS STORED IN T1
!         ----------------------------------------
!
          CALL OS( 'X=C     ' , X=T1 , C=0.D0 )
!
          DO I = 1,NPOIN
            T1%R(I) = (0.5D0*FW%R(I)*T4%R(I))/
     &                (H%R(I)*((COSH(K%R(I)*H%R(I)))**2))
            T1%R(I) = T1%R(I)/CG%R(I)
          ENDDO
        ENDIF
!
        IF (FORMFR .EQ. 2) THEN
!         ---------------------------------------------------
!         COMPUTES BOTTOM SPEED FROM STOKES THEORY
!         ---------------------------------------------------
          CALL OS( 'X=C     ' , X=T1 , C=0.D0 )
!
          IF ((.NOT. ALEMON).AND.(.NOT. ALEMUL)) THEN
            DO I = 1,NPOIN
              T1%R(I) = (2*FW%R(I)*HMU%R(I)*
     &                ((OMEGA/SINH(K%R(I)*H%R(I)))**3))
              T1%R(I) = T1%R(I)/(3.D0*PI*GRAV)
              T1%R(I) = T1%R(I)/CG%R(I)
            ENDDO
          ELSE
            CALL OS('X=Y     ', X=T4 , Y=UEB)
            DO I = 1,NPOIN
!  FOR MEMORY : DIRECT CALCULATION OF MU BASED ON MEAN OMEGA
!  CP suspects that th(kD)=kD approximation was used here
              T1%R(I) = (0.5D0*FW%R(I)*T4%R(I))/
     &                (H%R(I)*((COSH(K%R(I)*H%R(I)))**2))
              T1%R(I) = T1%R(I)/CG%R(I)
            ENDDO
          ENDIF
        ENDIF
!
!     -------------------------
!     END 'IF BOTTOM FRICTION'
!     -------------------------
!
      ENDIF
!
      DO I = 1,NPOIN
!       --------------------------
!       MU = MU_DEFERL + MU_FROTTE
!       --------------------------
!
        MU2%R(I) = MU2%R(I) + T1%R(I)
      ENDDO
!
!
! END OF THE CALCULATION OF THE DISSIPATION TERM MU
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
