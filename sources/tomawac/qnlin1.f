!                   *****************
                    SUBROUTINE QNLIN1
!                   *****************
!
     &( TSTOT , TSDER , IANGNL, NF    , NDIRE , NPOIN2, F     ,
     &  XKMOY , TAUX1 , TAUX2 , TAUX3 , TAUX4 , TAUX5 , DFINI )
!
!***********************************************************************
! TOMAWAC   V6P3                                   24/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE NON-LINEAR INTERACTIONS
!+                SOURCE TERM (FREQUENCY QUADRUPLETS) USING THE DIA METHOD
!+               ("DISCRETE INTERACTION APPROXIMATION") PROPOSED BY
!+                HASSELMANN AND HASSELMANN (1985).
!+
!+
!+            PROCEDURE SPECIFIC TO THE CASE WHERE THE FREQUENCIES
!+                FOLLOW A GEOMETRICAL PROGRESSION AND THE DIRECTIONS
!+                ARE EVENLY DISTRIBUTED OVER [0;2.PI].
!
!note     THIS SUBROUTINE USES THE OUTPUT FROM 'PRENL1' TO OPTIMISE
!+          THE COMPUTATIONS FOR DIA.
!
!reference  HASSELMANN S., HASSELMANN K. ET AL.(1985) :
!+                     "COMPUTATIONS AND PARAMETERIZATIONS OF THE NONLINEAR
!+                      ENERGY TRANSFER IN GRAVITY-WAVE SPECTRUM. PART1 :
!+                      A NEW METHOD FOR EFFICIENT COMPUTATION OF THE EXACT
!+                      NON-LINEAR TRANSFER INTEGRAL". JPO, VOL 15, PP 1369-1377.
!reference HASSELMANN S., HASSELMANN K. ET AL.(1985) :
!+                     "COMPUTATIONS AND PARAMETERIZATIONS OF THE NONLINEAR
!+                      ENERGY TRANSFER IN GRAVITY-WAVE SPECTRUM. PART2 :
!+                      PARAMETERIZATIONS OF THE NONLINEAR ENERGY TRANSFER
!+                      FOR APPLICATION IN WAVE MODELS". JPO, VOL 15, PP 1378-1391.
!
!history  M. BENOIT
!+        07/06/95
!+        V1P0
!+   CREATED
!
!history  M. BENOIT
!+        26/06/96
!+        V1P2
!+   MODIFIED
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
!+        24/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COEFNL         |-->| COEFFICIENTS USED FOR DIA METHOD
!| DEPTH          |-->| WATER DEPTH
!| DFINI          |<->| WORK TABLE
!| F              |-->| DIRECTIONAL SPECTRUM
!| F1             |-->| FIRST DISCRETIZED FREQUENCY
!| IANGNL         |-->| ANGULAR INDICES TABLE
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| RAISF          |-->| FREQUENTIAL RATIO
!| TAUX1          |<->| WORK TABLE
!| TAUX2          |<->| WORK TABLE
!| TAUX3          |<->| WORK TABLE
!| TAUX4          |<->| WORK TABLE
!| TAUX5          |<->| WORK TABLE
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| XKMOY          |-->| AVERAGE WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_QNLIN1 => QNLIN1
      USE DECLARATIONS_TOMAWAC, ONLY : DEPTH, PROINF, F1, RAISF, COEFNL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NDIRE,NF
      INTEGER, INTENT(IN)             :: IANGNL(NDIRE,8)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XKMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX1(NPOIN2),TAUX2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX3(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX4(NPOIN2),TAUX5(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DFINI(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  JBP0  , JFP0  , JFP1  , JFM0  , JFM1  , JFP   , JFM
      INTEGER  JBP1  , JB    , JBM0  , JBM1  , IMAGE , JP
      INTEGER  JPP0  , JPP1  , JPM0  , JPM1  , IP    , KAUX  , JF
      INTEGER  JFMIN , JFMAX
      DOUBLE PRECISION COEFP0, COEFP1, COEFM0, COEFM1, COEFJF, XXFAC
      DOUBLE PRECISION FMOIN , FPLUS , TERM1 , TERM2 , US1PL4, US1ML4
      DOUBLE PRECISION C1    , C2    , C3    , C4    , C5    , C6
      DOUBLE PRECISION D1    , D2    , D3    , D4    , D5    , D6
      DOUBLE PRECISION C1SQ  , C2SQ  , C3SQ  , C4SQ  , C5SQ  , C6SQ
      DOUBLE PRECISION C7    , C8    , D7    , D8    , C7SQ  , C8SQ
      DOUBLE PRECISION TERM3 , FDEJF , FREQ
!
!-----------------------------------------------------------------------
!
!     RECOVERS THE COEFFICIENTS COMPUTED IN 'PRENL1'
!
      C1    = COEFNL( 1)
      C2    = COEFNL( 2)
      C3    = COEFNL( 3)
      C4    = COEFNL( 4)
      C5    = COEFNL( 5)
      C6    = COEFNL( 6)
      C7    = COEFNL( 7)
      C8    = COEFNL( 8)
      JFP   = INT(COEFNL( 9)+1.D-7)
      JFM   = INT(COEFNL(10)-1.D-7)
      US1PL4= COEFNL(11)
      US1ML4= COEFNL(12)
      JFMIN = NINT(COEFNL(13))
      JFMAX = NINT(COEFNL(14))
      C1SQ  = C1**2
      C2SQ  = C2**2
      C3SQ  = C3**2
      C4SQ  = C4**2
      C5SQ  = C5**2
      C6SQ  = C6**2
      C7SQ  = C7**2
      C8SQ  = C8**2
!
!     CORRECTION FACTOR FOR FINITE WATER DEPTH
!
      IF(.NOT.PROINF) THEN
        DO IP=1,NPOIN2
          TERM1 = MAX(0.75D0*DEPTH(IP)*XKMOY(IP),0.5D0)
          DFINI(IP) = 1.D0+(5.5D0/TERM1)*(1.D0-0.833D0*TERM1)
     &               /EXP(MIN(1.25D0*TERM1,7.D2))
        ENDDO
      ENDIF
!
!     FIRST LOOP ON THE FREQUENCIES
!
      DO JF=JFMIN,JFMAX
!
!       COMPUTES THE CONSIDERED FREQUENCY
!
        FREQ = F1*RAISF**(JF-1)
!
!       GETS THE INDICES OF THE FREQUENCIES EITHER SIDE OF THE
!       'MAX' FREQUENCY: FREQ(JFP0)
!
        JFP0=JF+JFP
        JFP1=JFP0+1
!
!       GETS THE INDICES OF THE FREQUENCIES EITHER SIDE OF THE
!       'MIN' FREQUENCY: FREQ(JFM0)
!
        JFM0=JF+JFM-1
        JFM1=JFM0+1
!
!       LIMITS THE INDICES TO NF AND TAKES INTO ACCOUNT ANALYTICALLY
!       THE SPECTRUM TAIL (DECREASE IN -TAILF).
!
        CALL CQUEUE( JFP1 , JBP1 , COEFP1 )
        CALL CQUEUE( JFP0 , JBP0 , COEFP0 )
        CALL CQUEUE( JF   , JB   , COEFJF )
        CALL CQUEUE( JFM1 , JBM1 , COEFM1 )
        CALL CQUEUE( JFM0 , JBM0 , COEFM0 )
!
!       INTERPOLATION COEFFICIENTS FOR THE MODIFIED SPECTRUM
!
        D1=C1*COEFP0*US1PL4
        D2=C2*COEFP0*US1PL4
        D3=C3*COEFP1*US1PL4
        D4=C4*COEFP1*US1PL4
        D5=C5*COEFM0*US1ML4
        D6=C6*COEFM0*US1ML4
        D7=C7*COEFM1*US1ML4
        D8=C8*COEFM1*US1ML4
!
!       COMPUTES THE MULTIPLICATIVE COEFFICIENT (IN F**11) AND TAKES
!       INTO ACCOUNT THE CORRECTION TERM IN FINITE DEPTH
!
        XXFAC= 3000.D0*FREQ**11
        IF(PROINF) THEN
          DO IP=1,NPOIN2
            TAUX1(IP) = XXFAC
          ENDDO
        ELSE
          DO IP=1,NPOIN2
            TAUX1(IP) = DFINI(IP)*XXFAC
          ENDDO
        ENDIF
!
!       SECOND LOOP ON ANGULAR SYMMETRY
!
        DO IMAGE=1,2
!
          KAUX=(IMAGE-1)*4
!
!         THIRD LOOP ON THE DIRECTIONS
!
          DO JP=1,NDIRE
!
            JPP0 = IANGNL(JP,KAUX+1)
            JPP1 = IANGNL(JP,KAUX+2)
            JPM0 = IANGNL(JP,KAUX+3)
            JPM1 = IANGNL(JP,KAUX+4)
!
            IF (JFM0.LT.1) THEN
!
!........./-------------------------------------------------------/
!........./ AT LEAST ONE OF THE FREQUENCIES IS LOWER THAN FREQ(1) /
!........./ THE SPECTRUM F- WITH FREQUENCY (1-XLAMD).FREQ IS ZERO /
!........./-------------------------------------------------------/
!
              DO IP=1,NPOIN2
                FDEJF = F(IP,JP,JB )*COEFJF
                FPLUS = F(IP,JPP0,JBP0)*D1 + F(IP,JPP1,JBP0)*D2
     &                + F(IP,JPP0,JBP1)*D3 + F(IP,JPP1,JBP1)*D4
!
                TERM1 = FDEJF*FPLUS
                TERM3 = TAUX1(IP)*FDEJF
!
                TAUX2(IP) = TERM1*TERM3
                TAUX3(IP) = 2.D0*TERM1*TAUX1(IP)
                TAUX5(IP) = FDEJF*US1PL4*TERM3
              ENDDO ! IP
!
              IF (JB.EQ.JF) THEN
!
                DO IP=1,NPOIN2
                  TSTOT(IP,JP  ,JF  )=TSTOT(IP,JP  ,JF  )-TAUX2(IP)*2.D0
                  TSDER(IP,JP  ,JF  )=TSDER(IP,JP  ,JF  )-TAUX3(IP)*2.D0
                ENDDO ! IP
!
                IF (JBP0.EQ.JFP0) THEN
!
                  DO IP=1,NPOIN2
                    TSTOT(IP,JPP0,JFP0)=TSTOT(IP,JPP0,JFP0)+TAUX2(IP)*C1
                    TSTOT(IP,JPP1,JFP0)=TSTOT(IP,JPP1,JFP0)+TAUX2(IP)*C2
                    TSDER(IP,JPP0,JFP0)=TSDER(IP,JPP0,JFP0)
     &                                 +TAUX5(IP)*C1SQ
                    TSDER(IP,JPP1,JFP0)=TSDER(IP,JPP1,JFP0)
     &                                 +TAUX5(IP)*C2SQ
                  ENDDO ! IP
!
                  IF (JBP1.EQ.JFP1) THEN
!
                    DO IP=1,NPOIN2
                      TSTOT(IP,JPP0,JFP1)=TSTOT(IP,JPP0,JFP1)
     &                                   +TAUX2(IP)*C3
                      TSTOT(IP,JPP1,JFP1)=TSTOT(IP,JPP1,JFP1)
     &                                   +TAUX2(IP)*C4
                      TSDER(IP,JPP0,JFP1)=TSDER(IP,JPP0,JFP1)
     &                                   +TAUX5(IP)*C3SQ
                      TSDER(IP,JPP1,JFP1)=TSDER(IP,JPP1,JFP1)
     &                                   +TAUX5(IP)*C4SQ
                    ENDDO ! IP
!
                  ENDIF
                ENDIF
              ENDIF
!
            ELSE
!
!........./--------------------------------------------------------/
!........./ FREQUENCIES F-, F, F+ MAY HAVE ENERGY                  /
!........./--------------------------------------------------------/
!
              DO IP=1,NPOIN2
                FDEJF = F(IP,JP,JB )*COEFJF
                FPLUS = F(IP,JPP0,JBP0)*D1 + F(IP,JPP1,JBP0)*D2
     &                + F(IP,JPP0,JBP1)*D3 + F(IP,JPP1,JBP1)*D4
                FMOIN = F(IP,JPM0,JBM0)*D5 + F(IP,JPM1,JBM0)*D6
     &                + F(IP,JPM0,JBM1)*D7 + F(IP,JPM1,JBM1)*D8
!
                TERM1 = FDEJF*(FPLUS+FMOIN)
                TERM2 = 2.D0*FPLUS*FMOIN
                TERM3 = TAUX1(IP)*FDEJF
!
                TAUX2(IP) = (TERM1-TERM2)*TERM3
                TAUX3(IP) = (2.D0*TERM1-TERM2)*TAUX1(IP)
                TAUX5(IP) = (FDEJF-2.D0*FMOIN)*US1PL4*TERM3
                TAUX4(IP) = (FDEJF-2.D0*FPLUS)*US1ML4*TERM3
              ENDDO ! IP
!
              IF (JBM0.EQ.JFM0) THEN
!
                DO IP=1,NPOIN2
                  TSTOT(IP,JPM0,JFM0)=TSTOT(IP,JPM0,JFM0)+TAUX2(IP)*C5
                  TSTOT(IP,JPM1,JFM0)=TSTOT(IP,JPM1,JFM0)+TAUX2(IP)*C6
                  TSDER(IP,JPM0,JFM0)=TSDER(IP,JPM0,JFM0)+TAUX4(IP)*C5SQ
                  TSDER(IP,JPM1,JFM0)=TSDER(IP,JPM1,JFM0)+TAUX4(IP)*C6SQ
                ENDDO ! IP
!
                IF (JBM1.EQ.JFM1) THEN
!
                  DO IP=1,NPOIN2
                    TSTOT(IP,JPM0,JFM1)=TSTOT(IP,JPM0,JFM1)+TAUX2(IP)*C7
                    TSTOT(IP,JPM1,JFM1)=TSTOT(IP,JPM1,JFM1)+TAUX2(IP)*C8
                    TSDER(IP,JPM0,JFM1)=TSDER(IP,JPM0,JFM1)
     &                                 +TAUX4(IP)*C7SQ
                    TSDER(IP,JPM1,JFM1)=TSDER(IP,JPM1,JFM1)
     &                                 +TAUX4(IP)*C8SQ
                  ENDDO ! IP
!
                  IF (JB.EQ.JF) THEN
!
                    DO IP=1,NPOIN2
                      TSTOT(IP,JP  ,JF  )=TSTOT(IP,JP  ,JF  )
     &                                   -TAUX2(IP)*2.D0
                      TSDER(IP,JP  ,JF  )=TSDER(IP,JP  ,JF  )
     &                                   -TAUX3(IP)*2.D0
                    ENDDO ! IP
!
                    IF (JBP0.EQ.JFP0) THEN
!
                      DO IP=1,NPOIN2
                        TSTOT(IP,JPP0,JFP0)=TSTOT(IP,JPP0,JFP0)
     &                                     +TAUX2(IP)*C1
                        TSTOT(IP,JPP1,JFP0)=TSTOT(IP,JPP1,JFP0)
     &                                     +TAUX2(IP)*C2
                        TSDER(IP,JPP0,JFP0)=TSDER(IP,JPP0,JFP0)
     &                                     +TAUX5(IP)*C1SQ
                        TSDER(IP,JPP1,JFP0)=TSDER(IP,JPP1,JFP0)
     &                                     +TAUX5(IP)*C2SQ
                      ENDDO ! IP
!
                      IF (JBP1.EQ.JFP1) THEN
!
                        DO IP=1,NPOIN2
                          TSTOT(IP,JPP0,JFP1)=TSTOT(IP,JPP0,JFP1)
     &                                       +TAUX2(IP)*C3
                          TSTOT(IP,JPP1,JFP1)=TSTOT(IP,JPP1,JFP1)
     &                                       +TAUX2(IP)*C4
                          TSDER(IP,JPP0,JFP1)=TSDER(IP,JPP0,JFP1)
     &                                       +TAUX5(IP)*C3SQ
                          TSDER(IP,JPP1,JFP1)=TSDER(IP,JPP1,JFP1)
     &                                       +TAUX5(IP)*C4SQ
                        ENDDO ! IP
!
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
!
            ENDIF
!
          ENDDO ! JP
!
        ENDDO ! IMAGE
!
      ENDDO ! JF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
