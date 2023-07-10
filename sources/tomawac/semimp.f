!                   *****************
                    SUBROUTINE SEMIMP
!                   *****************
!
     & (F    , CF   ,  XK   ,  NF   , NDIRE, NPOIN2,
     &  IANGNL,TSTOT,  TSDER,  TOLD , TNEW,  Z0NEW, TWNEW,
     &  TAUX1, TAUX2,  TAUX3,  TAUX4, TAUX5, TAUX6, TAUX7,
     &  MDIA,  IANMDI, COEMDI, FBOR, PART)
!
!***********************************************************************
! TOMAWAC   V7P3
!***********************************************************************
!
!brief    SOLVES THE INTEGRATION STEP OF THE SOURCE TERMS USING
!+                A SCHEME WITH VARIABLE DEGREE OF IMPLICITATION.
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
!history  E.G.RENOU (EDF), G.MATTAROLO (EDF)
!+        12/05/2011
!+        V6P1
!+   MODIFIED: integration of new source terms, developed by
!+   E.G. Renou.
!+     - modification of the variables in argument list
!+     - modification of the local variable declarations
!+     - modification concerning friction velocity and roughness
!+       length calculation
!+     - calls to subroutines QWINDL, QWIND3, QMOUT2, QNLIN2,
!+       QNLIN3
!
!history  G.MATTAROLO (EDF - LNHE)
!+        27/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  U.H.MERKEL
!+        27/06/2012
!+        V6P2
!+   Renamed SUM to SOMME, due to NAG compiler
!
!history  E. GAGNAIRE-RENOU (EDF - LNHE)
!+        12/03/2013
!+        V6P3
!+   HF diagnostic tail is not necessarily imposed
!
!history  VITO BACCHI (EDF - LNHE)
!+        12/09/2014
!+        V7P0
!+   Friction due to vegetation added.
!
!history THIERRY FOUQUET (EDF-LNHE)
!+       19/11/2014
!+       V7P0
!+   BAJ MODELING
!
!history A JOLY (EDF-LNHE)
!+       18/05/2017
!+       V7P3
!+   New condition to stop source terms being added to open
!+   boundaries.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| ADVECTION FIELD ALONG FREQUENCY
!| COEMDI         |-->| COEFFICIENTS USED FOR MDIA METHOD
!| F              |<->| DIRECTIONAL SPECTRUM
!| FBOR           |-->| SPECTRAL VARIANCE DENSITY AT THE BOUNDARIES
!| FMOY           |<--| MEAN FREQUENCIES F-10
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| IANGNL         |-->| ANGULAR INDICES TABLE
!| IANMDI         |-->| ANGULAR INDICES TABLE FOR MDIA
!| MDIA           |-->| NUMBER OF CONFIGURATIONS FOR MDIA METHOD
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NF             |-->| NUMBER OF FREQUENCIES
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| TAUX1          |<->| WORK TABLE
!| TAUX2          |<->| WORK TABLE
!| TAUX3          |<->| WORK TABLE
!| TAUX4          |<->| WORK TABLE
!| TAUX5          |<->| WORK TABLE
!| TAUX6          |<->| WORK TABLE
!| TAUX7          |<->| WORK TABLE
!| TNEW           |<->| WORK TABLE
!| TOLD           |<->| WORK TABLE
!| TSDER          |<--| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<--| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| TWNEW          |<->| WIND DIRECTION AT TIME N+1
!| USNEW          |<->| FRICTION VELOCITY AT TIME N+1
!| VARIAN         |-->| SPECTRUM VARIANCE
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKMOY          |<--| AVERAGE WAVE NUMBER
!| Z0NEW          |<->| SURFACE ROUGHNESS LENGTH AT TIME N+1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,T3_01,T3_02,TEXVEB,
     &                                 NAMEWX,NAMEWY,UNITVEB,PHASVEB,
     &                                 SOURCE_ON_BND,DEBUG,CG,POROUS,
     &                                 AMORP,LT, GRAVIT, CIMPLI,
     &                                 IFRBJ, IFRTG , IFRRO, IFRIH,
     &                                 DIAGHF, COEFHS, NSITS,  SMOUT,
     &  FMOY, VARIAN, XKMOY, SFROT, SVENT, LVENT, STRIF, VENT, VENSTA,
     &  LUVEF,  LUVEB, NAMVEF, NAMVEB, FMTVEB, FMTVEF,
     &  SBREK,  XDTBRK, NDTBRK, STRIA, PROINF, DF_LIM, LIMIT,
     &  VEGETATION, CBAJ, SDSCU,FREQ, DFREQ, DEPTH,
     &  TETA, TAILF, RAISF, NBOR, NPTFR, LIFBOR,
     &  TV1   ,TV2   , UV1, UV2, VV1, VV2, UV, VV,
     &  TAUWAV,USOLD, USNEW, TWOLD, Z0OLD, AT, DTSI, INDIV, NVWIN,
     &  WAC_CPL_RUN
      
! FROM DECLARATION_TOMAWAC :
! DIAGHF          OPTION FOR SPECTRUM DIAGNOSTIC TAIL
! CBAJ            CHOICE OF THE CENTRAL FREQUENCY CALCULUS
! COEFHS          MAXIMUM VALUE OF THE RATIO HM0 ON D
! DF_LIM          WORK TABLE
! DTSI            INTEGRATION TIME STEP (SECONDS)
! F1              MINIMAL DISCRETIZED FREQUENCY
! INDIV           WIND FILE FORMAT
! LIMIT           TYPE OF WAVE GROWTH LIMITER MODEL SELECTED
! LVENT           LINEAR WAVE GROWTH MODEL SELECTION
! NDTBRK          NUMBER OF TIME STEPS FOR BREAKING SOURCE TERM
! NSITS           NUMBER OF ITERATIONS FOR THE SOURCE TERMS
! PROINF          LOGICAL INDICATING INFINITE DEPTH ASSUMEPTION
! RAISF           FREQUENTIAL RATIO
! SBREK           DEPTH-INDUCED BREAKING DISSIPATION MODEL
! SDSCU           DISSIPATION BY STRONG CURRENT
! SMOUT           SELECTION OF WHITE CAPPING SOURCE TERM MODEL
! STRIA           SELECTION OF THE TRIAD INTERACTION MODEL
! STRIF           SELECTION OF QUADRUPLET INTERACTION MODEL
! SFROT           SELECTION OF THE BOTTOM FRICTION DISSIPATION
! SVENT           SELECTION OF THE WIND GENERATION MODEL
! TAILF           SPECTRUM TAIL FACTOR
! TAUWAV          STRESS DUE TO THE WAVES
! TETA            DISCRETIZED DIRECTIONS
! TWOLD           WIND DIRECTION AT TIME N
! TNEW            WORK TABLE
! TOLD            WORK TABLE
! TV1             TIME T1 IN THE WIND FILE
! TV2             TIME T2 IN THE WIND FILE
! USOLD           FRICTION VELOCITY AT TIME N
! U1,V1           WIND SPEED AT TIME T1 IN THE WIND FILE
! U2,V2           WIND SPEED AT TIME T2 IN THE WIND FILE
! UV,VV           WIND DATA INTERPOLATED OVER 2D MESH
! VEGETATION      IF YES, VEGETATION TAKEN INTO ACCOUNT
! VENSTA          INDICATES IF THE WIND IS STATIONARY
! VENT            INDICATES IF WIND IS TAKEN INTO ACC
! XDTBRK          COEFFICIENT OF TIME SUB-INCREMENTS FOR BREAKING
! Y               ORDINATES OF POINTS IN THE MESH
! Z0OLD           SURFACE ROUGHNESS LENGTH AT TIME N
      USE INTERFACE_TOMAWAC, EX_SEMIMP => SEMIMP
!
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2,NDIRE,NF, IANGNL(*)
      DOUBLE PRECISION, INTENT(INOUT) :: Z0NEW(NPOIN2), TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX1(NPOIN2), TAUX2(NPOIN2),
     &                                   TAUX3(NPOIN2), TAUX4(NPOIN2),
     &                                   TAUX5(NPOIN2), TAUX6(NPOIN2),
     &                                   TAUX7(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TOLD(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: TNEW(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN) :: CF(*)
!....MDIA method declarations
      INTEGER, INTENT(IN) :: MDIA, IANMDI(*)
      DOUBLE PRECISION, INTENT(IN) ::  COEMDI(*)
      DOUBLE PRECISION, INTENT(IN)   :: FBOR(NPTFR,NDIRE,NF)
      INTEGER,          INTENT(IN)      :: PART
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISITS,IFF,IP,JP,K,NVENT,IFCAR,MF1,MF2,MFMAX,IDT
      INTEGER IPTFR
      DOUBLE PRECISION AUX1,AUX2,AUX3,AUX4,COEF
      DOUBLE PRECISION FM1,FM2,TDEB,TFIN,VITVEN
      DOUBLE PRECISION VITMIN,HM0LOC,HM0MAX,DTN,SOMM,AUXI,USMIN
!                              MDIA, HERE HARDCODED
      DOUBLE PRECISION  XCCMDI(4)
!
      LOGICAL TROUVE(3)
!
      DOUBLE PRECISION  CPHAS , SEUILF
!
      CHARACTER(LEN=8) FMTVEN
!
!-----------------------------------------------------------------------
!
      VITMIN=1.D-3
!
!     ------------------------------------------------------------------
!     CHOPS THE SPECTRUM IN ACCORDANCE WITH THE BATHYMETRY
!     -----------------------------------------------------------------
!
      IF(.NOT.PROINF) THEN
!
!       0.1 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
!       -----------------------------------------------
!
        IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE TOTNRJ'
        CALL TOTNRJ(VARIAN, F, NF, NDIRE, NPOIN2)
        IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE TOTNRJ'
!
!       0.2 COMPUTES THE CORRECTION COEFFICIENT ON THE SPECTRUM
!       -------------------------------------------------------
!
        DO IP=1,NPOIN2
          HM0MAX=COEFHS*DEPTH(IP)
          HM0LOC =MAX(4.D0*SQRT(VARIAN(IP)),1.D-20)
          TAUX1(IP)=MIN((HM0MAX/HM0LOC)**2,1.D0)
        ENDDO
!
!       0.3 CORRECTS THE SPECTRUM
!       --------------------------
!
        DO IFF=1,NF
          DO JP=1,NDIRE
            DO IP=1,NPOIN2
              F(IP,JP,IFF)=F(IP,JP,IFF)*TAUX1(IP)
            ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!     ----------------------------------------------------------------
!     IF THE COMPUTATION INCLUDES STATIONARY WINDS, DUPLICATES THE
!     CONDITIONS AT THE START OF THE TIME STEP TO THE END OF THE TIME
!     STEP. (THIS IS BECAUSE ARRAYS TWNEW, USNEW AND Z0NEW ARE WORKING
!     ARRAYS USED IN DUMP2D BETWEEN 2 CALLS TO SEMIMP).
!
!     TODO: QUESTION JMH 16/09/2014 : WHAT IF VENT=.FALSE. AND USNEW USED
!     IN BETWEEN ???? IT WOULD BE SAFER TO HAVE USOLD AND USNEW
!     WITH THIS NAME AND ONLY USED FOR THIS...
!
!     ----------------------------------------------------------------
!
      IF(VENT.AND.VENSTA) THEN
        DO IP=1,NPOIN2
          TWNEW(IP)=TWOLD(IP)
        ENDDO
!
        IF (SVENT.GE.2.OR.(LVENT.EQ.1.AND.SVENT.NE.1).OR.
     &                                (SMOUT.EQ.2.AND.SVENT.NE.1)) THEN
          DO IP=1,NPOIN2
            USNEW(IP)=USOLD(IP)
            Z0NEW(IP)=Z0OLD(IP)
          ENDDO
        ENDIF
      ENDIF
!
!     -----------------------------------------------------------------
!     START OF THE MAIN LOOP ON THE NUMBER OF TIME STEPS (NSITS)
!     FOR INTEGRATION OF THE SOURCE TERMS, BY PROPAGATION TIME STEP
!     -----------------------------------------------------------------
!
      DO ISITS=1,NSITS
!
!       1. ASSIGNS THE START AND END DATES OF TIME STEP
!       ===============================================
!
        TDEB=AT-(NSITS-ISITS+1)*DTSI
        TFIN=TDEB+DTSI
!
!
!       2. UPDATES (IF HAS TO) THE WIND ARRAYS
!       ======================================
!
        IF(VENT.AND..NOT.VENSTA) THEN
!
!         2.1 UPDATES THE WIND FIELD FOR DATE TFIN
!         ---------------------------------------------------
!
          IF(NAMVEB(1:1).NE.' '.OR.NAMVEF(1:1).NE.' ') THEN
            IF(NAMVEF(1:1).NE.' ') THEN
              NVENT=LUVEF
              FMTVEN=FMTVEF
            ELSE
              NVENT=LUVEB
              FMTVEN=FMTVEB
            ENDIF
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE NOUDON'
            CALL NOUDON(UV,NAMEWX,2, VV,NAMEWY,2,
     &                  VV,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',0,
     &                  NPOIN2, NVENT,FMTVEN,TFIN,TV1,TV2,
     &                  UV1,UV2,VV1,VV2,VV1,VV2,INDIV,
     &                  'WIND   ',NVWIN,TEXVEB,TROUVE,UNITVEB,PHASVEB)
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE NOUDON'
          ELSEIF (PART.NE.WAC_CPL_RUN) THEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE ANAVEN'
            CALL ANAVEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE ANAVEN'
          ENDIF
!
!         2.2 COMPUTES THE WIND DIRECTION
!         -----------------------------------
!
          DO IP=1,NPOIN2
            VITVEN=SQRT(UV(IP)**2+VV(IP)**2)
            IF(VITVEN.GT.VITMIN) THEN
              TWNEW(IP)=ATAN2(UV(IP),VV(IP))
            ELSE
              TWNEW(IP)=0.D0
            ENDIF
          ENDDO
!
!         2.3 COMPUTES THE FRICTION VELOCITIES AND ROUGHNESS LENGTHS
!         ------------------------------------------------------------
!
          IF(SVENT.GE.2.OR.(LVENT.EQ.1.AND.SVENT.NE.1).OR.
     &        (SMOUT.EQ.2.AND.SVENT.NE.1)) THEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE USTAR2'
            CALL USTAR2( USNEW, NPOIN2)
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE USTAR2'
          ENDIF
!
        ENDIF
!
        IF(VENT.AND.SVENT.EQ.1) THEN
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE USTAR1'
          CALL USTAR1(USNEW, Z0NEW, TAUWAV, NPOIN2)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE USTAR1'
        ENDIF
!
!
!       3. COMPUTES MEAN PARAMETERS FOR THE DIRECTIONAL SPECTRUM
!       =========================================================
!
!       3.1 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
!       -----------------------------------------------
!
        IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE TOTNRJ'
        CALL TOTNRJ(VARIAN, F, NF, NDIRE, NPOIN2)
        IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE TOTNRJ'
!
        IF (CBAJ.EQ.0) THEN
!
!       3.2 COMPUTES THE MEAN FREQUENCY OF THE SPECTRUM
!       -----------------------------------------------
!
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE FREMOY'
          CALL FREMOY(FMOY, F, NF, NDIRE, NPOIN2)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE FREMOY'
!
!       3.3 COMPUTES THE MEAN WAVE NUMBER OF THE SPECTRUM
!       -------------------------------------------------
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE KMOYEN'
          CALL KMOYEN (XKMOY, XK , F, NF, NDIRE, NPOIN2,
     &         TAUX1 , TAUX2 , TAUX3 )
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE KMOYEN'
        ELSEIF (CBAJ.EQ.1) THEN
!
!       3.2 COMPUTES THE MEAN FREQUENCY OF THE SPECTRUM
!       -----------------------------------------------
!
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE FREM01'
          CALL FREM01 (FMOY, F, NF, NDIRE, NPOIN2)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE FREM01'
!
!       3.3 COMPUTES THE MEAN WAVE NUMBER OF THE SPECTRUM
!       -------------------------------------------------
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE KMOYE2'
          CALL KMOYE2(XKMOY, XK, F, NF, NDIRE , NPOIN2,
     &         TAUX1 , TAUX2 , TAUX3 )
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE KMOYE2'
        ELSE
          WRITE(LU,*) 'UNKNOWN VALUE OF BAJ:',CBAJ
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       4. COMPUTES THE CONTRIBUTIONS OF THE SOURCE TERMS FOR GENERATION,
!          WHITECAPPING AND INTERACTIONS BETWEEN QUADRUPLETS
!       =============================================================
!
!       4.1 INITIALISES THE ARRAYS FOR THE SOURCE TERMS
!       ----------------------------------------------------
        DO IFF=1,NF
          DO JP=1,NDIRE
            DO IP=1,NPOIN2
              TSTOT(IP,JP,IFF)=0.D0
              TSDER(IP,JP,IFF)=0.D0
            ENDDO
          ENDDO
        ENDDO
!
!       4.2 GENERATION BY WIND
!       ----------------------
!
        IF(VENT) THEN
          IF(SVENT.EQ.1) THEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QWIND1'
            CALL QWIND1
     &( TSTOT , TSDER , F     , XK    , USOLD , USNEW , TWOLD , TWNEW ,
     &  Z0OLD , Z0NEW , NF    , NDIRE , NPOIN2, TOLD  , TNEW  ,
     &  TAUX2 , TAUX3 , TAUX4 , TAUX5 , TAUX6 , TAUX7 )
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QWIND1'
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE STRESS'
            CALL STRESS
     &( TAUWAV, TSTOT , F     , USNEW , TWNEW , Z0NEW ,
     &  NPOIN2, NDIRE , NF    , TAUX1 , TAUX2 , TAUX3 )
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE STRESS'
          ELSEIF(SVENT.EQ.2) THEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QWIND2'
            CALL QWIND2
     &( TSTOT , TSDER , F     , XK    , USOLD , USNEW , TWOLD , TWNEW ,
     &  NF    , NDIRE , NPOIN2, T3_01%R,T3_02%R )
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QWIND2'
          ELSEIF(SVENT.EQ.3) THEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QWIND3'
            CALL QWIND3
     &( TSTOT , TSDER , F     , XK    , USOLD , USNEW , TWOLD , TWNEW ,
     &  NF    , NDIRE , NPOIN2, TAUX1 , TAUX2 , TAUX3 , TAUX4 )
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QWIND3'
!
          ENDIF
!
!       ADDS THE LINEAR WIND GROWTH SOURCE TERME
!       """""""""""""""""""""""""""""""""""""""
!
          IF(LVENT.EQ.1) THEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QWINDL'
            CALL QWINDL(TSTOT, USOLD, USNEW, TWOLD, TWNEW, NF, NDIRE,
     &                  NPOIN2, T3_01%R, T3_02%R, TAUX5, TAUX6)
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QWINDL'
          ENDIF
!
        ELSE
!
          DO IP=1,NPOIN2
            USNEW(IP)=0.D0
          ENDDO
!
        ENDIF

        IF (CBAJ.EQ.1) THEN
! Calculation pf mean frequency fmean_WS put in the  tabular TAUX7
          SEUILF = 1.D-20
          DO IP=1,NPOIN2
            TAUX1(IP) = 0.0D0
            TAUX2(IP) = 0.0D0
          ENDDO
!
          DO IFF=1,NF
            AUX3=DEUPI/DBLE(NDIRE)*DFREQ(IFF)
            AUX4=AUX3/FREQ(IFF)
            DO JP=1,NDIRE
              DO IP=1,NPOIN2
                CPHAS=DEUPI*FREQ(IFF)/XK(IP,IFF)
                AUXI=28.0D0/CPHAS*USNEW(IP)*COS(TETA(JP)-TWNEW(IP))
                IF ((TSTOT(IP,JP,IFF).GT.0).OR.(AUXI.GE.1.0D0)) THEN
                  TAUX1(IP) = TAUX1(IP) + F(IP,JP,IFF)*AUX3
                  TAUX2(IP) = TAUX2(IP) + F(IP,JP,IFF)*AUX4
                ENDIF
              ENDDO
            ENDDO
          ENDDO
!
          DO IP=1,NPOIN2
            IF (TAUX1(IP).LT.SEUILF) THEN
              TAUX7(IP) = SEUILF
            ELSE
              TAUX7(IP) = TAUX1(IP)/TAUX2(IP)
            ENDIF
          ENDDO
        ENDIF

!       4.3 NON-LINEAR INTERACTIONS BETWEEN QUADRUPLETS
!       -----------------------------------------------
!
        IF(STRIF.EQ.1) THEN
!
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QNLIN1'
          CALL QNLIN1(TSTOT, TSDER, IANGNL, NF, NDIRE, NPOIN2,
     &                F, XKMOY, TAUX1, TAUX2, TAUX3, TAUX4, TAUX5,TAUX6)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QNLIN1'
!
        ELSEIF (STRIF.EQ.2) THEN
!
!         sets XCCMDI values for MDIA method
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QNLIN2'
          XCCMDI(1)=8.360D7
          XCCMDI(2)=7.280D7
          XCCMDI(3)=3.340D7
          XCCMDI(4)=2.570D6
          DO K=1,MDIA
            XCCMDI(K)=XCCMDI(K)/DBLE(MDIA)
          ENDDO
!         alls MDIA method
          DO K=1,MDIA
            CALL QNLIN2
     &( TSTOT, TSDER, IANMDI((K-1)*NDIRE*16+1:K*NDIRE*16),
     &  COEMDI((K-1)*32+1:K*32), NF, NDIRE,
     &  NPOIN2, F, XKMOY, TAUX1, TAUX2, XCCMDI(K))
          ENDDO
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QNLIN2'
!....calls GQM method
        ELSEIF (STRIF.EQ.3) THEN
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QNLIN3'
          CALL QNLIN3(TSTOT, TSDER, F, NPOIN2, NDIRE, NF, TAUX1)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QNLIN3'
        ENDIF
!
!       4.4 WHITE-CAPPING DISSIPATION
!       -------------------------------------------------
!
        IF(SMOUT.EQ.1) THEN
!
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QMOUT1'
          CALL QMOUT1
     &( TSTOT, TSDER, F     , XK    , VARIAN, FMOY, XKMOY,
     &  NF    , NDIRE , NPOIN2, TAUX1 )
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QMOUT1'
!
        ELSEIF(SMOUT.EQ.2) THEN
!
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QMOUT2'
          CALL QMOUT2
     &( TSTOT , TSDER , F     , XK    , VARIAN, FMOY  , XKMOY , USOLD ,
     &  USNEW , NF    , NDIRE , NPOIN2, TAUX1 , TAUX2 , TAUX5 , TAUX6 )
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QMOUT2'
!
        ENDIF
!
!       4.5 BOTTOM FRICTION DISSIPATION
!       -------------------------------
!
        IF(SFROT.EQ.1.AND..NOT.PROINF) THEN
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QFROT1'
          CALL QFROT1
     &( TSTOT , TSDER , F     , XK    , NF    ,NDIRE , NPOIN2)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QFROT1'
        ELSEIF(SFROT.NE.0) THEN
          WRITE(LU,*) 'OPTION FOR BOTTOM FRICTION DISSIPATION'
          WRITE(LU,*) 'UNKNOWN: SFROT=',SFROT
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       5. UPDATES THE SPECTRUM - TAKES THE SOURCE TERMS INTO ACCOUNT
!         (GENERATION, WHITECAPPING AND QUADRUPLET INTERACTIONS)
!       ==============================================================
!
!
!       COMPUTATION OF LIMITING FACTOR INSERTED
!       IN THE LOOP TO HAVE DF_LIM(NPOIN2) INSTEAD
!       OF DF_LIM(NPOIN2,NF)
!
!     if nf is growing, inverse if limit and loop on nf
        DO IFF=1,NF
!         LIMITING FACTOR TAKEN FROM WAM-CYCLE 4
          IF(LIMIT.EQ.1) THEN
            COEF=0.62D-4*DTSI/1200.D0
            AUXI=COEF/FREQ(IFF)**5
            DO IP=1,NPOIN2
              DF_LIM(IP)=AUXI
            ENDDO
!         LIMITING FACTOR FROM HERSBACH AND JANSSEN (1999)
          ELSEIF(LIMIT.EQ.2) THEN
            COEF=3.D-7*GRAVIT*FREQ(NF)*DTSI
            AUXI=COEF/FREQ(IFF)**4
            USMIN=GRAVIT*5.6D-3/FREQ(IFF)
            DO IP=1,NPOIN2
              DF_LIM(IP)=AUXI*MAX(USNEW(IP),USMIN)
            ENDDO
!
!           LIMITING FACTOR FROM LAUGEL ou BAJ
!
          ELSEIF (LIMIT.EQ.3) THEN
            COEF=3.0D-7*GRAVIT*DTSI
            AUXI=COEF/FREQ(IFF)**4
            USMIN=GRAVIT*5.6D-3/FREQ(IFF)
            DO IP=1,NPOIN2
              DF_LIM(IP)=AUXI*MAX(USNEW(IP),USMIN)*TAUX7(IP)
            ENDDO
!
          ELSEIF(LIMIT.NE.0) THEN
            WRITE(LU,*) 'UNKNOWN LIMITING FACTOR:',LIMIT
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(LIMIT.NE.0) THEN
            DO JP=1,NDIRE
              DO IP=1,NPOIN2
                AUX1=MAX(1.D0-DTSI*TSDER(IP,JP,IFF)*CIMPLI,1.D0)
                AUX2=DTSI*TSTOT(IP,JP,IFF)/AUX1
                AUX3=MIN(ABS(AUX2),DF_LIM(IP))
                AUX4=SIGN(AUX3,AUX2)
                F(IP,JP,IFF)=MAX(F(IP,JP,IFF)+AUX4,0.D0)
              ENDDO
            ENDDO
          ELSE
            DO JP=1,NDIRE
              DO IP=1,NPOIN2
                AUX1=MAX(1.D0-DTSI*TSDER(IP,JP,IFF)*CIMPLI,1.D0)
                AUX2=DTSI*TSTOT(IP,JP,IFF)/AUX1
                F(IP,JP,IFF)=MAX(F(IP,JP,IFF)+AUX2,0.D0)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
!
        IF(DIAGHF.EQ.1) THEN
!
!       6. TREATS THE HIGH FREQUENCIES DIFFERENTLY
!       =======================================================
!
!       6.1 COMPUTES THE MEAN FREQUENCY OF THE SPECTRUM
!       ----------------------------------------------
!
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE FREMOY'
          CALL FREMOY(FMOY, F, NF, NDIRE, NPOIN2)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE FREMOY'
!
          AUX1=GRAVIT/(7.D0*DEUPI*FREQ(1))
          AUX2=2.5D0/FREQ(1)
          AUX3=1.D0/LOG10(RAISF)
!
!     IF CBAJ and loop inversed 6.2 and 6.3 written twice with a different formula for FM2
          IF(CBAJ.EQ.0) THEN
            DO IP=1,NPOIN2
!
!       6.2 COMPUTES THE LAST FREQUENCY OF THE DISCRETISED SPECTRUM.
!           THIS FREQUENCY IS THE MAXIMUM OF (FM1=4.*FPM ; FM2=2.5*FMOY).
!           ITS INDEX IS MFMAX.
!       -------------------------------------------------------------
!
              FM1 =AUX1/MAX(USNEW(IP),1.D-90)
              FM2 =AUX2*FMOY(IP)
              MF1=INT(AUX3*LOG10(FM1)+1.D0)
              MF2=INT(AUX3*LOG10(FM2)+1.D0)
              MFMAX=MAX(MIN(MAX(MF1,MF2),NF),1)
!
!       6.3 MODIFIES THE HIGH FREQUENCY PART OF THE SPECTRUM
!           A DECREASE IN F**(-TAILF) IS IMPOSED BEYOND
!           FREQ(MFMAX).  (TAILF=5 IN WAM-CYCLE 4)
!       -------------------------------------------------------------
!
              DO IFF=MFMAX+1,NF
                AUX4=(FREQ(MFMAX)/FREQ(IFF))**TAILF
                DO JP=1,NDIRE
                  F(IP,JP,IFF)=AUX4*F(IP,JP,MFMAX)
                ENDDO
              ENDDO
            ENDDO

          ELSE
            DO IP=1,NPOIN2
              FM1 =AUX1/MAX(USNEW(IP),1.D-90)
              FM2 =AUX2*TAUX7(IP)
              MF1=INT(AUX3*LOG10(FM1)+1.D0)
              MF2=INT(AUX3*LOG10(FM2)+1.D0)
              MFMAX=MAX(MIN(MAX(MF1,MF2),NF),1)
              DO IFF=MFMAX+1,NF
                AUX4=(FREQ(MFMAX)/FREQ(IFF))**TAILF
                DO JP=1,NDIRE
                  F(IP,JP,IFF)=AUX4*F(IP,JP,MFMAX)
                ENDDO
              ENDDO
!
            ENDDO
          ENDIF
        ELSEIF(DIAGHF.GE.2) THEN
          WRITE(LU,*) 'OPTION FOR DIAGNOSTIC TAIL'
          WRITE(LU,*) 'UNKNOWN: DIAGHF=',DIAGHF
          CALL PLANTE(1)
          STOP
        ENDIF
!
!
!       7. TAKES THE BREAKING SOURCE TERM INTO ACCOUNT
!       =================================================
!
!        IF((SBREK.GT.0.OR.STRIA.GT.0).AND..NOT.PROINF) THEN
!VB mofid
        IF(((SBREK.GT.0.OR.STRIA.GT.0.OR.VEGETATION.OR.POROUS).AND.
     &     .NOT.PROINF).OR.SDSCU.EQ.2) THEN
!VB fin modif
!
!         7.1 COMPUTES A REPRESENTATIVE FREQUENCY
!         ------------------------------------------
          IF (SBREK.GT.0.AND.SBREK.LT.5) THEN
            IF (SBREK.EQ.1) IFCAR = IFRBJ
            IF (SBREK.EQ.2) IFCAR = IFRTG
            IF (SBREK.GE.3) IFCAR = IFRRO
            IF (SBREK.GE.4) IFCAR = IFRIH
!
            IF (IFCAR.EQ.1) THEN
!
!             MEAN FREQUENCY FMOY
!             - - - - - - - - - - - -
              IF (CBAJ.EQ.1) THEN
                IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE FREMOY'
                CALL FREMOY(TAUX3, F, NF, NDIRE, NPOIN2 )
                IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE FREMOY'
              ELSE
                DO IP=1,NPOIN2
                  TAUX3(IP)=FMOY(IP)
                ENDDO
              ENDIF

            ELSE IF (IFCAR.EQ.2) THEN
!
!             MEAN FREQUENCY F01
!             - - - - - - - - - - -
              IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE FREM01'
              CALL FREM01( TAUX3, F, NF, NDIRE, NPOIN2)
              IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE FREM01'

            ELSE IF (IFCAR.EQ.3) THEN
!
!             MEAN FREQUENCY F02
!             - - - - - - - - - - -
              IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE FREM02'
              CALL FREM02( TAUX3, F, NF, NDIRE, NPOIN2)
              IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE FREM02'

            ELSE IF (IFCAR.EQ.4) THEN
!
!             PEAK FREQUENCY (DISCRETE FREQUENCY WITH MAX VARIANCE)
!             - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE FREPIC'
              CALL FREPIC( TAUX3, F, NF, NDIRE, NPOIN2)
              IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE FREPIC'

            ELSE IF (IFCAR.EQ.5) THEN
!
!             PEAK FREQUENCY (READ WITH EXPONENT 5)
!             - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE FPREAD'
              CALL FPREAD( TAUX3, F, NF, NDIRE, NPOIN2, 5.D0)
              IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE FPREAD'

            ELSE IF (IFCAR.EQ.6) THEN
!
!             PEAK FREQUENCY (READ WITH EXPONENT 8)
!             - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE FPREAD'
              CALL FPREAD( TAUX3, F, NF, NDIRE, NPOIN2, 8.D0)
              IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE FPREAD'

            ELSE

              WRITE(LU,*) 'WAVE FREQUENCY NOT EXPECTED......IFCAR=',
     &                       IFCAR
              CALL PLANTE(1)
              STOP
            ENDIF
!
        ENDIF
!
!.........LOOP ON SUB-TIME STEPS FOR BREAKING
!         = = = = = = = = = = = = = = = = = = = = = = = = = = =
        SOMM=(XDTBRK**NDTBRK-1.D0)/(XDTBRK-1.D0)
        DTN=DTSI/SOMM
!
        DO IDT=1,NDTBRK
!         7.2 INITIALISES THE ARRAYS FOR THE SOURCE-TERMS
!         ----------------------------------------------------
          DO IFF=1,NF
            DO JP=1,NDIRE
              DO IP=1,NPOIN2
                TSTOT(IP,JP,IFF)=0.D0
              ENDDO
            ENDDO
          ENDDO
!
!         7.3 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
!         --------------------------------------------
!
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE TOTNRJ'
          CALL TOTNRJ(VARIAN, F, NF, NDIRE, NPOIN2)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE TOTNRJ'
!
!
!         7.4 COMPUTES THE WAVE BREAKING CONTRIBUTION
!         --------------------------------------
!
!         7.4.1 BREAKING ACCORDING TO BATTJES AND JANSSEN (1978)
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
          IF(SBREK.EQ.1) THEN
!
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QBREK1'
          CALL QBREK1
     & ( TSTOT , F     , TAUX3 , VARIAN, NF    , NDIRE , NPOIN2)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QBREK1'
!
!
!         7.4.2 BREAKING ACCORDING TO THORNTON AND GUZA (1983)
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
          ELSEIF(SBREK.EQ.2) THEN
!
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QBREK2'
          CALL QBREK2
     & ( TSTOT , F     , TAUX3 , VARIAN, NF    , NDIRE ,
     &   NPOIN2)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QBREK2'
!
!
!         7.4.3 BREAKING ACCORDING TO ROELVINK (1993)
!         - - - - - - - - - - - - - - - - - - - - - -
!
          ELSEIF(SBREK.EQ.3) THEN
!
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QBREK3'
          CALL QBREK3
     &( TSTOT , F     , TAUX3 , VARIAN, NF    , NDIRE ,
     &  NPOIN2)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QBREK3'
!
!
!         7.4.4 BREAKING ACCORDING TO IZUMIYA AND HORIKAWA (1984)
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
          ELSEIF(SBREK.EQ.4) THEN
!
          IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QBREK4'
          CALL QBREK4
     &( TSTOT , F     ,TAUX3,VARIAN, NF    , NDIRE , NPOIN2)
          IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QBREK4'
!
          ELSEIF(SBREK.NE.0) THEN
            WRITE(LU,*) 'BREAKING FORMULATION NOT PROGRAMMED: ',
     &                     SBREK
            CALL PLANTE(1)
            STOP
          ENDIF
!
!       7.5 NON-LINEAR INTERACTIONS BETWEEN FREQUENCY TRIPLETS
!       -----------------------------------------------------------
          IF(STRIA.EQ.1) THEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE FREMOY'
            CALL FREMOY( FMOY, F, NF, NDIRE, NPOIN2)
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE FREMOY'
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QTRIA1'
            CALL QTRIA1
     &( F     , XK    , NF    , NDIRE , NPOIN2, TSTOT , VARIAN, FMOY  )
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QTRIA1'
!
          ELSEIF(STRIA.EQ.2) THEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QTRIA2'
            CALL QTRIA2
     &( F     , XK    , NF    , NDIRE , NPOIN2, TSTOT )
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QTRIA2'
          ENDIF
!
!
!         7.6 WAVE BLOCKING DISSIPATION
!         -----------------------------
          IF(SDSCU.EQ.2) THEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QDSCUR'
            CALL QDSCUR
     &( TSTOT , TSDER , F     , CF    , XK    , USOLD , USNEW ,
     &  NF    , NDIRE , NPOIN2, TAUX2 ,T3_01%R,T3_02%R)
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QDSCUR'
          ENDIF
!
!======================================================================
!         7.7 VEGETATION
!VBA PRISE EN COMPTE VEGETATION
!======================================================================
!
          IF(VEGETATION) THEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QVEG'
            CALL QVEG( TSTOT, TSDER, F, VARIAN, FMOY, XKMOY, NF,
     &                   NDIRE  ,NPOIN2)
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QVEG'
          ENDIF
!======================================================================
!         4.6 POROUS MEDIA
!TF PRISE EN COMPTE POROSITE
!======================================================================
!
          IF(POROUS) THEN
            IF(DEBUG.EQ.2) WRITE(LU,*) '     APPEL DE QPOROS'
            CALL QPOROS( TSTOT , TSDER , F , CG, LT, XK,
     &                   NF    , NDIRE  , NPOIN2, AMORP)
            IF(DEBUG.EQ.2) WRITE(LU,*) '     RETOUR DE QPOROS'
          ENDIF
!
!======================================================================
!VBA PRISE EN COMPTE VEGETATION
!======================================================================
!
!
!         7.9 UPDATES THE SPECTRUM - TAKES THE BREAKING SOURCE TERM
!             INTO ACCOUNT (EXPLICIT EULER SCHEME)
!         ---------------------------------------------------------
!
        DO IFF=1,NF
          DO JP=1,NDIRE
            DO IP=1,NPOIN2
              F(IP,JP,IFF)=MAX(F(IP,JP,IFF)+DTN*TSTOT(IP,JP,IFF),0.D0)
            ENDDO
          ENDDO
        ENDDO
!
        DTN=DTN*XDTBRK
!
        ENDDO
!
        ENDIF
!
!
!       8. TRANSFERS DATA FROM NEW TO OLD FOR THE NEXT TIME STEP
!       ==============================================================
        IF(VENT) THEN
          DO IP=1,NPOIN2
            USOLD(IP)=USNEW(IP)
            Z0OLD(IP)=Z0NEW(IP)
            TWOLD(IP)=TWNEW(IP)
          ENDDO
        ENDIF
!
!======================================================================
!        9 IGNORE BOUNDARY CONDITIONS ON IMPOSED BOUNDARIES
!======================================================================
! ALONG THE IMPOSED BOUNDARIES, REWRITE THE CORRECT SPECTRUM
! IF THE SOURCE TERMS SHOULD NOT BE TAKEN INTO ACCOUNT
!
        IF(.NOT.SOURCE_ON_BND) THEN
          DO IPTFR=1,NPTFR
            IF(LIFBOR(IPTFR).EQ.KENT) THEN
              DO IFF=1,NF
                DO IP=1,NDIRE
                  F(NBOR(IPTFR),IP,IFF)=FBOR(IPTFR,IP,IFF)
                ENDDO
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDDO

!
!     -----------------------------------------------------------------
!     END OF THE MAIN LOOP ON THE NUMBER OF TIME STEPS (NSITS)
!     FOR INTEGRATION OF THE SOURCE TERMS, BY PROPAGATION TIME STEP
!     -----------------------------------------------------------------
!
      RETURN
      END

