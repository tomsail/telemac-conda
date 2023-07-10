!                     *****************
                      SUBROUTINE BORICE
!                     *****************
!
     &(H,U,V,F,AT,LT,DT,TRA05,TRA06,LIUBOR,NPTFR,NUMLIQ,KLOG,MASK,MESH
     & ,S)
!
!***********************************************************************
! KHIONE   V7P3
!***********************************************************************
!
!brief    INCLUDE ICE PROCESSES TO THE BOUNDARIES SETTINGS.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME OF THE DATASET
!| DT             |-->| TIME STEP
!| F              |-->| TRACER VALUES
!| H              |-->| WATER DEPTH
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITION FOR THE VELOCITY
!| LT             |-->| NUMBER OF TIME STEPS
!| MASK           |-->| BLOCK OF MASKS FOR DIFFERENT BOUNDARY CONDITIONS
!| MESH           |-->| MESH STRUCTURE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINT
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER
!| S              |-->| VOID STRUCTURE
!| TRA05          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| TRA06          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| U              |-->| X-COMPONENT OF THE VELOCITY
!| V              |-->| Y-COMPONENT OF THE VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE, ONLY:
     &         TITICECAS,ICE_FILES,CLGRFO,LEOPRD,
     &         LINES,CLOGGING,BD_ICE,IND_FRA,T1,T2,
     &         NFRCLOG,NUMCLOG,CLOG_EF,CLOG_THETA,
     &         CLOG_VDIST,CLOG_VDIAM,CLOG_VLGTH,
     &         CLOG_TDIST,CLOG_TDIAM,CLOG_TLGTH,
     &         ICETYPE,CLOG_TWDTH,CLOG_VWDTH,
     &         RHO_ICE,CLOG_EF,CLOG_VOLUM,ICELOC,T3,NSECLOG,NTOTCLOG
      USE FREEZEUP_KHIONE, ONLY: CLOGGED_ON_BAR
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
      USE INTERFACE_KHIONE, EX_BORICE => BORICE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: KLOG, NPTFR
!
      INTEGER,          INTENT(IN)    :: NUMLIQ(NPTFR)
      INTEGER,          INTENT(IN)    :: LIUBOR(NPTFR)
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: H,U,V,F,S
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: MASK, TRA05,TRA06
      INTEGER,          INTENT(IN)    :: LT
      DOUBLE PRECISION, INTENT(IN)    :: AT,DT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER               K,N,I,IFRLIQ,IELEB,IFILE
      LOGICAL               OUTPUT
      DOUBLE PRECISION PI, ANG,RV0,RV1,RT0,RT1, RATE, QCV,CV,CA,
     &      MFBAR,MFVB,MFTB, MFGRD,MFVT,MFTT,MASS0, DW,DV,TAW, NV,NT
      DOUBLE PRECISION, ALLOCATABLE :: QF(:),QA(:),QV(:)
      CHARACTER(LEN=250)    CELL
!
!-----------------------------------------------------------------------
!
      INTRINSIC ADJUSTL,TRIM
!
!=======================================================================
!
!     5 - CLOGGING
!
!     NO IMPACT ON THE HYDRODYNAMICS FOR NOW
!     WARNING: ONLY WORKS FOR MONO-CLASS FRAZIL ICE
!
      IF( CLOGGING ) THEN
!
        IF(NSECLOG.GT.0) THEN
          ALLOCATE(QF(NSECLOG))
          ALLOCATE(QA(NSECLOG))
          ALLOCATE(QV(NSECLOG))
          CALL SECTION_KHIONE(MESH,MESH%IKLE%I,MESH%NELMAX,
     &                        MESH%IFABOR%I,F,U,V,QF,QA,QV,H,S,LT)
        ENDIF
!
        IF( LT.LE.1 .AND. NTOTCLOG.GT.0 ) THEN
!
          IF(NFRCLOG.GT.0) THEN
!
!-----------------------------------------------------------------------
!         COMPUTES THE LENGTH OF THE OPEN BOUNDARY (CLOG_TLGTH) ONCE
            CALL OS('X=C     ', X=T3, C=1.D0)
            DO IFRLIQ = 1,NFRCLOG
              N = NUMCLOG(IFRLIQ)
              CALL OS( 'X=0     ', X=TRA05 )
              DO IELEB = 1,MESH%NELEB
                K = MESH%IKLBOR%I(IELEB)
                IF( NUMLIQ(K).EQ.N )
     &            TRA05%R(IELEB) = MASK%ADR(8)%P%R(IELEB)
              ENDDO
              CALL VECTOR(TRA06,'=','MASVEC          ',
     &          IELBOR(11,1),1.D0,T3,T3,T3,T3,T3,T3,MESH,.TRUE.,TRA05)
              CLOG_TLGTH(IFRLIQ) = BIEF_SUM(TRA06)
              IF( NCSIZE.GT.1 )
     &          CLOG_TLGTH(IFRLIQ) = P_DSUM(CLOG_TLGTH(IFRLIQ))
            ENDDO
          ENDIF
!
!-----------------------------------------------------------------------
!         HEADER OF ASCII CLOGGING FILE
!
          IF( ICE_FILES(CLGRFO)%NAME.NE.' ' ) THEN
            IF(NCSIZE.EQ.1.OR.IPID.EQ.0) THEN
              IFILE = ICE_FILES(CLGRFO)%LU
              WRITE(IFILE,301) ''
              WRITE(IFILE,301) TITICECAS
              WRITE(IFILE,301) ''
 301          FORMAT('# ',A)
              LINES%HTXT = 'T'
              WRITE(IFILE,300,ADVANCE='NO') TRIM(ADJUSTL(LINES%HTXT))
              LINES%HUNT = 'S'
              DO IFRLIQ = 1,NTOTCLOG
                IF( LINES%CELLS(IFRLIQ)%NVAL.EQ.0 ) THEN
                  LINES%CELLS(IFRLIQ)%NVAL = 8
                  ALLOCATE(LINES%CELLS(IFRLIQ)%TXT(8))
                  ALLOCATE(LINES%CELLS(IFRLIQ)%UNT(8))
                  ALLOCATE(LINES%CELLS(IFRLIQ)%VAL(8))
                ENDIF
                WRITE(CELL,'(I12)') IFRLIQ
                LINES%CELLS(IFRLIQ)%TXT(1) = 'FRAZIL('
     &            // TRIM(ADJUSTL(CELL)) // ')'
                LINES%CELLS(IFRLIQ)%UNT(1) = 'SI'
                LINES%CELLS(IFRLIQ)%TXT(2) = 'WIDTH_V('
     &            // TRIM(ADJUSTL(CELL)) // ')'
                LINES%CELLS(IFRLIQ)%UNT(2) = 'M'
                LINES%CELLS(IFRLIQ)%TXT(3) = 'WIDTH_H('
     &            // TRIM(ADJUSTL(CELL)) // ')'
                LINES%CELLS(IFRLIQ)%UNT(3) = 'M'
                LINES%CELLS(IFRLIQ)%TXT(4) = 'CLG_AREA('
     &            // TRIM(ADJUSTL(CELL)) // ')'
                LINES%CELLS(IFRLIQ)%UNT(4) = 'M^2'
                LINES%CELLS(IFRLIQ)%TXT(5) = 'IMASS_V('
     &            // TRIM(ADJUSTL(CELL)) // ')'
                LINES%CELLS(IFRLIQ)%UNT(5) = 'G'
                LINES%CELLS(IFRLIQ)%TXT(6) = 'IMASS_H('
     &            // TRIM(ADJUSTL(CELL)) // ')'
                LINES%CELLS(IFRLIQ)%UNT(6) = 'G'
                LINES%CELLS(IFRLIQ)%TXT(7) = 'CLG_VOL('
     &            // TRIM(ADJUSTL(CELL)) // ')'
                LINES%CELLS(IFRLIQ)%UNT(7) = 'M^3'
                LINES%CELLS(IFRLIQ)%TXT(8) = 'CLG_MAS('
     &            // TRIM(ADJUSTL(CELL)) // ')'
                LINES%CELLS(IFRLIQ)%UNT(8) = 'SI'
                WRITE(IFILE,302,ADVANCE='NO') ( ' ' //
     &           TRIM(ADJUSTL(LINES%CELLS(IFRLIQ)%TXT(K))), K=1,8)
              ENDDO
              WRITE(IFILE,300) ''
              WRITE(IFILE,300,ADVANCE='NO') TRIM(ADJUSTL(LINES%HUNT))
              DO I = 1,NTOTCLOG
                WRITE(IFILE,302,ADVANCE='NO')
     &            ( ' ' // TRIM(ADJUSTL(LINES%CELLS(I)%UNT(K)))
     &            , K=1,8 )
              ENDDO
              WRITE(IFILE,300) ''
 300          FORMAT(A)
 302          FORMAT(8A)
            ENDIF
          ENDIF
        ENDIF
!
!-----------------------------------------------------------------------
!       PREPARE COMPUTATION OF THE FLUXES AND THE AVERAGE BAR HEIGHTS
!
!
        CALL OS('X=Y     ', X=T2, Y=H )
        CALL OS('X=YZ    ', X=T1, Y=H, Z=F%ADR(IND_FRA)%P )

!
        PI  = 4.D0*ATAN(1.D0)
        OUTPUT = .FALSE.
!
!       ACUMULATION ANGLE
        ANG = PI * CLOG_THETA / 180.D0
!       INITIAL FRAZIL RADIUS
        RV0 = CLOG_VDIAM*COS(ANG)
        RT0 = CLOG_TDIAM*COS(ANG)
!
!       CLOGGING RATES ( AF = 1.D0 )
        RATE = 2.D0*SIN(ANG)*SIN(ANG)*(1.D0)/( 1.D0-CLOG_EF )/ANG
!
!-----------------------------------------------------------------------
!
        IF(NFRCLOG.GT.0) THEN
          DO IFRLIQ = 1,NFRCLOG
            N = NUMCLOG(IFRLIQ)
!
!           MASKING ONE LIQUID BOUNDARY AT A TIME
            CALL OS( 'X=0     ', X=TRA05 )
            DO IELEB = 1,MESH%NELEB
              K = MESH%IKLBOR%I(IELEB)
              IF( NUMLIQ(K).EQ.N )
     &          TRA05%R(IELEB) = MASK%ADR(8)%P%R(IELEB)
            ENDDO
!
!           FRAZIL FLUX = INTEGRAL( F * H * UV ) ACROSS BOUNDARY N
            CALL VECTOR(TRA06,'=','FLUBDF          ',
     &        IELBOR(11,1),1.D0,T1,T1,T1,U,V,V,MESH,.TRUE.,TRA05)
            QCV = BIEF_SUM(TRA06)
            IF( NCSIZE.GT.1 ) QCV = P_DSUM(QCV)
!           FRAZIL FLUX BY UNIT OF WIDTH
            QCV = ABS(QCV) / CLOG_TLGTH(IFRLIQ)
!
!           CROSS SECTIONAL AREA = INTEGRAL( H ) ACROSS BOUNDARY N
            CALL VECTOR(TRA06,'=','MASVEC          ',
     &        IELBOR(11,1),1.D0,T2,T2,T2,T2,T2,T2,MESH,.TRUE.,TRA05)
            CA = BIEF_SUM(TRA06)
            IF( NCSIZE.GT.1 ) CA = P_DSUM(CA)
!           AVERAGE BAR HEIGHT
            IF( CLOG_TLGTH(IFRLIQ).GT.0.D0 ) THEN
              CLOG_VLGTH(IFRLIQ) = CA/CLOG_TLGTH(IFRLIQ)
            ELSE
              CLOG_VLGTH(IFRLIQ) = 0.D0
            ENDIF
!
!           AVERAGE CONCENTRATION = INTEGRAL( F * H ) ACROSS BOUNDARY N
            CALL VECTOR(TRA06,'=','MASVEC          ',
     &        IELBOR(11,1),1.D0,T1,T1,T1,T1,T1,T1,MESH,.TRUE.,TRA05)
            CV = BIEF_SUM(TRA06)
            IF( NCSIZE.GT.1 ) CV = P_DSUM(CV)
!           ... DIVIDED BY THE AREA OF THE CROSS SECTION
            IF( CA.GT.0.D0 ) THEN
              CV = CV/CA
            ELSE
              CV = 0.D0
            ENDIF
!
!           REPRESENTATIVE NUMBERS OF BARS
            IF( CLOG_VDIST*CLOG_VDIAM.GT.0.D0 ) THEN
!             THIS COULD BE AN ODD NUMBER
              NV = CLOG_TLGTH(IFRLIQ)/CLOG_VDIST
            ELSE
              NV = 0.D0
            ENDIF
            IF( CLOG_TDIST*CLOG_TDIAM.GT.0.D0 ) THEN
!             COUNTIN ONLY THE FULLY SUBMERGED
              NT = 1.D0*INT( CLOG_VLGTH(IFRLIQ)/CLOG_TDIST )
            ELSE
              NT = 0.D0
            ENDIF
!
!           VERTICAL BARS
            IF( CLOG_VLGTH(IFRLIQ)*NV.GT.0.D0 ) THEN
              CLOG_VWDTH(IFRLIQ) = CLOG_VWDTH(IFRLIQ) +
     &                             DT*RATE*QCV/CLOG_VLGTH(IFRLIQ)
              RV1 = CLOG_VWDTH(IFRLIQ)/2.D0/SIN(ANG)
              CALL CLOGGED_ON_BAR( RV0,RV1,
     &          CLOG_VDIAM,CLOG_VLGTH(IFRLIQ),NV,ANG,MFVB,MFVT)
            ELSE
              RV1 = 0.D0
              MFVB = 0.D0
              MFVT = 0.D0
            ENDIF
!
!           TRANSVERSE BARS
            IF( CLOG_TLGTH(IFRLIQ)*NT.GT.0.D0 ) THEN
              CLOG_TWDTH(IFRLIQ) = CLOG_TWDTH(IFRLIQ) +
     &                             DT*RATE*QCV/CLOG_VLGTH(IFRLIQ)
              RT1 = CLOG_TWDTH(IFRLIQ)/2.D0/SIN(ANG)
              CALL CLOGGED_ON_BAR( RT0,RT1,
     &          CLOG_TDIAM,CLOG_TLGTH(IFRLIQ),NT,ANG,MFTB,MFTT)
            ELSE
              RT1 = 0.D0
              MFTB = 0.D0
              MFTT = 0.D0
            ENDIF
!
            IF( ICE_FILES(CLGRFO)%NAME.EQ.' ' ) CYCLE
            IF( LT.NE.LEOPRD*INT(LT/LEOPRD) .AND. LT.GT.1 ) CYCLE
!
!           ADDITIONAL PRINTOUTS
            OUTPUT = .TRUE.
!
            IF(NCSIZE.EQ.1.OR.IPID.EQ.0) THEN
              WRITE(LINES%CELLS(IFRLIQ)%VAL(1),303) CV
              WRITE(LINES%CELLS(IFRLIQ)%VAL(2),303) CLOG_VWDTH(IFRLIQ)
              WRITE(LINES%CELLS(IFRLIQ)%VAL(3),303) CLOG_TWDTH(IFRLIQ)
!             WIDTH OF THE VERTICAL BARS EXCLUDED FROM THE TRANSVERSE BARS
              DW = CLOG_VDIST - CLOG_VWDTH(IFRLIQ)
              DV = MAX( CLOG_VWDTH(IFRLIQ),CLOG_VDIAM )
!             AVAILABLE AREA FOR THE RACK
              TAW = CLOG_VLGTH(IFRLIQ)*DW*(NV-1.D0)
     &          - CLOG_TWDTH(IFRLIQ) * ( CLOG_TLGTH(IFRLIQ)-NV*DV ) * NT
              TAW = MAX(TAW,0.D0)
              WRITE(LINES%CELLS(IFRLIQ)%VAL(4),303) TAW
!             FRAZIL VOLUME FOR THE GRID
              CLOG_VOLUM(IFRLIQ) = CLOG_VOLUM(IFRLIQ) +
     &          QCV * ( 1.D0-CLOG_EF ) * DT *
     &          ( CLOG_VWDTH(IFRLIQ)*NV + CLOG_TWDTH(IFRLIQ)*NT )
!             FRAZIL MASS FOR THE GRID
              MASS0 = CLOG_VOLUM(IFRLIQ)*RHO_ICE
!
              MFBAR = MFVB + MFTB
              WRITE(LINES%CELLS(IFRLIQ)%VAL(5),303) MFBAR
              MFGRD = MFVT + MFTT
              WRITE(LINES%CELLS(IFRLIQ)%VAL(6),303) MFGRD
              WRITE(LINES%CELLS(IFRLIQ)%VAL(7),303) CLOG_VOLUM(IFRLIQ)
              WRITE(LINES%CELLS(IFRLIQ)%VAL(8),303) MASS0
            ENDIF
!
          ENDDO
        ENDIF
!
        IF(NSECLOG.GT.0) THEN
          DO I=NFRCLOG+1,NTOTCLOG
            CLOG_VLGTH(I) = QA(I-NFRCLOG)/CLOG_TLGTH(I)
            QCV = QF(I-NFRCLOG)
            IF(QA(I-NFRCLOG).GT.0.D0) THEN
              CV = QV(I-NFRCLOG) / QA(I-NFRCLOG)
            ELSE
              CV=0.D0
            ENDIF
!           REPRESENTATIVE NUMBERS OF BARS
            IF( CLOG_VDIST*CLOG_VDIAM.GT.0.D0 ) THEN
!             THIS COULD BE AN ODD NUMBER
              NV = CLOG_TLGTH(I)/CLOG_VDIST
            ELSE
              NV = 0.D0
            ENDIF
            IF( CLOG_TDIST*CLOG_TDIAM.GT.0.D0 ) THEN
!             COUNTIN ONLY THE FULLY SUBMERGED
              NT = 1.D0*INT( CLOG_VLGTH(I)/CLOG_TDIST )
            ELSE
              NT = 0.D0
            ENDIF
!
!           VERTICAL BARS
            IF( CLOG_VLGTH(I)*NV.GT.0.D0 ) THEN
              CLOG_VWDTH(I) = CLOG_VWDTH(I) +
     &                             DT*RATE*QCV/CLOG_VLGTH(I)
              RV1 = CLOG_VWDTH(I)/2.D0/SIN(ANG)
              CALL CLOGGED_ON_BAR( RV0,RV1,
     &          CLOG_VDIAM,CLOG_VLGTH(I),NV,ANG,MFVB,MFVT)
            ELSE
              RV1 = 0.D0
              MFVB = 0.D0
              MFVT = 0.D0
            ENDIF
!
!           TRANSVERSE BARS
            IF( CLOG_TLGTH(I)*NT.GT.0.D0 ) THEN
              CLOG_TWDTH(I) = CLOG_TWDTH(I) +
     &                             DT*RATE*QCV/CLOG_VLGTH(I)
              RT1 = CLOG_TWDTH(I)/2.D0/SIN(ANG)
              CALL CLOGGED_ON_BAR( RT0,RT1,
     &          CLOG_TDIAM,CLOG_TLGTH(I),NT,ANG,MFTB,MFTT)
            ELSE
              RT1 = 0.D0
              MFTB = 0.D0
              MFTT = 0.D0
            ENDIF
!
            IF( ICE_FILES(CLGRFO)%NAME.EQ.' ' ) CYCLE
            IF( LT.NE.LEOPRD*INT(LT/LEOPRD) .AND. LT.GT.1 ) CYCLE
!
!           ADDITIONAL PRINTOUTS
            OUTPUT = .TRUE.
!
            IF(NCSIZE.EQ.1.OR.IPID.EQ.0) THEN
              WRITE(LINES%CELLS(I)%VAL(1),303) CV
              WRITE(LINES%CELLS(I)%VAL(2),303) CLOG_VWDTH(I)
              WRITE(LINES%CELLS(I)%VAL(3),303) CLOG_TWDTH(I)
!             WIDTH OF THE VERTICAL BARS EXCLUDED FROM THE TRANSVERSE BARS
              DW = CLOG_VDIST - CLOG_VWDTH(I)
              DV = MAX( CLOG_VWDTH(I),CLOG_VDIAM )
!             AVAILABLE AREA FOR THE RACK
              TAW = CLOG_VLGTH(I)*DW*(NV-1.D0)
     &          - CLOG_TWDTH(I) * ( CLOG_TLGTH(I)-NV*DV ) * NT
              TAW = MAX(TAW,0.D0)
              WRITE(LINES%CELLS(I)%VAL(4),303) TAW
!             FRAZIL VOLUME FOR THE GRID
              CLOG_VOLUM(I) = CLOG_VOLUM(I) +
     &          QCV * ( 1.D0-CLOG_EF ) * DT *
     &          ( CLOG_VWDTH(I)*NV + CLOG_TWDTH(I)*NT )
!             FRAZIL MASS FOR THE GRID
              MASS0 = CLOG_VOLUM(I)*RHO_ICE
!
              MFBAR = MFVB + MFTB
              WRITE(LINES%CELLS(I)%VAL(5),303) MFBAR
              MFGRD = MFVT + MFTT
              WRITE(LINES%CELLS(I)%VAL(6),303) MFGRD
              WRITE(LINES%CELLS(I)%VAL(7),303) CLOG_VOLUM(I)
              WRITE(LINES%CELLS(I)%VAL(8),303) MASS0
            ENDIF

          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
!
 303    FORMAT(G16.9E3)
        IF( ICE_FILES(CLGRFO)%NAME.NE.' ' .AND. OUTPUT ) THEN
          IF(NCSIZE.EQ.1.OR.IPID.EQ.0) THEN
            WRITE(LINES%HVAL,303) AT
            WRITE(ICE_FILES(CLGRFO)%LU,302,ADVANCE='NO')
     &        TRIM(ADJUSTL(LINES%HVAL))
            DO I=1,NTOTCLOG
              WRITE(ICE_FILES(CLGRFO)%LU,302,ADVANCE='NO')
     &          ( ' ' // TRIM(ADJUSTL(LINES%CELLS(I)%VAL(K)))
     &          , K=1,8 )
            ENDDO
            WRITE(ICE_FILES(CLGRFO)%LU,300) ''
          ENDIF
        ENDIF
!
      ENDIF
!
!=======================================================================
!
!     7 - STATIC BORDER ICE TRACKING
!
      IF( BD_ICE ) THEN
!
        IF( LT.LE.1 ) THEN
!
          DO K = 1,NPTFR
            IF( LIUBOR(K).EQ.KLOG ) THEN
              N = MESH%NBOR%I(K)
              IF( ICETYPE%I(N).EQ.1 ) ICELOC%I(N) = 2
            ENDIF
          ENDDO
!
        ENDIF
!
      ENDIF
!
!=======================================================================
!
!     ? - OTHER PROCESSES
!
      IF(NSECLOG.GT.0) THEN
        DEALLOCATE(QF)
        DEALLOCATE(QA)
        DEALLOCATE(QV)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
