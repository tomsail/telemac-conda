!                   ************************
                    SUBROUTINE SOURCE_ICOVER
!                   ************************
!
     &(NPOIN,TEXP,TN,HN,DT)
!
!***********************************************************************
! KHIONE   V7P2                                             02/11/2016
!***********************************************************************
!
!brief    COMPUTES CONTRIBUTION TO TRACER SOURCE TERMS RESULTING
!+        FROM FRAZIL ICE PROCESSES.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT         |-->| TIME IN SECONDS
!| DT         |-->| TIME STEP
!| HN         |-->| WATER DEPTH AT TIME N
!| IND_T      |-->| TRACER INDEX FOR WATER TEMPERATURE
!| LAMBD0     |-->| LATITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!| MARDAT     |-->| DATE (YEAR,MONTH,DAY)
!| MARTIM     |-->| TIME (HOUR,MINUTE,SECOND)
!| NPOIN      |-->| NUMBER OF NODES IN THE MESH
!| NTRAC      |-->| NUMBER OF TRACERS
!| TEXP       |<--| EXPLICIT SOURCE TERM.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE, ONLY : THERMAL_BUDGET,IND_T,ICETYPE,
     &                ICELOC,ICETYPEP,SUMPH_ICE,ANFEM,THIFEM,THIFEMS,
     &                TMELT,IND_DCI,IND_DTI,PRE_MIN,DYN_ICOVER,HWI,
     &                SUM_SRCP,IPREC
      USE METEO_TELEMAC, ONLY : SYNC_METEO
      USE THERMAL_KHIONE, ONLY : THERMAL_FLUXES,ICOVER_GROWTH,
     &                           WATERICE_HEAT_COEF
      USE FREEZEUP_KHIONE, ONLY : THERMAL_GROWTH,MELTING_POINT,
     &  SEEDING,SECONDARY_NUCLEATION,FLOCCULATION_BREAKUP,
     &  TURBULENT_PARAMETERS,EROSION_DEPOSITION
      USE INTERFACE_KHIONE, EX_SOURCE_ICOVER => SOURCE_ICOVER
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN
!
      DOUBLE PRECISION,INTENT(IN)    :: DT
!
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TEXP
      TYPE(BIEF_OBJ), INTENT(IN)     :: HN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                     :: I,IT,IL,ITP
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-2
      DOUBLE PRECISION            :: SRCCI_P,SRCCI_M,SRCCI
      DOUBLE PRECISION            :: SRCTI_P,SRCTI_M,SRCTI
      DOUBLE PRECISION            :: SRCTI_TMP,SRCTI_BDI
      DOUBLE PRECISION            :: CIDIFF,TIDIFF
!
      INTRINSIC MAX
!
!=======================================================================
!
!     DYNAMIC AND STATIC ICE COVER EVOLUTION
!
!-----------------------------------------------------------------------
!
      IF( THERMAL_BUDGET ) THEN
!
        DO I = 1,NPOIN
!
! ~~>     DOES NOT APPLY HEAT EXCHANGES ON DRY BANKS
          IF( HN%R(I).GT.EPS ) THEN
!
! ~~>       INIT ICE COVER SOURCES
            SRCTI_TMP = 0.D0
            SRCTI_BDI = 0.D0
            IF(DYN_ICOVER) THEN
              SRCCI_M = 0.D0
              SRCTI_M = 0.D0
              SRCCI_P = 0.D0
              SRCTI_P = 0.D0
            ENDIF
!
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       MASS EXCHANGES BETWEEN SUSPENDED FRAZIL ICE AND
!           THE ICE COVER I.E. DEPOSITION AND EROSION
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            IF(IPREC.GT.0)THEN
!             DYNAMIC ICE LAYER
              IF((DYN_ICOVER).AND.(ICETYPE%I(I).LE.1)) THEN
                IF((TN%ADR(IND_DTI)%P%R(I).LE.PRE_MIN).AND.
     &             (TN%ADR(IND_DCI)%P%R(I).LT.1.D0)) THEN
                  SRCCI_M = -SUM_SRCP%R(I)/PRE_MIN
                ELSE
                  SRCTI_M = -SUM_SRCP%R(I)
                ENDIF
!             STATIC ICE LAYER (BORDER ICE)
              ELSE
                IF(ICETYPE%I(I).GT.1) THEN
                  SRCTI_BDI = -SUM_SRCP%R(I)
                ENDIF
              ENDIF
            ENDIF
!
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       THERMAL EXPANSION/DECAY OF THE ICE COVER
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            IF(ANFEM%R(I).GT.0.D0) THEN
              CALL ICOVER_GROWTH( SRCTI_TMP,TN%ADR(IND_T)%P%R(I),
     &          TMELT%R(I),SUMPH_ICE%R(I),THIFEM%R(I),HWI%R(I),DT)
!
!             DYNAMIC ICE LAYER
              IF((DYN_ICOVER).AND.(ICETYPE%I(I).LE.1)) THEN
                IF(TN%ADR(IND_DCI)%P%R(I).LT.1.D0) THEN
                  IF (SRCTI_TMP.LE.0.D0) THEN
                    SRCCI_P = SRCTI_TMP/PRE_MIN
                  ENDIF
                ELSE
                  SRCTI_P = SRCTI_TMP
                ENDIF
!
!             STATIC ICE LAYER (BORDER ICE)
              ELSE
                IF(ICETYPE%I(I).GT.1) THEN
                  SRCTI_BDI = SRCTI_BDI + SRCTI_TMP
                  IF( SRCTI_BDI.LE.(-1.D0*THIFEMS%R(I)) ) THEN
!                   COMPLETE MELTDOWN OF STATIC ICE COVER
                    THIFEMS%R(I) = 0.D0
                  ELSE
!                   STANDARD THERMAL EXPANSION OR MELTING
                    THIFEMS%R(I) = THIFEMS%R(I) + SRCTI_BDI*DT
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
!
!           COMPLETE MELT DOWN OF BORDER ICE
            IF( THIFEMS%R(I).LE.0.D0 .AND.
     &          ANFEM%R(I).LE.0.D0 ) THEN
              IT = ICETYPE%I(I)
              ITP = ICETYPEP%I(I)
              IL = ICELOC%I(I)
              IF( IT.EQ.2 ) ICETYPE%I(I) = 1
              IF( IL.EQ.3 ) ICELOC%I(I) = 1
              IF( ITP.EQ.2 ) ICETYPEP%I(I) = 1
            ENDIF
!
! ~~>       EXPLICIT SOURCE TERM FOR THE ICE COVER
!           **************************************
            IF(DYN_ICOVER) THEN
              SRCCI = SRCCI_M + SRCCI_P
              SRCTI = SRCTI_M + SRCTI_P
!
! ~~>         SPLITING OF THE SOURCES
!
!             SURFACE FRACTION LOWER THAN MAXIMUM
!             > SOURCE ON SURFACE FRACTION / CONSTANT THICKNESS
              IF(TN%ADR(IND_DCI)%P%R(I).LT.1.D0) THEN
!               IF SOURCE GIVES GREATER SURFACE FRACTION THAN 1
!               > SPLIT SOURCE AND UPDATE BOTH FRACTION AND THICKNESS
                IF(TN%ADR(IND_DCI)%P%R(I) +
     &             DT*SRCCI.GT.1.D0) THEN
                  CIDIFF = 1.D0 - TN%ADR(IND_DCI)%P%R(I)
                  SRCTI = PRE_MIN*(SRCCI - CIDIFF/DT)
                  SRCCI = CIDIFF/DT
!               IF SOURCE GIVES SURFACE FRACTION LOWER THAN 1
!               > UPDATE OF SURFACE FRACTION AT MINIMAL THICKNESS
                ELSE
                  SRCTI = 0.D0
                ENDIF
!             SURFACE FRACTION AT MAXIMUM
!             > SOURCE ON SURFACE THICKNESS
              ELSE
!               IF SOURCE GIVES LOWER THICKNESS THAN PRE_MIN
!               > SPLIT SOURCE AND UPDATE BOTH FRACTION AND THICKNESS
                IF(MAX(TN%ADR(IND_DTI)%P%R(I),PRE_MIN) + 
     &             DT*SRCTI.LT.PRE_MIN) THEN 
                  TIDIFF = PRE_MIN - MAX(TN%ADR(IND_DTI)%P%R(I),PRE_MIN)
                  ! HERE SRCTI<0 AND -TIDIFF>0
                  SRCCI = (SRCTI - TIDIFF/DT)/PRE_MIN 
                  SRCTI = TIDIFF/DT
!               IF SOURCE GIVES GREATER THICKNESS THAN PRE_MIN
!               > UPDATE OF THICKNESS AT MAXIMAL SURFACE FRACTION
                ELSE
                  SRCCI = 0.D0
                ENDIF
              ENDIF
!
! ~~>         COMPLETE MELTDOWN OF DYNAMIC ICE COVER (MIN LIMITORS)
              SRCTI = MAX(SRCTI, -TN%ADR(IND_DTI)%P%R(I)/DT)
              SRCCI = MAX(SRCCI, -TN%ADR(IND_DCI)%P%R(I)/DT)
!
! ~~>         SOURCE FOR ICE COVER FRACTION
              TEXP%ADR(IND_DCI)%P%R(I) = TEXP%ADR(IND_DCI)%P%R(I)
     &                                 + SRCCI
! ~~>         SOURCE FOR ICE COVER THICKNESS
              TEXP%ADR(IND_DTI)%P%R(I) = TEXP%ADR(IND_DTI)%P%R(I)
     &                                 + SRCTI
            ENDIF
!
          ENDIF !HN.GT.EPS
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END
