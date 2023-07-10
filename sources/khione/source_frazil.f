!                   ************************
                    SUBROUTINE SOURCE_FRAZIL
!                   ************************
!
     &(NPOIN,TEXP,TIMP,TN,HN,U,V,DT,CF,AK,EP,ITURB_TEL,LT,NPLAN)
!
!***********************************************************************
! KHIONE   V7P2                                             02/11/2016
!***********************************************************************
!
!brief    COMPUTES CONTRIBUTION TO TRACER SOURCE TERMS RESULTING
!+        FROM FRAZIL ICE PROCESSES.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK         |-->| TURBULENT KINETIC ENERGY K AT TIME T(N+1)
!| DT         |-->| TIME STEP
!| EP         |-->| TURBULENT ENERGY DISSIPATION AT TIME T(N+1)
!| HN         |-->| WATER DEPTH AT TIME N
!| IND_T      |-->| TRACER INDEX FOR WATER TEMPERATURE
!| ITURB_TEL  |-->| T2D TURBULENCE MODEL
!| LAMBD0     |-->| LATITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!| MARDAT     |-->| DATE (YEAR,MONTH,DAY)
!| MARTIM     |-->| TIME (HOUR,MINUTE,SECOND)
!| NPOIN      |-->| NUMBER OF NODES IN THE MESH
!| NTRAC      |-->| NUMBER OF TRACERS
!| TEXP       |<--| EXPLICIT SOURCE TERM.
!| TIMP       |<--| IMPLICIT SOURCE TERM.
!| U          |-->| X COMPONENT OF THE VELOCITY
!| V          |-->| Y COMPONENT OF THE VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE, ONLY : THERMAL_BUDGET,IND_FRA,
     &                IND_T,IND_S,NC_FRA,ANFEM,THIFEM,THIFEMS,
     &                TMELT,CP_EAU,RO0,RHO_ICE,LH_ICE,ITGM,KGM,
     &                EPSGM,ALPGM,NUTGM,SALINITY,INRJB,IND_DTI,
     &                DYN_ICOVER,SUM_SRCP,LISPRD
      USE METEO_TELEMAC, ONLY : SYNC_METEO
      USE THERMAL_KHIONE, ONLY : THERMAL_FLUXES,ICOVER_GROWTH,
     &                           WATERICE_HEAT_COEF
      USE FREEZEUP_KHIONE, ONLY : THERMAL_GROWTH,MELTING_POINT,
     &  SEEDING,SECONDARY_NUCLEATION,FLOCCULATION_BREAKUP,
     &  TURBULENT_PARAMETERS,EROSION_DEPOSITION
      USE INTERFACE_KHIONE, EX_SOURCE_FRAZIL => SOURCE_FRAZIL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN,ITURB_TEL,LT
      INTEGER, INTENT(IN), OPTIONAL  :: NPLAN
!
      DOUBLE PRECISION,INTENT(IN)    :: DT
!
      TYPE(BIEF_OBJ), INTENT(IN)     :: AK,EP
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TEXP,TIMP
      TYPE(BIEF_OBJ), INTENT(IN)     :: HN,U,V,CF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL                     :: MINFLAG,IMP
      INTEGER                     :: LIMFLAG
      INTEGER                     :: I,J,K,II,PLAN,IPLAN
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-2
      DOUBLE PRECISION            :: CONSTSS
      DOUBLE PRECISION            :: VMAG
      DOUBLE PRECISION            :: SRCT,SRCSR
      DOUBLE PRECISION            :: SRCGM_EXP(NC_FRA)
      DOUBLE PRECISION            :: SRCGM_IMP(NC_FRA)
      DOUBLE PRECISION            :: SRCSE(NC_FRA)
      DOUBLE PRECISION            :: SRCSN(NC_FRA)
      DOUBLE PRECISION            :: SRCFB(NC_FRA)
      DOUBLE PRECISION            :: SRCP(NC_FRA)
      DOUBLE PRECISION            :: SUM_SRC(NC_FRA)
      DOUBLE PRECISION            :: SUM_FRZL,SUM_SRCGM
!
      INTRINSIC MAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THERMAL BALANCE, SUPERCOOLING AND FRAZIL GROWTH
!
!-----------------------------------------------------------------------
!
      IMP = .FALSE.
      IF( LT.EQ.(LT/LISPRD)*LISPRD ) IMP = .TRUE.
!
      IF( THERMAL_BUDGET ) THEN
!
!     TEST IF COUPLING WITH T3D
      IF(PRESENT(NPLAN)) THEN
        PLAN = NPLAN
        TEXP%ADR(IND_T)%P%TYPR='Q'
        TIMP%ADR(IND_T)%P%TYPR='Q'
        IF(SALINITY) THEN
          TEXP%ADR(IND_S)%P%TYPR='Q'
        ENDIF
        DO I=1,NC_FRA
          TEXP%ADR(IND_FRA+I-1)%P%TYPR='Q'
          TIMP%ADR(IND_FRA+I-1)%P%TYPR='Q'
        ENDDO
      ELSE
        PLAN = 1
      ENDIF
!
!       MAJORATED RADIATION
        CONSTSS = 1.D0/(RO0*CP_EAU)
!
        DO I = 1,NPOIN
!
! ~~>     DOES NOT APPLY HEAT EXCHANGES AND FRAZIL SOURCES
!         ON DRY BANKS
          IF( HN%R(I).GT.EPS ) THEN
!
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       COMPUTING TOTAL ICE COVER THICKNESS
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            IF(DYN_ICOVER) THEN
!             TOTAL ti = STATIC ICE ti + DYN. ICE ti
              THIFEM%R(I) = THIFEMS%R(I) + TN%ADR(IND_DTI)%P%R(I)
            ELSE
              THIFEM%R(I) = THIFEMS%R(I)
            ENDIF
!
            DO IPLAN=1,PLAN
              II = NPOIN*(IPLAN-1)
!
! ~~>         FLOW SPEED
              VMAG = SQRT( U%R(I+II)**2 + V%R(I+II)**2 )
!
!             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>         SUPER COOLING AND FRAZIL ICE EVOLUTION
!             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ~~>         INITIALIZE SOURCES
              SRCT = 0.D0
              SRCSR = 0.D0
              SUM_FRZL = 0.D0
              SUM_SRCGM = 0.D0
!
              DO K=1,NC_FRA
                SRCGM_EXP(K) = 0.D0
                SRCGM_IMP(K) = 0.D0
                SRCSE(K) = 0.D0
                SRCSN(K) = 0.D0
                SRCFB(K) = 0.D0
                SRCP(K) = 0.D0
                SUM_SRC(K) = 0.D0
              ENDDO
!
! ~~>         TURBULENT PARAMETERS
              CALL TURBULENT_PARAMETERS(I+II,VMAG,HN%R(I),
     &          KGM%R(I+II),EPSGM%R(I+II),ALPGM%R(I+II),NUTGM%R(I+II),
     &          CF%R(I),AK,EP,ITURB_TEL)
!
! ~~>         SEEDING
              CALL SEEDING(I+II,HN%R(I),TN,TMELT%R(I+II),SRCSE,
     &          SUM_SRC,MINFLAG)
!
! ~~>         COMPUTE THERMAL GROWTH/DECAY
              CALL THERMAL_GROWTH(I+II,TN,TMELT%R(I+II),DT,
     &          SRCGM_EXP,SRCGM_IMP,SUM_SRCGM,SUM_FRZL,SUM_SRC,
     &          EPSGM%R(I+II),ALPGM%R(I+II),NUTGM%R(I+II),CONSTSS,
     &          LIMFLAG,IMP)

! ~~>         COMPUTE SECONDARY NUCLEATION AND FLOCCULATION
              CALL SECONDARY_NUCLEATION(I+II,TN,DT,SRCSN,SUM_SRC,
     &          EPSGM%R(I+II),LIMFLAG,MINFLAG)
!
! ~~>         COMPUTE FLOCCULATION AND BREAKUP
              CALL FLOCCULATION_BREAKUP(I+II,TN,DT,SRCFB,SUM_SRC,
     &          LIMFLAG,MINFLAG)
!
! ~~>         EXCHANGE WITH ICE COVER (EROSION/DEPOSITION)
              CALL EROSION_DEPOSITION(I+II,TN,DT,SRCP,SUM_SRCP%R(I),
     &          SUM_SRC,HN%R(I),ANFEM%R(I),THIFEM%R(I),
     &          LIMFLAG,MINFLAG)
!
! ~~>         EXPLICIT/IMPLICIT SOURCE TERM FOR FRAZIL CONC. K
!             ************************************************
              DO K=1,NC_FRA
                J = IND_FRA+K-1
                IF(ITGM.EQ.1) THEN
                  TEXP%ADR(J)%P%R(I+II) = TEXP%ADR(J)%P%R(I+II)
     &                               + SRCGM_EXP(K) + SRCSE(K)
     &                               + SRCSN(K) + SRCFB(K)
     &                               + SRCP(K)
                ELSE IF(ITGM.EQ.2) THEN
                  TEXP%ADR(J)%P%R(I+II) = TEXP%ADR(J)%P%R(I+II)
     &                               + SRCGM_EXP(K) + SRCSE(K)
     &                               + SRCSN(K) + SRCFB(K)
     &                               + SRCP(K)
                  IF(PLAN.GT.1) THEN
                    TIMP%ADR(J)%P%R(I+II) = TIMP%ADR(J)%P%R(I+II)
     &                                 - SRCGM_IMP(K)
                  ELSE
                    TIMP%ADR(J)%P%R(I+II) = TIMP%ADR(J)%P%R(I+II)
     &                                 + SRCGM_IMP(K)*HN%R(I)
                  ENDIF
                ENDIF
!
!               POSITIVITY WARNING
                IF(TEXP%ADR(J)%P%R(I+II).LT.(-TN%ADR(J)%P%R(I+II)/DT))
     &          THEN
                  WRITE(LU,*) ''
                  WRITE(LU,*) 'POSITIVITY OF FRAZIL CONC. '
                  WRITE(LU,*) 'MAY BE COMPROMISED '
                  WRITE(LU,*) ''
                  WRITE(LU,*) 'CHECK TIME STEP '
                  WRITE(LU,*) 'OR CHECK MODEL PARAMETERS '
                  WRITE(LU,*) ''
                  CALL PLANTE(1)
                  STOP
                ENDIF
              ENDDO
!
! ~~>         EXPLICIT/IMPLICIT SOURCE TERM FOR TEMPERATURE
!             *********************************************
              IF (INRJB.EQ.1) THEN
                SRCT = SUM_SRCGM*RHO_ICE*LH_ICE*CONSTSS
              ELSE IF(INRJB.EQ.2) THEN
                SRCT = SUM_SRCGM*RHO_ICE*LH_ICE*CONSTSS/(1.D0-SUM_FRZL)
     &               + SUM_SRCGM*RHO_ICE*(TN%ADR(IND_T)%P%R(I+II)-
     &                 TMELT%R(I+II))
     &                 /(RO0*(1.D0-SUM_FRZL))
              ENDIF
!
              IF(ITGM.EQ.1) THEN
                TEXP%ADR(IND_T)%P%R(I+II) = TEXP%ADR(IND_T)%P%R(I+II) +
     &           SRCT
              ELSE IF(ITGM.EQ.2) THEN
                TEXP%ADR(IND_T)%P%R(I+II) = TEXP%ADR(IND_T)%P%R(I+II)
     &                                 + SRCT*TMELT%R(I+II)
                IF(PLAN.GT.1) THEN
                  TIMP%ADR(IND_T)%P%R(I+II) = TIMP%ADR(IND_T)%P%R(I+II)
     &                                   + SRCT
                ELSE
                  TIMP%ADR(IND_T)%P%R(I+II) = TIMP%ADR(IND_T)%P%R(I+II)
     &                                   - SRCT*HN%R(I)
                ENDIF
              ENDIF
!
! ~~>         EXPLICIT SOURCE TERM FOR SALINITY (SALT REJECTION)
!             **************************************************
              IF (SALINITY) THEN
                SRCSR = RHO_ICE*TN%ADR(IND_S)%P%R(I+II)*SUM_SRCGM/RO0
                TEXP%ADR(IND_S)%P%R(I+II) = TEXP%ADR(IND_S)%P%R(I+II)
     &                                      + SRCSR
              ENDIF
            ENDDO !NPLAN
!
          ENDIF !HN.GT.EPS
        ENDDO !NPOIN
      ENDIF !THERMAL_BUDGET
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END
