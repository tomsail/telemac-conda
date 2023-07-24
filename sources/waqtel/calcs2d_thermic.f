!                     **************************
                      SUBROUTINE CALCS2D_THERMIC
!                     **************************
!
     & (NPOIN,TN,TEXP,HPROP,PATMOS)
!
!
!***********************************************************************
! WAQTEL   V8P1
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR  WAQ THERMIC PROCESS
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION
!
!history  S.E. BOURBAN (HRW)
!+        07/06/2017
!+        V7P3
!+        Indexing tracer (IND_*) to avoid conflicting naming convention
!+        between user defined tracers, water quality processes and
!+        ice processes. Introduction of the array RANK_*.
!
!history  S.E. BOURBAN (HRW)
!+        25/09/2017
!+        V7P3
!+        TEXP and TIMP are now additive to account for a variety of
!+        of sources / sinks on a given TRACER
!
!history  C.-T. PHAM
!+        17/01/2018
!+        V7P3
!+        The calculation of P_VAP_SAT has been moved before used in
!+        the calculation of PATMC
!
!history  C.-T. PHAM
!+        13/11/2019
!+        V8P1
!+        The calculation of HA is to be done with P_VAP, not P_VAP_SAT
!+        so PATMC is not to used for HA, only for HA_SAT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DT             |-->| TIME STEP
!| DIMM           |-->| 2D OR 3D
!| HPROP          |-->| PROPAGATION DEPTH
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NPOIN          |-->| NUMBER OF NODES IN THE MESH
!| NTRAC          |-->| NUMBER OF TRACERS
!| PATMOS         |-->| ATMOSPHERIC PRESSURE
!| TETAT          |-->| COEFFICIENT OF IMPLICITATION FOR TRACERS.
!| TEXP           |-->| EXPLICIT SOURCE TERM.
!| TIMP           |-->| IMPLICIT SOURCE TERM.
!| TN             |-->| TRACERS AT TIME N
!| TSCE           |-->| PRESCRIBED VALUES OF TRACERS AT POINT SOURCES
!| TSCEXP         |<--| EXPLICIT SOURCE TERM OF POINT SOURCES
!|                |   | IN TRACER EQUATION, EQUAL TO:
!|                |   | TSCE - ( 1 - TETAT ) TN
!| VOLU2D         |-->| BASES AREA (NON ASSEMBLED)
!| YASMI          |<--| IF YES, THERE ARE IMPLICIT SOURCE TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!***********************************************************************
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL,ONLY: COEF_K,EMA,CFAER,BOLTZ,CP_EAU,
     &                              CP_AIR,EMI_EAU,RO0,IND_T
      USE METEO_TELEMAC, ONLY: TAIR,PVAP,RAY3,WINDS,CLDC
!      USE EXCHANGE_WITH_ATMOSPHERE
      USE INTERFACE_WAQTEL, EX_CALCS2D_THERMIC => CALCS2D_THERMIC
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN
      TYPE(BIEF_OBJ), INTENT(IN)      :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TEXP
      TYPE(BIEF_OBJ), INTENT(IN)      :: HPROP
      TYPE(BIEF_OBJ), INTENT(IN)      :: PATMOS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  LOCAL VARIABLES
!
      INTEGER                     :: I
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION            :: CE,CV,RE,L_VAP
      DOUBLE PRECISION            :: RA,PATMC
      DOUBLE PRECISION            :: TEMPER,HA_SAT
      DOUBLE PRECISION            :: ROA,HA,P_VAP_SAT
      DOUBLE PRECISION   :: CONSTCE,CONSTCV,CONSTRA,CONSTSS,CONSTRE
!
      INTRINSIC MAX
!
! ----------------------------------------------------------------
!
!     SOME OPTIMIZATION
      CONSTRE = EMI_EAU*BOLTZ
!
!     MAJORATED RADIATION
!
      CONSTSS = 1.D0/(RO0*CP_EAU)
!
!     LOOP OVER ALL MESH POINTS
!
      DO I=1,NPOIN
        CONSTCV = CP_AIR*(CFAER(1)+CFAER(2)*WINDS%R(I))
        CONSTCE = CFAER(1)+CFAER(2)*WINDS%R(I)
        TEMPER = TN%ADR(IND_T)%P%R(I)
!       PRESSURE OF EVAPORATION
        P_VAP_SAT = 6.11D0*EXP(17.27D0*TEMPER /(TEMPER+237.3D0))
!       AIR DENSITY
        ROA = 100.D0*PATMOS%R(I)/((TAIR%R(I)+273.15D0)*287.D0)
!       AIR SPECIFIC MOISTURE
        PATMC=PATMOS%R(I)-0.378D0*P_VAP_SAT
        HA  = 0.622D0*PVAP%R(I)/(MAX(PATMOS%R(I)-0.378D0*PVAP%R(I),EPS))
!       RADIATION ON WATER SURFACE
        RE = CONSTRE*(TEMPER+273.15D0)**4
!       ADVECTIVE HEAT FLUX
        CV = ROA*CONSTCV*(TEMPER-TAIR%R(I))
!       VAPOR LATENT HEAT
        L_VAP = 2500900.D0 - 2365.D0*TEMPER
!       AIR MOISTURE AT SATURATION
        IF(ABS(PATMC).GT.EPS) THEN
          HA_SAT = 0.622D0*P_VAP_SAT/PATMC
        ELSE
          HA_SAT = 0.D0
        ENDIF
!       EVAPORATION HEAT FLUX
        CE = ROA*L_VAP*CONSTCE*(HA_SAT-HA)
!       ATMOSPHERIC RADIATION
        CONSTRA = EMA*BOLTZ *(TAIR%R(I)+273.15D0)**4 *
     &            (1.D0+COEF_K*(CLDC%R(I)/8.D0)**2)
        IF(HA_SAT.LT.HA) THEN
          RA = 1.8D0*CONSTRA
        ELSE
          RA = CONSTRA
        ENDIF
!       READY TO INTRODUCE SOURCE TERM
        TEXP%ADR(IND_T)%P%R(I) = TEXP%ADR(IND_T)%P%R(I) +
     &            CONSTSS*(RAY3%R(I)+RA-RE-CV-CE) / MAX(HPROP%R(I),EPS)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
!
      END SUBROUTINE
