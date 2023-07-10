!                     ***************************
                      SUBROUTINE CALCS3D_THERMICS
!                     ***************************
!
     & (NPOIN2,NPOIN3,TA,ATABOS,BTABOS,PATMOS,ATMOSEXCH,WINDX,WINDY,RHO)
!
!***********************************************************************
! WAQTEL   V8P2
!***********************************************************************
!
!brief   COMPUTES BOUNDARY CONDITIONS FOR WAQ THERMIC PROCESS
!        COUPLED WITH T3D
!
!history  R. ATA
!+        21/02/2016
!+        V7P2
!+  Creation from old BORD3D (V7P0 and V7P1)
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
!history  C.-T. PHAM (LNHE)
!+        31/07/2019
!+        V8P1
!+        Density RHO computed in drsurr in TELEMAC-3D and given to
!+        WAQTEL as optional (none in 2D) rather than computed again
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DT             |-->| TIME STEP
!| DIMM           |-->| 2D OR 3D
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NPOIN          |-->| NUMBER OF NODES IN THE MESH
!| NTRAC          |-->| NUMBER OF TRACERS
!| PATMOS         |-->| ATMOSPHERIC PRESSURE
!| RHO            |-->| WATER DENSITY
!| TETAT          |-->| COEFFICIENT OF IMPLICITATION FOR TRACERS.
!| TEXP           |-->| EXPLICIT SOURCE TERM.
!| TIMP           |-->| IMPLICIT SOURCE TERM.
!| TN             |-->| TRACERS AT TIME N
!| TSCE           |-->| PRESCRIBED VALUES OF TRACERS AT POINT SOURCES
!| TSCEXP         |<--| EXPLICIT SOURCE TERM OF POINT SOURCES
!|                |   | IN TRACER EQUATION, EQUAL TO:
!|                |   | TSCE - ( 1 - TETAT ) TN
!| VOLU2D         |-->| BASES AREA (NON ASSEMBLED)
!| WINDX          |-->| X COMPONENT OF WIND VELOCITY
!| WINDY          |-->| Y COMPONENT OF WIND VELOCITY
!| YASMI          |<--| IF YES, THERE ARE IMPLICIT SOURCE TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL,ONLY:C_ATMOS,CP_EAU,IND_T,CFAER,N_C_ATMOS
      USE METEO_TELEMAC, ONLY: TAIR,CLDC,HREL
      USE EXCHANGE_WITH_ATMOSPHERE
      USE INTERFACE_WAQTEL, EX_CALCS3D_THERMICS => CALCS3D_THERMICS
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPOIN2,NPOIN3
      INTEGER, INTENT(IN)           :: ATMOSEXCH
      TYPE(BIEF_OBJ), INTENT(IN)    :: TA,WINDX,WINDY
      TYPE(BIEF_OBJ), INTENT(INOUT) :: ATABOS,BTABOS
      TYPE(BIEF_OBJ), INTENT(IN)    :: PATMOS,RHO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  LOCAL VARIABLES
!
      INTEGER          IPOIN2
      DOUBLE PRECISION TREEL,RO,LAMB
      DOUBLE PRECISION FACT,WW,WW2,A
      DOUBLE PRECISION RAY_ATM,RAY_EAU,FLUX_EVAP
      DOUBLE PRECISION FLUX_SENS,DEBEVAP
!
! ----------------------------------------------------------------
!
!     INITIALISATION
      CALL OS( 'X=0     ' ,X=ATABOS%ADR(IND_T)%P)
      CALL OS( 'X=0     ' ,X=BTABOS%ADR(IND_T)%P)
      IF(ATMOSEXCH.EQ.0) RETURN
      IF(ATMOSEXCH.EQ.1.OR.ATMOSEXCH.EQ.2) THEN
!
        FACT=LOG(1.D4)/LOG(5.D4)
        DO IPOIN2=1,NPOIN2
          TREEL=TA%ADR(IND_T)%P%R(NPOIN3-NPOIN2+IPOIN2)
!          IF( IND_S.EQ.0 ) THEN
!            SAL = 0.D0
!          ELSE
!            SAL = TA%ADR(IND_S)%P%R(NPOIN3-NPOIN2+IPOIN2)
!          ENDIF
!          RO = RO0*(1.D0-(7.D0*(TREEL-4.D0)**2-750.D0*SAL)*1.D-6)
          RO = RHO%R(NPOIN3-NPOIN2+IPOIN2)
          LAMB=RO*CP_EAU
!
          WW = SQRT(WINDX%R(IPOIN2)**2 + WINDY%R(IPOIN2)**2)
!
          IF(N_C_ATMOS.EQ.2.AND.ATMOSEXCH.EQ.2) THEN
            WW2 = WW
          ELSE
!           LOG LAW FOR WIND AT 2 METERS
!            WW2 = WW * LOG(2.D0/0.0002D0)/LOG(10.D0/0.0002D0)
!           WRITTEN BELOW AS:
            WW2 = WW * FACT
!           ALTERNATIVE LAW FOR WIND AT 2 METERS
!            WW2 = 0.6D0*WW
          ENDIF
!
          IF(ATMOSEXCH.EQ.1) THEN
            A=(4.48D0+0.049D0*TREEL+2021.5D0*C_ATMOS*(1.D0+WW)*
     &        (1.12D0+0.018D0*TREEL+0.00158D0*TREEL**2))/LAMB
            ATABOS%ADR(IND_T)%P%R(IPOIN2)=-A
            BTABOS%ADR(IND_T)%P%R(IPOIN2)= A*TAIR%R(IPOIN2)
          ELSEIF(ATMOSEXCH.EQ.2) THEN
!
!     SENSIBLE HEAT FLUXES
!
            CALL EVAPO(TREEL,TAIR%R(IPOIN2),WW2,PATMOS%R(IPOIN2),
     &                 HREL%R(IPOIN2),RO,FLUX_EVAP,FLUX_SENS,DEBEVAP,
     &                 C_ATMOS,CFAER(1),CFAER(2))
!
!     LONGWAVE HEAT FLUXES
!
            CALL SHORTRAD(TREEL,TAIR%R(IPOIN2),CLDC%R(IPOIN2),
     &                    HREL%R(IPOIN2),RAY_ATM,RAY_EAU)
!
!     BOUNDARY CONDITION FOR TEMPERATURE AT SURFACE
!
            ATABOS%ADR(IND_T)%P%R(IPOIN2) = 0.D0
            BTABOS%ADR(IND_T)%P%R(IPOIN2) = (RAY_ATM-RAY_EAU-FLUX_EVAP
     &                                      -FLUX_SENS)/LAMB
          ENDIF
        ENDDO
      ELSE
        WRITE(LU,*) "CALCS3D_THERMICS: MODELE EXCHANGE WITH  "
        WRITE(LU,*) "        THE ATMOSPHERE NOT IMPLEMENTED YET"
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
