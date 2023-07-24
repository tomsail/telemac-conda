!                     ***********************
                      SUBROUTINE CALCS3D_AED2
!                     ***********************
     &(NPOIN3,NPOIN2,NPLAN,Z,RHO,TA,TEXP,TIMP,LONGIT,
     & LATIT,AT,MARDAT,MARTIM,EXTCAED2,RAYAED2)
!
!***********************************************************************
! WAQTEL      V8P2
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS WHEN COUPLING WITH AED2 FOR 3D CASE
!         INCLUDES WAQ THERMIC PROCESS
!
!history  M JODEAU
!+        18/05/2016
!+        V7P3
!+       Creation
!
!history  M.-J. JODEAU
!+        20/09/2017
!+        V7P3
!+  Taking into account the extinction coefficient
!+  when coupling with T3D for the temperature
!
!history  S.E. BOURBAN (HRW)
!+        21/09/2017
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
!+        15/11/2018
!+        V8P0
!+   Solar radiation is stored for the T3D-AED2 coupling
!
!history  C.-T. PHAM (LNHE)
!+        31/07/2019
!+        V8P1
!+        Density RHO computed in drsurr in TELEMAC-3D and given to
!+        WAQTEL as optional (none in 2D) rather than computed again
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT            |-->| TIME IN SECONDS
!| EXTCAED2      |<--| EXTINCTION COEFFICIENT CALCULATED BY AED2
!| FLUXAED2      |<--| EXPLICIT SOURCE TERM CALCULATED BY AED2
!| LATIT         |-->| LATITUDE OF ORIGIN POINT
!| LONGIT        |-->| LONGITUTE OF ORIGIN POINT
!| MARDAT        |-->| DATE (YEAR,MONTH,DAY)
!| MARTIM        |-->| TIME (HOUR,MINUTE,SECOND)
!| NPLAN         |-->| NUMBER OF VERTICAL PLANES
!| NPOIN2        |-->| NUMBER OF NODES IN THE 2D MESH
!| NPOIN3        |-->| NUMBER OF NODES IN THE 3D MESH
!| RAYAED2       |<->| SOLAR RADIATION FOR AED2 COUPLING
!| RHO           |-->| WATER DENSITY
!| TEXP          |<--| EXPLICIT SOURCE TERM.
!| TIMP          |<--| IMLICIT SOURCE TERM
!| Z             |-->| Z COORDINATES FOR NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL,ONLY: CP_EAU,IND_T,SOLRADMETEO
      USE METEO_TELEMAC, ONLY: RAY3,CLDC
      USE EXCHANGE_WITH_ATMOSPHERE
#if defined HAVE_AED2
      USE T3D_AED2
      USE DECLARATIONS_WAQTEL,ONLY: FLUXAED2,DEJA_CA,NWAQ_AED2,RANK_AED2
#endif
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER       , INTENT(IN)    :: NPOIN3,NPOIN2,NPLAN
      INTEGER, INTENT(IN)           :: MARDAT(3),MARTIM(3)
      DOUBLE PRECISION, INTENT(IN)  :: Z(NPOIN3),LATIT,LONGIT,AT
      DOUBLE PRECISION, INTENT(IN)  :: RHO(NPOIN3)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TEXP,RAYAED2,TIMP
      TYPE(BIEF_OBJ), INTENT(IN)    :: TA
      DOUBLE PRECISION, INTENT(INOUT) :: EXTCAED2(NPOIN2,NPLAN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,IPLAN,ITRAC,IERR
      DOUBLE PRECISION LAMB
!     DOUBLE PRECISION RAY_SOL
!
      INTRINSIC EXP
!
!----------------------------------------------------------------------
!
#if defined HAVE_AED2
      IF(.NOT.DEJA_CA) THEN
        ALLOCATE(FLUXAED2(NPOIN2,NWAQ_AED2,NPLAN),STAT=IERR)
        DEJA_CA = .TRUE.
      ENDIF
!
! COMPUTATION OF THE SOURCE TERMS BY AED2
!
      CALL DO_AED2_MODELS(NPOIN3,NPOIN2,NPLAN,FLUXAED2,EXTCAED2,TA)
!
      DO J=1,NWAQ_AED2
        ITRAC = RANK_AED2(J)
        TEXP%ADR(ITRAC)%P%TYPR='Q'
        TIMP%ADR(ITRAC)%P%TYPR='Q'
        DO I=1,NPOIN2
          DO K=1,NPLAN
!           NEGATIVE FLUXES ARE DISCRETISED IMPLICITELY
            IF(FLUXAED2(I,J,K).GE.0.D0) THEN
              TEXP%ADR(ITRAC)%P%R(I+(K-1)*NPOIN2) =
     &        TEXP%ADR(ITRAC)%P%R(I+(K-1)*NPOIN2) + FLUXAED2(I,J,K)
!           BEWARE OF THE - SIGN. SIGN CONVENTION IN T3D
!           IMPLICIT SOURCE TERMS ARE SINKS
            ELSE
              TIMP%ADR(ITRAC)%P%R(I+(K-1)*NPOIN2) =
     &        TIMP%ADR(ITRAC)%P%R(I+(K-1)*NPOIN2) - FLUXAED2(I,J,K)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS (TO CHECK)
!
!     ACTIVATE BIEF_OBJ FOR FURTHER CALCULATIONS
!      TEXP%ADR(RANKTR1)%P%TYPR='Q'
!      TIMP%ADR(RANKTR2)%P%TYPR='Q'
!      TIMP%ADR(NTRAC  )%P%TYPR='Q'
!
!-----------------------------------------------------------------------
!
!     THERMIC PART, NOT A CALL TO CALCS3D_THERMICV
!
!-----------------------------------------------------------------------
!
!     SOURCE IN TEMPERATURE NOT EQUAL TO ZERO

      TEXP%ADR(IND_T)%P%TYPR='Q'
!
!     INCIDENT SOLAR RADIATION
!
!     IF(.NOT.SOLRADMETEO) THEN
!       CALL SOLRAD(RAY_SOL,NEBU,MARDAT,MARTIM,AT,LATIT,LONGIT)
!       CALL OS('X=C     ', X=RAY3, C=RAY_SOL)
!     ELSE
!!       RAY3 READ IN METEO SUBROUTINE
!       RAY_SOL = RAY3
!     ENDIF
!
!     CALL OS('X=C     ', X=RAYAED2, C=RAY_SOL)
!
      IF(SOLRADMETEO) CALL OS('X=Y     ', X=RAYAED2, Y=RAY3)
!
!     EXTINCTION COEFFICIENT CALCULATED BY AED2
!     TAKES INTO ACCOUNT BIOEXTINCTION & CO
!
!      SAL = 0.D0
      DO I=1,NPOIN2
        IF(.NOT.SOLRADMETEO) THEN
          CALL SOLRAD(RAY3%R(I),CLDC%R(I),MARDAT,MARTIM,AT,LATIT,
     &                LONGIT)
          RAYAED2%R(I) = RAY3%R(I)
        ENDIF
        DO IPLAN=1,NPLAN
          J = I + (IPLAN-1)*NPOIN2
!          TREEL=TA%ADR(IND_T)%P%R(NPOIN3-NPOIN2+I)
!          IF (IND_S.NE.0) THEN
!            SAL = TA%ADR(IND_S)%P%R(NPOIN3-NPOIN2+I)
!          ENDIF
!          RO=RO0*(1.D0-(7.D0*(TREEL-4.D0)**2-750.D0*SAL)*1.D-6)
          LAMB=RHO(NPOIN3-NPOIN2+I)*CP_EAU
          TEXP%ADR(IND_T)%P%R(J) = TEXP%ADR(IND_T)%P%R(J) +
     &             EXTCAED2(I,IPLAN)
     &            *EXP(EXTCAED2(I,IPLAN)*(Z(J)-Z(I+(NPLAN-1)*NPOIN2)))
     &            *RAY3%R(I)/LAMB
        ENDDO
      ENDDO
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
