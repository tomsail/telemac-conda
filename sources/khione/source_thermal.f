!                   *************************
                    SUBROUTINE SOURCE_THERMAL
!                   *************************
!
     &( NPOIN,TEXP,TN,HN,U,V,T1,S,MESH,DT,AT,MARDAT,MARTIM,LAMBD0,
     &  NPOIN3)
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
!| MESH       |<->| MESH STRUCTURE
!| NPOIN      |-->| NUMBER OF NODES IN THE MESH
!| NTRAC      |-->| NUMBER OF TRACERS
!| S          |<->| VOID STRUCTURE
!| T1         |<->| WORKING ARRAY
!| TEXP       |<--| EXPLICIT SOURCE TERM.
!| TN         |-->| TRACERS AT TIME N
!| U          |-->| X COMPONENT OF THE VELOCITY
!| V          |-->| Y COMPONENT OF THE VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_KHIONE, ONLY : THERMAL_BUDGET,BD_ICE,IND_T,IND_S,
     &                SUMPH,SUMPH_ICE,PHCL,PHRI,PHPS,PHIB,PHIE,PHIH,
     &                PHIP,PHIW,ANFEM,TMELT,TCR,BCH,
     &                CP_EAU,RO0,IND_DCI,DYN_ICOVER,HWI
      USE METEO_TELEMAC, ONLY : SYNC_METEO,RAINFALL,
     &                          WINDS,TAIR,TDEW,CLDC,VISBI
      USE THERMAL_KHIONE, ONLY : THERMAL_FLUXES,ICOVER_GROWTH,
     &                           WATERICE_HEAT_COEF
      USE FREEZEUP_KHIONE, ONLY : THERMAL_GROWTH,MELTING_POINT,
     &  SEEDING,SECONDARY_NUCLEATION,FLOCCULATION_BREAKUP,
     &  TURBULENT_PARAMETERS,EROSION_DEPOSITION
      USE INTERFACE_KHIONE, EX_SOURCE_THERMAL => SOURCE_THERMAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN
      INTEGER, INTENT(IN)            :: MARDAT(3),MARTIM(3)
      INTEGER, INTENT(IN), OPTIONAL  :: NPOIN3
!
      DOUBLE PRECISION,INTENT(IN)    :: DT,AT
!
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: T1,S
      TYPE(BIEF_MESH),INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)     :: U,V
      DOUBLE PRECISION, INTENT(IN)   :: LAMBD0,HN(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: TEXP(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                     :: I,K
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-2
      DOUBLE PRECISION            :: CONSTSS
      DOUBLE PRECISION            :: VMAG
      DOUBLE PRECISION            :: SRCT
      DOUBLE PRECISION            :: B2,B3,HIN
      DOUBLE PRECISION            :: ANFEMD
!
      INTRINSIC MAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
!
!=======================================================================
!
!     FOR GOOD MEASURE - SHOULD BE DONE AT THE TELEMAC-2D LEVEL
!
!-----------------------------------------------------------------------
!
!     NOTE THAT "AT" FROM DIFSOU IS AREADY TOO FAR GONE
      HIN = 0.D0
      CALL SYNC_METEO(AT)
!
!     TEST IF COUPLING WITH T3D
      IF(PRESENT(NPOIN3)) THEN
        K = NPOIN3 - NPOIN
      ELSE
        K = 0
      ENDIF
!
!=======================================================================
!
!     MELTING POINT
!
!-----------------------------------------------------------------------
!
      IF( IND_S.NE.0 ) THEN
        IF(PRESENT(NPOIN3)) THEN
          DO I = 1,NPOIN3
            TMELT%R(I) = MELTING_POINT( TN%ADR(IND_S)%P%R(I) )
          ENDDO
        ELSE
          DO I = 1,NPOIN
            TMELT%R(I) = MELTING_POINT( TN%ADR(IND_S)%P%R(I) )
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!
!     THERMAL BUDGET
!
!-----------------------------------------------------------------------
!
      IF( THERMAL_BUDGET ) THEN
!
!       MAJORATED RADIATION
        CONSTSS = 1.D0/(RO0*CP_EAU)
!
        DO I = 1,NPOIN
!
! ~~>     DOES NOT APPLY HEAT EXCHANGES ON DRY BANKS
          IF( HN(I).GT.EPS ) THEN
!
! ~~>       FLOW SPEED
            VMAG = SQRT( U%R(I+K)**2 + V%R(I+K)**2 )
!
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       COMPUTING TOTAL ICE COVER SURFACE FRACTION
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            IF(DYN_ICOVER) THEN
!             MAX SURFACE FRACTION
              ANFEMD = MIN(TN%ADR(IND_DCI)%P%R(I), 1.D0)
!             TOTAL Ci = MAX(STATIC ICE Ci, DYN. ICE Ci)
              ANFEM%R(I) = MAX(ANFEM%R(I), ANFEMD)
            ENDIF
!
!           ~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       ATMOSPHERIC HEAT FLUXES
!           ~~~~~~~~~~~~~~~~~~~~~~~
!           COMPUTATION OF HEAT FLUXES BETWEEN ATMO AND ICE COVER
            IF(ANFEM%R(I).GT. 0.D0) THEN
              CALL THERMAL_FLUXES(TAIR%R(I),TN%ADR(IND_T)%P%R(I+K),
     &          TMELT%R(I),TDEW%R(I),CLDC%R(I),VISBI%R(I),WINDS%R(I),
     &          RAINFALL%R(I),SUMPH_ICE%R(I),PHCL%R(I),PHRI%R(I),
     &          PHPS%R(I),PHIB%R(I),PHIE%R(I),PHIH%R(I),PHIP%R(I),
     &          ANFEM%R(I),DT,AT,MARDAT,MARTIM,LAMBD0,1)
            ELSE
              SUMPH_ICE%R(I) = 0.D0
            ENDIF
!
!           COMPUTATION OF HEAT FLUXES BETWEEN ATMO AND OPEN WATER
            CALL THERMAL_FLUXES(TAIR%R(I),TN%ADR(IND_T)%P%R(I+K),
     &        TMELT%R(I),TDEW%R(I),CLDC%R(I),VISBI%R(I),WINDS%R(I),
     &        RAINFALL%R(I),SUMPH%R(I),PHCL%R(I),PHRI%R(I),PHPS%R(I),
     &        PHIB%R(I),PHIE%R(I),PHIH%R(I),PHIP%R(I),ANFEM%R(I),
     &        DT,AT,MARDAT,MARTIM,LAMBD0,0)
!
!           RADIATIVE FLUX REMOVED FROM THE BALANCE IF COUPLING WITH 3D
            IF(PRESENT(NPOIN3)) THEN
              SUMPH%R(I) = SUMPH%R(I) - PHPS%R(I)
            ENDIF
!
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       HEAT FLUX BETWEEN WATER AND ICE COVER
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            HWI%R(I) = WATERICE_HEAT_COEF(
     &        HN(I),VMAG,TN%ADR(IND_T)%P%R(I+K),TMELT%R(I) )
            PHIW%R(I) = - HWI%R(I)*(TN%ADR(IND_T)%P%R(I+K) - TMELT%R(I))
!
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       SOURCE TERM FOR EXCHANGES AT THE SURFACE
!           (HEAT EXCHANGES WITH THE ATMOSPHERE AND ICE COVER)
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            SRCT = CONSTSS*( SUMPH%R(I)*(1.D0-ANFEM%R(I))
     &           + PHIW%R(I)*ANFEM%R(I) )/ HN(I)
            TEXP(I) = TEXP(I) + SRCT
!
          ELSE
!
!           ~~~~~~~~~~~~~~~~~~~~~~~
! ~~>       ATMOSPHERIC HEAT FLUXES
!           ~~~~~~~~~~~~~~~~~~~~~~~
            PHCL%R(I) = 0.D0
            PHRI%R(I) = 0.D0
            PHPS%R(I) = 0.D0
            PHIB%R(I) = 0.D0
            PHIE%R(I) = 0.D0
            PHIH%R(I) = 0.D0
            PHIP%R(I) = 0.D0
!
            SUMPH%R(I) = 0.D0
            SUMPH_ICE%R(I) = 0.D0
!
          ENDIF
!
        ENDDO
!
      ENDIF
!
!
!=======================================================================
!
!     STATIC BORDER ICE GROWTH
!
!-----------------------------------------------------------------------
!
      IF( BD_ICE ) THEN
!
!-----------------------------------------------------------------------
!       PREPARATION TO STATIC BORDER ICE GROWTH
!
!       AREA AROUND NODES
!       TODO: CAN BE REPLACED BY VOLU2DPAR
        CALL VECTOR( T1,'=','MASBAS          ',U%ELM,1.D0,
     &                S,S,S,S,S,S,MESH,.FALSE.,S )
!       /!\ TODO: PARALELISATION
!
        B2 = BCH - 5.87D0 * LOG(BCH)
!
        DO I = 1,NPOIN
! ~~>     WIND SPEED EFFECTS ON ICE
!
! ~~>     FLOW SPEED EFFECTS ON ICE
          VMAG = SQRT( U%R(I)**2 + V%R(I)**2 )
!
! ~~>     CRITICAL AIR TEMPERATURE FOR BORDER ICE GROWTH
          B3 = B2 + 5.87D0 * LOG( MAX( BCH, 2.D0*SQRT(T1%R(I)) ) )
          TCR%R(I) = TN%ADR(IND_T)%P%R(I) +
     &       SUMPH%R(I) / ( 1130.D0 * VMAG + B3 * WINDS%R(I))
          IF( ( TCR%R(I).LT.0.3D0*TAIR%R(I) ).AND.
     &        ( TAIR%R(I).LT.0.0D0 ) ) THEN
            TCR%R(I) = 0.3D0 * TAIR%R(I)
          ENDIF
          IF( SUMPH%R(I).GT.0.D0 ) TCR%R(I) = TAIR%R(I)
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
      END
