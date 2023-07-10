!                   ******************
                    SUBROUTINE CONDICE
!                   ******************
!
     &( NPOIN,RECORD,AT,LISTIN )
!
!***********************************************************************
! KHIONE    V7P3
!***********************************************************************
!
!brief    INITIALISES THE PHYSICAL PARAMETERS FOR ICE.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |<->| TIME OF THE DATASET
!| LISTIN         |-->| IF YES, INFORMATIONS PRINTED ON LISTING
!| NPOIN          |-->| NUMBER OF POINT IN THE GEOMETRY MESH
!| RECORD         |-->| TIME STEP OF THE DATASET
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_KHIONE
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_KHIONE, EX_CONDICE => CONDICE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL         , INTENT(IN)    :: LISTIN
      INTEGER         , INTENT(IN)    :: NPOIN
      INTEGER         , INTENT(INOUT) :: RECORD
      DOUBLE PRECISION, INTENT(INOUT) :: AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER TROUVE(MAXVAR), I
      TROUVE = 0
!
!-----------------------------------------------------------------------
!
!   ATMOSPHERIC HEAT FLUXES
!
!     PHCL: SOLAR RAD (FLUX) REACHING SURFACE, UNDER CLEAR SKY
      CALL OS('X=C     ', X=PHCL, C=0.D0 )
!     PHRI: SOLAR RAD (FLUX) REACHING SURFACE, UNDER CLOUDY SKY
      CALL OS('X=C     ', X=PHRI, C=0.D0 )
!     PHPS: NET SOLAR RAD (FLUX) AFTER REFLECTION
      CALL OS('X=C     ', X=PHPS, C=0.D0 )
!     PHIB: EFFECTIVE BACK RADIATION (OPEN WATER OR ICE)
      CALL OS('X=C     ', X=PHIB, C=0.D0 )
!     PHIE: EVAPORATIVE HEAT TRANSFER
      CALL OS('X=C     ', X=PHIE, C=0.D0 )
!     PHIH: CONVECTIVE HEAT TRANSFER
      CALL OS('X=C     ', X=PHIH, C=0.D0 )
!     PHIP: HEAT TRANSFER DUE TO PRECIPITATION
      CALL OS('X=C     ', X=PHIP, C=0.D0 )
!     PHIW: HEAT TRANSFER BETWEEN WATER AND ICE
      CALL OS('X=C     ', X=PHIW, C=0.D0 )
!     SUMPH: NET SUM OF ALL THERMAL FLUXES (WATER SURFACE)
      CALL OS('X=C     ', X=SUMPH, C=0.D0 )
!     HWI: ICE WATER TRANSFER COEFF
      CALL OS('X=C     ', X=HWI, C=0.D0 )
!     SUMPH: NET SUM OF ALL THERMAL FLUXES (ICE COVER)
      CALL OS('X=C     ', X=SUMPH_ICE, C=0.D0 )
!
!-----------------------------------------------------------------------
!
!   FRAZIL ICE
!
!     FREEZING POINT OF WATER ( 0.oC BY DEFAULT )
      CALL OS('X=C     ', X=TMELT, C=CST_TMELT )
!     MEAN TURBULENT KINETIC ENERGY FOR FRAZIL THERMAL GROWTH
      CALL OS('X=C     ', X=KGM, C=0.D0 )
!     MEAN TURBULENT DISSIPATION RATE FOR FRAZIL THERMAL GROWTH
      CALL OS('X=C     ', X=EPSGM, C=0.D0 )
!     TURBULENT INTENSITY FOR FRAZIL THERMAL GROWTH
      CALL OS('X=C     ', X=ALPGM, C=0.D0 )
!     TURBULENT VISCOSITY FOR FRAZIL THERMAL GROWTH
      CALL OS('X=C     ', X=NUTGM, C=0.D0 )
      IF(THERMAL_BUDGET) THEN
!       TOTAL NUMBER OF PARTICLE
        CALL OS('X=C     ', X=NTOT, C=0.D0 )
!       TOTAL CONCENTRATION OF FRAZIL
        CALL OS('X=C     ', X=CTOT, C=0.D0 )
      ENDIF
!
!-----------------------------------------------------------------------
!
!   ICE COVER
!
!     PROBABILITY OF FRAZIL DEPOSITION - OPEN WATER
      CALL OS('X=C     ', X=THETA0, C=0.D0 )
!     PROBABILITY OF FRAZIL DEPOSITION - ICE COVER
      CALL OS('X=C     ', X=THETA1, C=0.D0 )
!     RATE OF REENTRAINMENT OF SURFACE PER UNIT AREA
      CALL OS('X=C     ', X=BETA1, C=0.D0 )
!     SETTLING VELOCITY OF FRAZIL ICE IN THE TURBULENT FLOW
      CALL OS('X=C     ', X=VBB, C=0.D0 )
!
!     MORE ICE THAN WATER AT SURFACE IF ANFEM(I) > 0.5
      CALL OS('X=C     ', X=ANFEM, C=0.D0 )
!     TOTAL ICE THICKNESS
      CALL OS('X=C     ', X=THIFEM, C=0.D0 )
!     SOLID ICE THICKNESS
      CALL OS('X=C     ', X=THIFEMS, C=0.D0 )
!
      CALL OS('X=C     ', X=ICESTR, C=FICE )
!
!     VERTICAL TURBULENT INTENSITY
      CALL OS('X=C     ', X=VZ, C=0.D0 )
!
!-----------------------------------------------------------------------
!
!     PREVIOUS ICE COVER COMPUTATION FILE
!
      IF( ICE_FILES(ICECOV)%NAME.NE.' ' ) THEN
        CALL READ_DATASET(ICE_FILES(ICECOV)%FMT,ICE_FILES(ICECOV)%LU,
     &    VARSOR,NPOIN,RECORD,AT,TEXTPR,TROUVE,
     &    ALIRE,LISTIN,RECORD.EQ.-1,MAXVAR)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISE ICE CHARACTERISATION (PRIME INTEGER)
      IF( TROUVE(20).EQ.1 ) THEN
        DO I = 1,NPOIN
          ICETYPE%I(I) = INT( ICETYPE%R(I)+1.D-3 )
        ENDDO
      ELSE
        ICETYPE%I = 1
      ENDIF
!
!-----------------------------------------------------------------------
!
!      NO MODIFICATION OF THE WATER DEPTH BY THE SOLID ICE COVER
!      CALL OS('X=X+CY  ', X=H, Y=THIFEMS, C=-RHO_ICE/RO0 )
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
