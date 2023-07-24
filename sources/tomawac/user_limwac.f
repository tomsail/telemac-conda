!                   **********************
                    SUBROUTINE USER_LIMWAC
!                   **********************
     &(F     , FBOR  , NPTFR , NDIRE , NF    , NPOIN2,
     & KENT  , PRIVE , NPRIV , IMP_FILE)
!
!***********************************************************************
! TOMAWAC
!***********************************************************************
!
!brief    USER LIMIT SPECTRUM.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| APHILL         |-->| BOUNDARY PHILLIPS CONSTANT
!| AT             |-->| COMPUTATION TIME
!| FMTBI1         |-->| BINARY FILE 1 FORMAT
!| BOUNDARY_COLOUR|-->| COLOUR OF BOUNDARY POINT (DEFAULT: ITS RANK)
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FBOR           |<->| SPECTRAL VARIANCE DENSITY AT THE BOUNDARIES
!| FETCHL         |-->| BOUNDARY MEAN FETCH VALUE
!| FPICL          |-->| BOUNDARY PEAK FREQUENCY
!| FPMAXL         |-->| BOUNDARY MAXIMUM PEAK FREQUENCY
!| FRA            |<--| DIRECTIONAL SPREADING FUNCTION VALUES
!| FRABL          |-->| BOUNDARY ANGULAR DISTRIBUTION FUNCTION
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GAMMAL         |-->| BOUNDARY PEAK FACTOR
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| HM0L           |-->| BOUNDARY SIGNIFICANT WAVE HEIGHT
!| IMP_FILE       |-->| MESH FILE WITH THE IMPOSED SPECTRA
!| KENT           |-->| B.C.: A SPECTRUM IS PRESCRIBED AT THE BOUNDARY
!| KSORT          |-->| B.C.: FREE BOUNDARY: NO ENERGY ENTERING THE DOMAIN
!| LIFBOR         |-->| TYPE OF BOUNDARY CONDITION ON F
!| LIMSPE         |-->| TYPE OF BOUNDARY DIRECTIONAL SPECTRUM
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| LUBI1           |-->| LOGICAL UNIT NUMBER OF THE USER BINARY FILE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NF             |-->| NUMBER OF FREQUENCIES
!| LUFO1           |-->| LOGICAL UNIT NUMBER OF THE USER FORMATTED FILE
!| NDIRE          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPRIV          |-->| NUMBER OF PRIVATE ARRAYS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| PRIVE          |-->| USER WORK TABLE
!| SIGMAL         |-->| BOUNDARY SPECTRUM VALUE OF SIGMA-A
!| SIGMBL         |-->| BOUNDARY SPECTRUM VALUE OF SIGMA-B
!| SPEC           |<--| VARIANCE DENSITY FREQUENCY SPECTRUM
!| SPEULI         |-->| INDICATES IF B.C. SPECTRUM IS MODIFIED BY USER
!| SPRE1L         |-->| BOUNDARY DIRECTIONAL SPREAD 1
!| SPRE2L         |-->| BOUNDARY DIRECTIONAL SPREAD 2
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TETA1L         |-->| BOUNDARY MAIN DIRECTION 1
!| TETA2L         |-->| BOUNDARY MAIN DIRECTION 2
!| UV, VV         |-->| WIND VELOCITIES AT THE MESH POINTS
!| VENSTA         |-->| INDICATES IF THE WIND IS STATIONARY
!| VENT           |-->| INDICATES IF WIND IS TAKEN INTO ACCOUNT
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XLAMDL         |-->| BOUNDARY WEIGHTING FACTOR FOR ANGULAR
!|                |   | DISTRIBUTION FUNCTION
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_USER_LIMWAC => USER_LIMWAC
!     USE DECLARATIONS_TOMAWAC, ONLY : UV2D,VV2D,PROF,FB_CTE,NPB,
!    &     LIMSPE, FPMAXL, FETCHL, SIGMAL, SIGMBL, GAMMAL, FPICL ,
!    &     HM0L  , APHILL, TETA1L, SPRE1L, TETA2L, SPRE2L, XLAMDL,
!    &     SPEULI, VENT  , VENSTA, GRAVIT, DEPTH , TETA  , FREQ  , X, Y,
!    &     SPEC  , FRA   , FRABL , AT    , LT    , DDC   , UV    , VV,
!    &     BOUNDARY_COLOUR,LIFBOR, NBOR, LUFO1, LUBI1, FMTBI1

      USE DECLARATIONS_SPECIAL
      USE BND_SPECTRA
      USE BIEF_DEF, ONLY : BIEF_FILE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      INTEGER, INTENT(IN)            :: NPTFR,NDIRE,NF,NPOIN2,NPRIV
      INTEGER, INTENT(IN)            :: KENT
      DOUBLE PRECISION, INTENT(IN)   :: PRIVE(NPOIN2,NPRIV)
      TYPE(BIEF_FILE), INTENT(IN)    :: IMP_FILE
      DOUBLE PRECISION, INTENT(INOUT):: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: FBOR(NPTFR,NDIRE,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) '*****  ERROR LIMWAC  ******'
      WRITE(LU,*)
     &     ' YOU DID NOT MODIFY THE BOUNDARY SPECTRUM WHEREAS '
      WRITE(LU,*) ' YOU ASK FOR THAT '
      CALL PLANTE(1)
      STOP
      RETURN
      END
