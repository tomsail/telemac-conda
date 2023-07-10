!                   *****************
                    SUBROUTINE LIMWAC
!                   *****************
!
     &(F     , FBOR  , NPTFR , NDIRE , NF   ,  NPOIN2,
     & KENT  , PRIVE , NPRIV , IMP_FILE)
!
!***********************************************************************
! TOMAWAC   V7P3                                   23/02/2017
!***********************************************************************
!
!brief    BOUNDARY CONDITIONS.
!
!warning  BY DEFAULT, THE BOUNDARY CONDITIONS SPECIFIED IN THE FILE
!+            DYNAM ARE DUPLICATED ON ALL THE DIRECTIONS AND FREQUENCIES
!
!history  F. MARCOS (LNH)
!+        01/02/95
!+        V1P0
!+
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
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  E. GAGNAIRE-RENOU & J.-M. HERVOUET (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   A line IF(LIMSPE.EQ.0...) RETURN removed.
!
!history  A. JOLY (EDF R&D, LNHE)
!+        23/02/2017
!+        V7P3
!+   SPECTRA READ FROM AN EXTERNAL MESH CAN NOW BE IMPOSED ON THE
!+   OPEN BOUNDARIES.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| APHILL         |-->| BOUNDARY PHILLIPS CONSTANT
!| AT             |-->| COMPUTATION TIME
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
!| LIFBOR         |-->| TYPE OF BOUNDARY CONDITION ON F
!| LIMSPE         |-->| TYPE OF BOUNDARY DIRECTIONAL SPECTRUM
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NBI1           |-->| LOGICAL UNIT NUMBER OF THE USER BINARY FILE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NF             |-->| NUMBER OF FREQUENCIES
!| NFO1           |-->| LOGICAL UNIT NUMBER OF THE USER FORMATTED FILE
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
      USE INTERFACE_TOMAWAC, EX_LIMWAC => LIMWAC
      USE DECLARATIONS_TOMAWAC, ONLY : UV2D,VV2D,PROF,FB_CTE,NPB,
     &     LIMSPE, FPMAXL, FETCHL, SIGMAL, SIGMBL, GAMMAL, FPICL ,
     &     HM0L  , APHILL, TETA1L, SPRE1L, TETA2L, SPRE2L, XLAMDL,
     &     SPEULI, VENT  , VENSTA, DEPTH , SPEC  , FRA   , FRABL ,
     &     AT    , LT    , UV    , VV    , LIFBOR, NBOR  , REFLECTION

      USE DECLARATIONS_SPECIAL
      USE BND_SPECTRA
      USE BIEF_DEF, ONLY : BIEF_FILE
      IMPLICIT NONE
!
!
      INTEGER, INTENT(IN)            :: NPTFR,NDIRE,NF,NPOIN2,NPRIV
      INTEGER, INTENT(IN)            :: KENT
      DOUBLE PRECISION, INTENT(IN)   :: PRIVE(NPOIN2,NPRIV)
      TYPE(BIEF_FILE), INTENT(IN)    :: IMP_FILE
      DOUBLE PRECISION, INTENT(INOUT):: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: FBOR(NPTFR,NDIRE,NF)
!
      INTEGER IFF,IDIRE,IPTFR
      LOGICAL FLAG
!
!     REFLECTION BOUNDARY CONDITION
      INTEGER, PARAMETER :: KREFL = 8
!
!
!***********************************************************************
!
!     MODIFIES THE TYPE OF BOUNDARY CONDITION (OPTIONAL)
!
!     CAN BE CODED BY THE USER (SPEULI=.TRUE.)
!
!     LIFBOR(IPTFR)=KENT OR KSORT
!
      FLAG=.FALSE.
      IF (VENT .AND. (LIMSPE.EQ.1 .OR. LIMSPE.EQ.2 .OR. LIMSPE.EQ.3
     & .OR. LIMSPE.EQ.5)) FLAG=.TRUE.
      
!
!     THE FIRST TIME, ALLOCATES MEMORY FOR THE USEFUL ARRAYS
!     ---------------------------------------------------------------
!
      IF(LT.LT.1) THEN
        NPB=1
        IF(FLAG) THEN
          ALLOCATE(UV2D(1:NPTFR),VV2D(1:NPTFR))
          NPB=NPTFR
        ENDIF
        IF(LIMSPE.EQ.7 .OR. SPEULI) THEN
          IF (.NOT.ALLOCATED(PROF)) ALLOCATE(PROF(1:NPTFR))
          NPB=NPTFR
        ENDIF
        IF (REFLECTION) NPB=NPTFR
        IF(NPB.EQ.1) THEN
          IF (.NOT.ALLOCATED(FB_CTE)) ALLOCATE(FB_CTE(1:NDIRE,1:NF))
        ENDIF
      ENDIF
      IF (.NOT.ALLOCATED(UV2D)) ALLOCATE(UV2D(NPTFR))
!     MEMCHECK DO NOT LIKE FOR DEFERL_BJ78
      IF (.NOT.ALLOCATED(VV2D)) ALLOCATE(VV2D(NPTFR))
      IF (.NOT.ALLOCATED(PROF)) ALLOCATE(PROF(NPTFR))
      IF (.NOT.ALLOCATED(FB_CTE)) ALLOCATE(FB_CTE(1:NDIRE,1:NF))
!
!     THE FIRST TIME (AND POSSIBLY SUBSEQUENTLY IF THE WIND IS NOT
!     STATIONARY AND IF THE BOUNDARY SPECTRUM DEPENDS ON IT),
!     COMPUTES THE BOUNDARY SPECTRUM
!
      IF(LT.LT.1 .OR. (.NOT.VENSTA.AND.FLAG) .OR. SPEULI .OR.
     &   (IMP_FILE%NAME(1:1).NE.' ').OR. REFLECTION ) THEN
        IF(FLAG) THEN
          DO IPTFR=1,NPTFR
            UV2D(IPTFR)=UV(NBOR(IPTFR))
            VV2D(IPTFR)=VV(NBOR(IPTFR))
          ENDDO
        ENDIF
        IF(LIMSPE.EQ.7 .OR. SPEULI) THEN
          DO IPTFR=1,NPTFR
            PROF(IPTFR)=DEPTH(NBOR(IPTFR))
          ENDDO
        ENDIF
!
!       WHEN NPB=1 FBOR ONLY FILLED FOR FIRST POINT
!
!       SPECTRUM ON BOUNDARIES
!
        IF(NPB.EQ.NPTFR) THEN
          CALL SPEINI
     &(   FBOR  ,SPEC  ,FRA   ,UV2D  ,VV2D  ,FPMAXL,FETCHL,
     &    SIGMAL,SIGMBL,GAMMAL,FPICL ,HM0L  ,APHILL,TETA1L,
     &    SPRE1L,TETA2L,SPRE2L,XLAMDL,NPB   ,NDIRE ,NF    ,
     &    LIMSPE,PROF  ,FRABL )
        ELSE
          CALL SPEINI
     &(   FB_CTE,SPEC  ,FRA   ,UV2D  ,VV2D  ,FPMAXL,FETCHL,
     &    SIGMAL,SIGMBL,GAMMAL,FPICL ,HM0L  ,APHILL,TETA1L,
     &    SPRE1L,TETA2L,SPRE2L,XLAMDL,NPB   ,NDIRE ,NF    ,
     &    LIMSPE,PROF  ,FRABL )
        ENDIF
!
!       IF THERE IS A MESHED FILE WITH THE BOUNDARY SPECTRA
!       THEY NEED TO BE IMPOSED
!
        IF(IMP_FILE%NAME(1:1).NE.' ')THEN
          CALL IMPOSE_BND_SPECTRA(IMP_FILE,LT,AT,FBOR,NPTFR,NDIRE,NF)
        ENDIF
        
        IF (REFLECTION) THEN
          CALL REFLECT(NDIRE, NF, NPTFR, NPOIN2, FBOR, F, KREFL) 
        ENDIF
      ENDIF
      
!
!     ===========================================================
!     TO BE MODIFIED BY USER - RESU CAN BE CHANGED
!     ===========================================================
!
      IF(SPEULI) THEN
        CALL USER_LIMWAC
     &(F     , FBOR  , NPTFR , NDIRE , NF    , NPOIN2,
     & KENT  , PRIVE , NPRIV ,  IMP_FILE)
!
!     ===========================================================
!     END OF USER MODIFICATIONS
!     ===========================================================
!
      ELSE
!
!     -----------------------------------------------------------------
!     DUPLICATES THE BOUNDARY CONDITION FROM DYNAM ON ALL THE
!     DIRECTIONS AND FREQUENCIES, IF LIQUID BOUNDARY
!     -----------------------------------------------------------------
!
        IF(FLAG.OR.LIMSPE.EQ.7.OR.
     &  (IMP_FILE%NAME(1:1).NE.' ').OR.REFLECTION) THEN
          DO IPTFR=1,NPTFR
            IF(LIFBOR(IPTFR).EQ.KENT.OR.LIFBOR(IPTFR).EQ.KREFL) THEN
              DO IFF=1,NF
                DO IDIRE=1, NDIRE
                  F(NBOR(IPTFR),IDIRE,IFF)=FBOR(IPTFR,IDIRE,IFF)
                ENDDO
              ENDDO
            ENDIF
          ENDDO
        ELSE
          DO IPTFR=1,NPTFR
            IF(LIFBOR(IPTFR).EQ.KENT) THEN
              DO IFF=1,NF
                DO IDIRE=1, NDIRE
                  F(NBOR(IPTFR),IDIRE,IFF)=FB_CTE(IDIRE,IFF)
                ENDDO
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
