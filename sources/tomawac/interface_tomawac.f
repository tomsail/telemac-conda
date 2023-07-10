!
!  INTERFACES OF TOMAWAC SUBROUTINES
!
      MODULE INTERFACE_TOMAWAC
!
!=======================================================================
!
!     DEFINITION OF STRUCTURES
!
      USE BIEF_DEF
!
!-----------------------------------------------------------------------
!
!     DEFINITION OF INTERFACES
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ANACOS
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ANAMAR
      IMPLICIT NONE

        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ANAVEN
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ANGLES(XLAMD , DTPLUS, DTMOIN)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN)    :: XLAMD
      DOUBLE PRECISION, INTENT(INOUT) :: DTPLUS, DTMOIN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE BORNES(B , N , A , XM , X0 , X1 )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             ::  N
      DOUBLE PRECISION, INTENT(IN)    :: B , A , XM
      DOUBLE PRECISION, INTENT(INOUT) :: X0 , X1
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CONDIW (PART, U_TEL, V_TEL, H_TEL,
     &   WINDUTEL, WINDVTEL)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: PART
      TYPE(BIEF_OBJ), INTENT(IN)      :: U_TEL,V_TEL,H_TEL
      TYPE(BIEF_OBJ), INTENT(IN)      :: WINDUTEL, WINDVTEL
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CONW4D
     &(CX, CY, CT, CF, XK, CG, NPOIN2, NDIRE, JF, NF)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2,JF
      DOUBLE PRECISION, INTENT(INOUT) :: CX(NPOIN2,NDIRE,JF)
      DOUBLE PRECISION, INTENT(INOUT) :: CY(NPOIN2,NDIRE,JF)
      DOUBLE PRECISION, INTENT(INOUT) :: CT(NPOIN2,NDIRE,JF)
      DOUBLE PRECISION, INTENT(INOUT) :: CF(NPOIN2,NDIRE,JF)
      DOUBLE PRECISION, INTENT(IN)    :: CG(NPOIN2,NF),XK(NPOIN2,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CONWAC
     &( CX    , CY    , CT    , XK    , CG    , NPOIN2, NDIRE , JF    ,
     &  NF    )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2,JF
      DOUBLE PRECISION, INTENT(IN)    :: CG(NPOIN2,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CX(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: CY(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: CT(NPOIN2,NDIRE)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CORMAR( PART, U_TEL, V_TEL, H_TEL,
     &   WINDXTEL, WINDYTEL)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER,          INTENT(IN) :: PART
      TYPE(BIEF_OBJ),   INTENT(IN) :: U_TEL,V_TEL,H_TEL
      TYPE(BIEF_OBJ),   INTENT(IN) :: WINDXTEL, WINDYTEL
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        FUNCTION COUPLE
     &( XK1   , YK1   , XK2   , YK2   , XK3   , YK3   , XK4   , YK4   )
      IMPLICIT NONE
      DOUBLE PRECISION , INTENT(IN)    :: XK1   , YK1   , XK2   , YK2
      DOUBLE PRECISION , INTENT(IN)    :: XK3   , YK3   , XK4   , YK4
      DOUBLE PRECISION COUPLE
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE COUUTI
     &(NCOU,FMTCOU)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NCOU
      CHARACTER(LEN=8), INTENT(IN)    :: FMTCOU
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_CURRENT
     &(NCOU,FMTCOU)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NCOU
      CHARACTER(LEN=8), INTENT(IN)    :: FMTCOU
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CQUEUE
     &( JFRE  , JBIS  , COEF1 )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: JFRE
      DOUBLE PRECISION, INTENT(INOUT) :: COEF1
      INTEGER, INTENT(INOUT)          :: JBIS
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        FUNCTION DELFRA( SS    )
      IMPLICIT NONE
      DOUBLE PRECISION DELFRA, SS
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DIFFRAC
     &( CX    , CY    , CT    , XK    , CG    , NPOIN2,
     &  NDIRE , IFF   , NF    , F     , RX    , RY    ,
     &  RXX   , RYY   , NEIGB )
        USE BIEF_DEF
        USE DECLARATIONS_TOMAWAC, ONLY : MAXNSP
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NF, NDIRE, NPOIN2, IFF
      INTEGER, INTENT(IN) :: NEIGB(NPOIN2,MAXNSP)
      DOUBLE PRECISION, INTENT(INOUT) :: CX(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: CY(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: CT(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(IN)    :: CG(NPOIN2,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: RX(MAXNSP,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: RY(MAXNSP,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: RXX(MAXNSP,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: RYY(MAXNSP,NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DUMP2D(  XF1 , NP1 )
      IMPLICIT NONE
      INTEGER, INTENT(IN)          :: NP1
      DOUBLE PRECISION, INTENT(IN) :: XF1(NP1)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ECRETE
     &( F     , DEPTH , NPOIN2, NDIRE , NF    , PROMIN)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPOIN2 , NDIRE, NF
      DOUBLE PRECISION, INTENT(IN)    :: DEPTH(NPOIN2),PROMIN
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ECRSPE
     &( F     , NDIRE , NF    , NPOIN2, LT, AUXIL , NOLEO ,
     &  NLEO  , DEBRES,  DATE , TIME  , KNOLG , MESH )
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPOIN2,NLEO,NF,NDIRE,LT
      INTEGER, INTENT(IN)             :: KNOLG(*),NOLEO(NLEO)
      INTEGER, INTENT(IN)             :: DATE(3),TIME(3)
      DOUBLE PRECISION, INTENT(INOUT) :: AUXIL(NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      LOGICAL, INTENT(IN)             :: DEBRES
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE F1F1F1
     &( F1SF  , NF1   , IQ_OM1)
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: IQ_OM1
      INTEGER,          INTENT(INOUT) :: NF1
      DOUBLE PRECISION, INTENT(INOUT) :: F1SF(*)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FBOTT3D
     &  (FBX, FBY, FS, NPOIN2, XK, NDIRE, NF)
      IMPLICIT NONE
      INTEGER, INTENT(IN)    :: NPOIN2, NDIRE, NF
      DOUBLE PRECISION, INTENT(IN)    :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT)    :: FBX(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT)    :: FBY(NPOIN2)  
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FDISS3D
     &  (FDX, FDY, NPOIN2, XK, NDIRE, FS, NF)
      IMPLICIT NONE
        INTEGER, INTENT(IN) :: NPOIN2, NDIRE, NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2, NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2, NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FDX(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FDY(NPOIN2)  
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
          SUBROUTINE FDISSK    
     &        (FDK, NPOIN2, NDIRE, FS, ZTEL, NZ, HSMJT, FZNORM, NF)
      IMPLICIT NONE
      INTEGER, INTENT(IN)  :: NZ  
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE, NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2, NDIRE, NF)
      DOUBLE PRECISION, INTENT(IN) :: HSMJT(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FDK(NPOIN2, NZ)
      DOUBLE PRECISION, INTENT(IN) :: ZTEL(NPOIN2, NZ)
      DOUBLE PRECISION, INTENT(INOUT) :: FZNORM(NPOIN2)
          END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FILT_SA
        IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        FUNCTION FONCRO
     &( X     , B     , N     , A     , XM    )
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: N
      DOUBLE PRECISION, INTENT(IN)    :: X      , B     , A     , XM
      DOUBLE PRECISION FONCRO
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FPREAD( FREAD, F, NF, NDIRE, NPOIN2, EXPO )
      IMPLICIT NONE
      INTEGER,          INTENT(IN) :: NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN) :: EXPO  , F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FREAD(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FREM01( FM01 , F, NF, NDIRE, NPOIN2)
      IMPLICIT NONE
      INTEGER,          INTENT(IN) ::  NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FM01(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FREM02( FM02, F, NF, NDIRE, NPOIN2)
      IMPLICIT NONE
      INTEGER,          INTENT(IN) ::  NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FM02(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FREMOY( FMOY, F, NF, NDIRE, NPOIN2)
      IMPLICIT NONE
      INTEGER,         INTENT(IN) ::  NF    , NDIRE , NPOIN2
      DOUBLE PRECISION,INTENT(IN) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION,INTENT(INOUT) :: FMOY(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FREPIC( FPIC, F, NF, NDIRE, NPOIN2)
      IMPLICIT NONE
      INTEGER,INTENT(IN)             :: NF    , NDIRE , NPOIN2
      DOUBLE PRECISION,INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION,INTENT(INOUT) :: FPIC(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FRIC3D
     &  (CFWC, NPOIN2, DIRHOU, U_TEL, V_TEL,  UWBM)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN2
      TYPE(BIEF_OBJ),   INTENT(IN) :: U_TEL,V_TEL
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: CFWC
      DOUBLE PRECISION, INTENT(IN) :: UWBM(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: DIRHOU(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
!V6P2 New subroutine
      INTERFACE
        SUBROUTINE FRMSET ( NEIGB , NPOIN2, NELEM2,
     &  IKLE  , RK    , RX    , RY    , RXX   , RYY )
        USE DECLARATIONS_TOMAWAC, ONLY : MAXNSP
        IMPLICIT NONE
!      INTEGER,INTENT(IN)            :: NPOIN2, MAXNSP, NELEM2
      INTEGER,INTENT(IN)            :: NPOIN2, NELEM2
      INTEGER,INTENT(INOUT)         :: NEIGB(NPOIN2,MAXNSP)
      INTEGER,INTENT(IN)            :: IKLE(NELEM2,3)
      DOUBLE PRECISION,INTENT(INOUT):: RK(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(INOUT):: RX(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(INOUT):: RY(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(INOUT):: RXX(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(INOUT):: RYY(MAXNSP,NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FSPRD1
     &( FRA   , NDIRE , SPRED1, TETA1 , SPRED2, TETA2 , XLAMDA)
      IMPLICIT NONE
      INTEGER,INTENT(IN)             :: NDIRE
      DOUBLE PRECISION,INTENT(IN)    :: SPRED1, TETA1 , SPRED2, TETA2
      DOUBLE PRECISION,INTENT(IN)    :: XLAMDA
      DOUBLE PRECISION,INTENT(INOUT) :: FRA(NDIRE)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FSPRD2
     &( FRA   , NDIRE , SPRED1, TETA1 , SPRED2, TETA2 , XLAMDA)
      IMPLICIT NONE
      INTEGER,INTENT(IN)             :: NDIRE
      DOUBLE PRECISION,INTENT(IN)    :: SPRED1, TETA1 , SPRED2, TETA2
      DOUBLE PRECISION,INTENT(IN)    :: XLAMDA
      DOUBLE PRECISION,INTENT(INOUT) :: FRA(NDIRE)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FSPRD3
     &( FRA   , NDIRE , SPRED1, TETA1 , SPRED2, TETA2 , XLAMDA)
      IMPLICIT NONE
      INTEGER,INTENT(IN)             :: NDIRE
      DOUBLE PRECISION,INTENT(IN)    :: SPRED1, TETA1 , SPRED2, TETA2
      DOUBLE PRECISION,INTENT(IN)    :: XLAMDA
      DOUBLE PRECISION,INTENT(INOUT) :: FRA(NDIRE)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        FUNCTION GAMMLN
     &( XX    , DEUPI )
      IMPLICIT NONE
      DOUBLE PRECISION GAMMLN
      DOUBLE PRECISION,INTENT(IN)    :: XX    , DEUPI
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE GAULEG
     &( W_LEG , X_LEG , NPOIN )
      IMPLICIT NONE
      INTEGER ,INTENT(IN)             :: NPOIN
      DOUBLE PRECISION ,INTENT(INOUT) :: W_LEG(NPOIN) , X_LEG(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE IFABTOM(IFABOR,NELEM2,NETAGE)
      IMPLICIT NONE
      INTEGER, INTENT(IN)          :: NELEM2,NETAGE
      INTEGER, INTENT(INOUT)       :: IFABOR(NELEM2,5,NETAGE)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE IMPR(LISPRD,LT,AT,ISITS,ICOD)
      IMPLICIT NONE
      INTEGER,INTENT(IN)           :: LT,ICOD,LISPRD,ISITS
      DOUBLE PRECISION,INTENT(IN)  :: AT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE INIPHY
     &( XK    , CG    , B     ,  NPOIN2, NF )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF    , NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: B(NPOIN2,NF)  , XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CG(NPOIN2,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE INITAB(IBOR1,IFABOR1,NELEM2_DIM,PART)
      IMPLICIT NONE
      INTEGER, INTENT(IN)    :: PART,NELEM2_DIM
      INTEGER, INTENT(IN)    :: IFABOR1(NELEM2_DIM,3)
      INTEGER, INTENT(INOUT) :: IBOR1(NELEM2_DIM,7)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE INTANG
     &( LAVANT, LAPRES, IDIRE , NDIRE , DELTAD)
      IMPLICIT NONE
      INTEGER, INTENT(IN)          :: NDIRE , IDIRE
      DOUBLE PRECISION, INTENT(IN) :: DELTAD
      INTEGER, INTENT(INOUT)       :: LAVANT, LAPRES
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE INVERT( RN    , N     , NP    )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N, NP
      DOUBLE PRECISION, INTENT(INOUT) :: RN(NP,NP)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION KERBOU
     &(XK1,XK2,FREQ1,FREQ2,DEPTH,TETA1,TETA2)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN) :: XK1,XK2,FREQ1,FREQ2,TETA1,TETA2
      DOUBLE PRECISION, INTENT(IN) :: DEPTH
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE KMOYE2
     &( XKMOY , XK    , F     , NF    , NDIRE , NPOIN2, 
     &  AUX1  , AUX2  , AUX3  )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: AUX1(NPOIN2),AUX2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: AUX3(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: XKMOY(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE KMOYEN
     &( XKMOY , XK    , F     , NF    , NDIRE ,
     &  NPOIN2, AUX1  , AUX2  , AUX3  )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: AUX1(NPOIN2),AUX2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: AUX3(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: XKMOY(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECDOI
     &(F1,NAME1,MODE1,F2,NAME2,MODE2, F3,NAME3,MODE3,
     & NPOIN2,NDON,FMTDON,
     & AT,TV1,TV2,F11,F12,F21,F22,F31,F32,INDIC,CHDON,NVAR,
     & TEXTE,TROUVE,UNITIME,PHASTIME)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NDON,NPOIN2,INDIC
      INTEGER, INTENT(IN)             :: MODE1,MODE2,MODE3
      INTEGER, INTENT(INOUT)          :: NVAR
      DOUBLE PRECISION, INTENT(INOUT) :: F1(NPOIN2),F2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F3(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F11(NPOIN2),F12(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F21(NPOIN2),F22(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F31(NPOIN2),F32(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: AT,UNITIME,PHASTIME
      DOUBLE PRECISION, INTENT(INOUT) :: TV1,TV2
      CHARACTER(LEN=8), INTENT(IN)    :: FMTDON
      CHARACTER(LEN=7), INTENT(IN)    :: CHDON
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1,NAME2,NAME3
      CHARACTER(LEN=32),INTENT(INOUT) :: TEXTE(30)
      LOGICAL, INTENT(INOUT)          :: TROUVE(3)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECDON
     &(F1,NAME1,MODE1,F2,NAME2,MODE2, F3,NAME3,MODE3,
     & NPOIN2,NDON,FFORMAT,INDIC,CHDON,TEXTE,TROUVE)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NDON,NPOIN2,INDIC
      INTEGER, INTENT(IN)             :: MODE1,MODE2,MODE3
      DOUBLE PRECISION, INTENT(INOUT) :: F1(NPOIN2),F2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F3(NPOIN2)
      CHARACTER(LEN=8), INTENT(IN)    :: FFORMAT
      CHARACTER(LEN=7), INTENT(IN)    :: CHDON
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1,NAME2,NAME3
      CHARACTER(LEN=32),INTENT(INOUT) :: TEXTE(30)
      LOGICAL, INTENT(INOUT)          :: TROUVE(3)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,
     &                            CAS_FILE,DICO_FILE, PART)
        USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
      INTEGER, INTENT(IN)                    :: PART, NCAR
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECSUI
     &(F,NDIRE,NF,NPOIN2,VENT,COURAN,NPRE,FMTPRE, MAREE,TRA01)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPRE,NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN2*NDIRE)
      LOGICAL, INTENT(IN)             :: COURAN,VENT,MAREE
      CHARACTER(LEN=8), INTENT(IN)    :: FMTPRE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LIMITE( F , FREQ  , NPOIN2, NDIRE , NF    )
        IMPLICIT NONE
        INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
        DOUBLE PRECISION, INTENT(IN)    :: FREQ(NF)
        DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LIMWAC
     &(F     , FBOR  , NPTFR , NDIRE , NF    , NPOIN2,
     & KENT  , PRIVE , NPRIV , IMP_FILE)
      USE BIEF_DEF, ONLY : BIEF_FILE
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPTFR,NDIRE,NF,NPOIN2,NPRIV
      INTEGER, INTENT(IN)             :: KENT
      DOUBLE PRECISION, INTENT(IN):: PRIVE(NPOIN2,NPRIV)
      TYPE(BIEF_FILE), INTENT(IN)    :: IMP_FILE
      DOUBLE PRECISION, INTENT(INOUT):: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: FBOR(NPTFR,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE MARUTI
     &(NMAR,FMTMAR)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NMAR
      CHARACTER(LEN=8), INTENT(IN)    :: FMTMAR
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE MOUDISS
     &  (FWX, FWY, NPOIN2, XK, NDIRE, FS,NF)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE,NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FWX(NPOIN2), FWY(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_TIDE
     &(NMAR,FMTMAR)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NMAR
      CHARACTER(LEN=8), INTENT(IN)    :: FMTMAR
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NOMVAR_TOMAWAC
     &(TEXTE,MNEMO,MAXVAR)
      IMPLICIT NONE
      INTEGER, INTENT(IN)         ::      MAXVAR
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(MAXVAR)
      CHARACTER(LEN=8) , INTENT(INOUT) :: MNEMO(MAXVAR)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NOUDON
     &(F1,NAME1,MODE1,F2,NAME2,MODE2,F3,NAME3,MODE3,
     & NPOIN,NDON,FFORMAT,
     & AT,TV1,TV2,F11,F12,F21,F22,F31,F32,INDIC,CHDON,NVAR,TEXTE,
     & TROUVE,UNITIME,PHASTIME)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NDON,NPOIN,INDIC
      INTEGER, INTENT(INOUT)          :: NVAR
      INTEGER, INTENT(IN)             :: MODE1,MODE2,MODE3
      DOUBLE PRECISION, INTENT(INOUT) :: F1(NPOIN),F2(NPOIN),F3(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: F11(NPOIN),F21(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: F12(NPOIN),F22(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: F31(NPOIN),F32(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: AT,UNITIME,PHASTIME
      DOUBLE PRECISION, INTENT(INOUT) :: TV1,TV2
      CHARACTER(LEN=8), INTENT(IN)    :: FFORMAT
      CHARACTER(LEN=7), INTENT(IN)    :: CHDON
      CHARACTER(LEN=32),INTENT(IN)    :: NAME1,NAME2,NAME3
      CHARACTER(LEN=32),INTENT(IN)    :: TEXTE(30)
      LOGICAL, INTENT(INOUT)          :: TROUVE(3)
!
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PREDIF
     &(CX    , CY    , IKLE2 , IFABOR, ELT   , ETA   ,
     & XK    , CG    , ITR01 , NPOIN3, NPOIN2, NELEM2, NDIRE ,
     & NF    , COURAN, F     , RX    , RY    , RXX   , RYY   , NEIGB )
      USE BIEF_DEF
        USE DECLARATIONS_TOMAWAC, ONLY : MAXNSP
      IMPLICIT NONE

      INTEGER,INTENT(IN)    :: NPOIN3,NPOIN2,NELEM2,NDIRE,NF
      INTEGER,INTENT(IN)    :: IKLE2(NELEM2,3)
      INTEGER,INTENT(IN)    :: NEIGB(NPOIN2,MAXNSP)
      INTEGER,INTENT(INOUT) :: IFABOR(NELEM2,7)
      INTEGER,INTENT(INOUT) :: ELT(NPOIN3,NF), ETA(NPOIN3,NF)
      INTEGER,INTENT(INOUT) :: ITR01(NPOIN3,3)
      DOUBLE PRECISION,INTENT(IN) :: RX(MAXNSP,NPOIN2),RY(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: RXX(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: RYY(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: XK(NPOIN2,NF),CG(NPOIN2,NF)
      DOUBLE PRECISION,INTENT(IN) :: F(NPOIN2,NDIRE,NF)
      LOGICAL,INTENT(IN) :: COURAN
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CX,CY
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PRENL1
     &( IANGNL, COEFNL, NDIRE , NF    , RAISF , XLAMD )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NDIRE , NF
      INTEGER, INTENT(INOUT)          :: IANGNL(NDIRE,8)
      DOUBLE PRECISION, INTENT(IN)    :: RAISF , XLAMD
      DOUBLE PRECISION, INTENT(INOUT) :: COEFNL(16)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
!V6P1 New subroutine
      INTERFACE
        SUBROUTINE PRENL2
     &( IANGNL, COEFNL, NDIRE , NF    , RAISF , XLAMD , XMU   )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NDIRE , NF
      INTEGER, INTENT(INOUT)          :: IANGNL(NDIRE,16)
      DOUBLE PRECISION, INTENT(IN)    :: RAISF , XLAMD , XMU
      DOUBLE PRECISION, INTENT(INOUT) :: COEFNL(32)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
!V6P1 New subroutine
      INTERFACE
        SUBROUTINE PRENL3
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PREPRO
     &( CX    , CY    , IKLE2 , IFABOR, ELT   , ETA   , FRE   ,
     &  XK    , CG    , ITR01 , NPOIN3, NPOIN2, NELEM2, NDIRE ,
     &  NF    , COURAN)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)    :: NPOIN3,NPOIN2,NELEM2,NDIRE,NF
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF),CG(NPOIN2,NF)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN3,NF),ETA(NPOIN3,NF), FRE(*)
      INTEGER, INTENT(IN)    :: IKLE2(NELEM2,3)
      INTEGER, INTENT(INOUT) :: ITR01(NPOIN3,3),IFABOR(NELEM2,7)
      LOGICAL, INTENT(IN)    :: COURAN
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CX,CY
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PREQT2
        IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PROPA
     &(F, B, ELT, ETA, FRE, NPOIN3, NPOIN2,
     & NDIRE, NF, COURAN, TRA01)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPOIN3,NPOIN2,NDIRE,NF
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: B(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3,NF)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN3,NF),ETA(NPOIN3,NF)
      INTEGER, INTENT(INOUT) :: FRE(*)
      LOGICAL, INTENT(IN)    :: COURAN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        FUNCTION QBBJ78( B     , IQBBJ )
      IMPLICIT NONE
      DOUBLE PRECISION QBBJ78
      DOUBLE PRECISION, INTENT(IN) ::  B
      INTEGER, INTENT(IN)     ::  IQBBJ
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QBREK1
     &( TSTOT , F     , FCAR  , VARIAN, NF    , NDIRE , NPOIN2)
      IMPLICIT NONE
      INTEGER, INTENT(IN)            :: NF, NDIRE, NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)   :: VARIAN(NPOIN2),FCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QBREK2
     &( TSTOT , F     , FCAR  , VARIAN, NF    , NDIRE , NPOIN2)
      IMPLICIT NONE
      INTEGER, INTENT(IN)            :: NF, NDIRE, NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)   :: VARIAN(NPOIN2), FCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QBREK3
     & ( TSTOT , F     , FCAR  , VARIAN, NF    , NDIRE , NPOIN2)
      IMPLICIT NONE
      INTEGER, INTENT(IN)   ::  NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)   :: VARIAN(NPOIN2), FCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QBREK4
     & ( TSTOT , F     , FCAR  , VARIAN, NF    , NDIRE , NPOIN2)
      IMPLICIT NONE
      INTEGER, INTENT(IN)   ::          NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)   :: VARIAN(NPOIN2),FCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QDSCUR
     &( TSTOT , TSDER , F     , CF    , XK    , USOLD , USNEW ,
     &  NF    , NDIRE , NPOIN2, F_INT , BETOTO, BETOTN)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: USNEW(NPOIN2),USOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: F_INT(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: BETOTO(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: BETOTN(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QFROT1
     &( TSTOT , TSDER , F     , XK    ,  NF    , NDIRE , NPOIN2)
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  NF , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        FUNCTION QGAUSS
     &( B     , N     , A     , XM    )
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  N
      DOUBLE PRECISION QGAUSS
      DOUBLE PRECISION, INTENT(IN)    :: B , A , XM
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QMOUT1
     &( TSTOT , TSDER , F     , XK    , ENRJ  , FMOY  , XKMOY ,
     &  NF  , NDIRE , NPOIN2, TAUX1   )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: XKMOY(NPOIN2),ENRJ(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX1(NPOIN2),FMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QMOUT2
     &( TSTOT , TSDER , F     , XK    , ENRJ  , FMOY  , XKMOY , USOLD ,
     &  USNEW , NF    , NDIRE , NPOIN2, TAUX1 ,F_INT  , BETOTO, BETOTN)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: USNEW(NPOIN2),USOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: FMOY(NPOIN2),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: ENRJ(NPOIN2),XKMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F_INT(NPOIN2),TAUX1(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: BETOTO(NPOIN2),BETOTN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QNLIN1
     &( TSTOT , TSDER , IANGNL, NF    , NDIRE , NPOIN2, F     ,
     &  XKMOY , TAUX1 , TAUX2 , TAUX3 , TAUX4 , TAUX5 , DFINI )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPOIN2,NDIRE,NF
      INTEGER, INTENT(IN)             :: IANGNL(NDIRE,8)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XKMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX1(NPOIN2),TAUX2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX3(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX4(NPOIN2),TAUX5(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DFINI(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
!V6P1 New subroutine
      INTERFACE
        SUBROUTINE QNLIN2
     &( TSTOT , TSDER , IANGNL,  COEFNL, NF    , NDIRE , NPOIN2, F     ,
     &  XKMOY , TAUX1 , DFINI , XCOEF )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPOIN2, NDIRE , NF
      INTEGER, INTENT(IN)             :: IANGNL(NDIRE,16)
      DOUBLE PRECISION, INTENT(IN)    :: XCOEF, COEFNL(32)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XKMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX1(NPOIN2), DFINI(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
!V6P1 New subroutine
      INTERFACE
        SUBROUTINE QNLIN3(F1, T1TOT, T1DER, N1POIN2, N1PLAN, N1F,
     &       FSEUIL)
      IMPLICIT NONE
      INTEGER, INTENT(IN)    :: N1POIN2,N1PLAN,N1F
      DOUBLE PRECISION, INTENT(INOUT) :: T1TOT( N1POIN2,N1PLAN,N1F)
      DOUBLE PRECISION, INTENT(INOUT) :: T1DER( N1POIN2,N1PLAN,N1F)
      DOUBLE PRECISION, INTENT(INOUT) :: F1(N1POIN2,N1PLAN,N1F)
      DOUBLE PRECISION, INTENT(INOUT)    :: FSEUIL(N1POIN2)
        END SUBROUTINE
      END INTERFACE
      INTERFACE
!
!-----------------------------------------------------------------------
!
        SUBROUTINE QPOROS
     &( TSTOT , TSDER , F , CG, LT,  XK,
     &  NF    , NDIRE  , NPOIN2   , AMORP  )
!
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2,LT
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CG(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: AMORP(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QTRIA1
     &( F     , XK    , NF    , NDIRE , NPOIN2, TSTOT , FTOT  , FMOY  )
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  NF, NDIRE, NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: FTOT(NPOIN2) , FMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QTRIA2
     &( F     , XK    , NF    , NDIRE , NPOIN2, TSTOT )
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  NF, NDIRE, NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!=======================================================================
!VBA AJOUT SUBROUTINE QVEG1 12/09/2014
!=======================================================================
!
      INTERFACE
        SUBROUTINE QVEG
     &( TSTOT , TSDER , F , VARIAN , FMOY , XKMOY , NF    , NDIRE  ,
     &  NPOIN2   )
!
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: XKMOY(NPOIN2),VARIAN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF),FMOY(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!=========================================================================
!VBA FIN AJOUT SUBROUTINE QVEG1
!=========================================================================
!
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QWIND1
     &( TSTOT , TSDER , F     , XK    , USOLD , USNEW , TWOLD , TWNEW ,
     &  Z0OLD , Z0NEW , NF    , NDIRE , NPOIN2, TOLD  , TNEW  ,
     &  USN   , USO   , OMNEW , OMOLD , BETAN , BETAO )
      IMPLICIT NONE
      INTEGER, INTENT(IN)   ::  NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)   :: TWOLD(NPOIN2), TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: USOLD(NPOIN2), USNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: Z0OLD(NPOIN2), Z0NEW(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TNEW(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT):: TOLD(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT):: USO(NPOIN2),U SN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: OMNEW(NPOIN2),OMOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: BETAN(NPOIN2), BETAO(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: TSDER(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QWIND2
     &( TSTOT , TSDER , F     , XK    , USOLD , USNEW , TWOLD , TWNEW ,
     &  NF    , NDIRE , NPOIN2, USN   , USO   )
      IMPLICIT NONE
      INTEGER, INTENT(IN)            :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: TWOLD(NPOIN2),TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: USNEW(NPOIN2),USOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: USO(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT):: USN(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF),XK(NPOIN2,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QWIND3
     &( TSTOT , TSDER , F     , XK    , USOLD , USNEW , TWOLD , TWNEW ,
     &  NF    , NDIRE , NPOIN2, BETAN , BETAO , DIRN  , DIRO  )
      IMPLICIT NONE
      INTEGER, INTENT(IN)   ::    NF  , NDIRE        , NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: TWOLD(NPOIN2), TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: USOLD(NPOIN2), USNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT):: DIRO(NPOIN2) , DIRN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: BETAN(NPOIN2), BETAO(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: TSDER(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE QWINDL
     &( TSTOT , USOLD , USNEW , TWOLD , TWNEW , NF    , NDIRE ,
     &  NPOIN2, USN   , USO   , FPMO  , FPMN )
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: FPMO(NPOIN2),FPMN(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TWOLD(NPOIN2),TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: USNEW(NPOIN2),USOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: USO(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: USN(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RADIAT
     &        ( FX    , FY    ,XK1   , FS    , CG1   ,
     &        CGSUC1, DSXXDX, DSXYDX, DSXYDY, DSYYDY )
      USE BIEF_DEF
      USE DECLARATIONS_TOMAWAC, ONLY : NDIRE,NF,NPOIN2
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN)    :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: CG1(NPOIN2,NF),XK1(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CGSUC1(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FX(NPOIN2),FY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DSXXDX(NPOIN2),DSXYDX(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DSXYDY(NPOIN2),DSYYDY(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE READ_POLY(WACF01,NPOLY,NSOM,XSOM,YSOM)
      USE BIEF_DEF, ONLY : BIEF_FILE
      IMPLICIT NONE
      TYPE(BIEF_FILE), TARGET,INTENT(IN) ::WACF01
      INTEGER, INTENT(INOUT)                         :: NPOLY
      INTEGER, ALLOCATABLE, INTENT(OUT) :: NSOM(:)
      DOUBLE PRECISION, ALLOCATABLE, INTENT(OUT)::XSOM(:,:),YSOM(:,:)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE READ_SPECTRA_COORDS
     &(FID,NP,XP,YP)
      IMPLICIT NONE
      INTEGER, INTENT(IN)                         :: FID
      INTEGER, INTENT(INOUT)                         :: NP
      DOUBLE PRECISION,ALLOCATABLE, INTENT(INOUT) :: XP(:),YP(:)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE REFLECT
     &(NDIRE, NF, NPTFR, NPOIN2, FBOR, F, KREFL)
      IMPLICIT NONE
      INTEGER, INTENT(IN)            :: NPTFR,NDIRE,NF, NPOIN2, KREFL
      DOUBLE PRECISION, INTENT(INOUT):: FBOR(NPTFR,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NDIRE,NF)
 
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RPI_INTR
     &(NEIGB  , NB_CLOSE, RX  , RY      , RXX     , RYY ,
     & NPOIN2 , I       , MAXNSP  , FFD , FIRDIV1 , FIRDIV2 ,
     & SECDIV1, SECDIV2 , SECDIV3, FRSTDIV , SCNDDIV)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN2, MAXNSP,I
      INTEGER, INTENT(IN) :: NEIGB(NPOIN2,MAXNSP),NB_CLOSE(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: RX(MAXNSP),RY(MAXNSP)
      DOUBLE PRECISION, INTENT(IN)    :: RXX(MAXNSP),RYY(MAXNSP)
      DOUBLE PRECISION, INTENT(INOUT) :: SECDIV1,SECDIV2,SECDIV3
      DOUBLE PRECISION, INTENT(INOUT) :: FIRDIV1,FIRDIV2
      DOUBLE PRECISION, INTENT(IN)    :: FFD(NPOIN2)
      LOGICAL, INTENT(IN)             :: FRSTDIV,SCNDDIV
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
!V6P2 New subroutine
      INTERFACE
        SUBROUTINE RPI_INVR
     &( X     , Y     , NEIGB , NB_CLOSE, RK_D , RX_D , RY_D  , RXX_D ,
     &  RYY_D , NPOIN2, I     , QUO   , AC    , MAXNSP, MINDIST )
      IMPLICIT NONE
      INTEGER, INTENT(IN)    :: NPOIN2, MAXNSP, I
      INTEGER, INTENT(IN)    :: NEIGB(NPOIN2,MAXNSP), NB_CLOSE(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: QUO, AC
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2), Y(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: MINDIST(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: RK_D(MAXNSP)
      DOUBLE PRECISION, INTENT(INOUT) :: RX_D(MAXNSP), RY_D(MAXNSP)
      DOUBLE PRECISION, INTENT(INOUT) :: RXX_D(MAXNSP), RYY_D(MAXNSP)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SEMIMP
     &( F     ,   CF , XK   ,  NF  , NDIRE, NPOIN2,
     &  IANGNL, TSTOT, TSDER, TOLD,  TNEW,  Z0NEW, TWNEW, 
     &  TAUX1,  TAUX2, TAUX3, TAUX4, TAUX5, TAUX6, TAUX7,
     &  MDIA, IANMDI, COEMDI, FBOR, PART)
        USE DECLARATIONS_SPECIAL
        USE DECLARATIONS_TOMAWAC, ONLY : NPTFR
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN2,NDIRE,NF, IANGNL(*), PART
      DOUBLE PRECISION, INTENT(INOUT) :: Z0NEW(NPOIN2), TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX1(NPOIN2),
     &                                   TAUX2(NPOIN2),TAUX3(NPOIN2),
     &                                   TAUX4(NPOIN2),TAUX5(NPOIN2),
     &                                   TAUX6(NPOIN2),TAUX7(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TOLD(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: TNEW(NPOIN2,NDIRE)
      DOUBLE PRECISION, INTENT(INOUT) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN) :: CF(*)
!....MDIA method declarations
      INTEGER, INTENT(IN) :: MDIA, IANMDI(*)
      DOUBLE PRECISION, INTENT(IN) ::  COEMDI(*)
      DOUBLE PRECISION, INTENT(IN)   :: FBOR(NPTFR,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SOR3D
     &(F,NDIRE,NF,NPOIN2,VENT, COURAN,MAREE,TITRE,TRA01,MESH3D)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN2*NDIRE)
      LOGICAL, INTENT(IN)             :: COURAN,VENT,MAREE
      CHARACTER(LEN=80), INTENT(IN)   :: TITRE
      TYPE(BIEF_MESH), INTENT(IN)     :: MESH3D
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SPEINI
     &( F     , SPEC  , FRA   , UV    , VV    ,
     &  FREMAX, FETCH , SIGMAA, SIGMAB, GAMMA , FPIC  , HM0I  , ALPHIL,
     &  TETA1 , SPRED1, TETA2 , SPRED2, XLAMDA, NPOIN2, NDIRE , NF    ,
     &  INISPE, DEPTH , FRABI )
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  NPOIN2, NDIRE , NF    , INISPE, FRABI
      DOUBLE PRECISION, INTENT(IN)    :: FREMAX, FETCH , SIGMAA
      DOUBLE PRECISION, INTENT(IN)    :: SIGMAB, GAMMA
      DOUBLE PRECISION, INTENT(IN)    :: FPIC  , HM0I  , ALPHIL, TETA1
      DOUBLE PRECISION, INTENT(IN)    :: SPRED1, TETA2
      DOUBLE PRECISION, INTENT(IN)    :: SPRED2, XLAMDA
      DOUBLE PRECISION, INTENT(IN)    :: UV(*) , VV(*)
      DOUBLE PRECISION, INTENT(IN)    :: DEPTH(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FRA(NDIRE), SPEC(NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SPEJON
     &( SPEC  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  FPMIN )
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  NF
      DOUBLE PRECISION, INTENT(IN)    :: SIGMAA, SIGMAB, GAMMA, FPMIN
      DOUBLE PRECISION, INTENT(IN)    :: FP    , AL
      DOUBLE PRECISION, INTENT(INOUT) :: SPEC(NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SPETMA
     &( SPEC  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  FPMIN , DEPTH  )
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  NF
      DOUBLE PRECISION, INTENT(IN)    :: SIGMAA, SIGMAB, GAMMA, FPMIN
      DOUBLE PRECISION, INTENT(IN)    :: FP, AL, DEPTH
      DOUBLE PRECISION, INTENT(INOUT) :: SPEC(NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DIR_SPREAD( DIRSPR, F, NDIRE, NF, NPOIN2)
      IMPLICIT NONE
      INTEGER, INTENT(IN)    :: NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: DIRSPR(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE STRESS
     &( TAUWAV, TSTOT , F     , USNEW , TWNEW , Z0NEW ,
     &  NPOIN2, NDIRE , NF    , XTAUW , YTAUW , TAUHF )
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  NPOIN2, NDIRE , NF
      DOUBLE PRECISION, INTENT(IN)    :: USNEW(NPOIN2), TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: Z0NEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TSTOT(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUWAV(NPOIN2), TAUHF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: XTAUW(NPOIN2) , YTAUW(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE TAUTOT
     &( TAUT  , UVENT , TAUW  , SEUIL ,
     &  ITR   , ITRMIN, ITRMAX)
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  ITRMIN, ITRMAX
      INTEGER, INTENT(INOUT) ::  ITR
      DOUBLE PRECISION, INTENT(IN)    :: UVENT , TAUW
      DOUBLE PRECISION, INTENT(IN)    :: SEUIL
      DOUBLE PRECISION, INTENT(INOUT) :: TAUT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE TETMOY
     &( TETAM , F     ,NDIRE , NF    , NPOIN2)
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TETAM(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE TOM_CORFON
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE TOTNRJ( VARIAN, F, NF, NDIRE, NPOIN2)
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::          NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: VARIAN(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE TRANSF
     &( FA    , FR    , XK    , KNEW  , NEWF  , NEWF1 , TAUX1 , TAUX2 ,
     &  NPOIN2, NDIRE , NF    )
      IMPLICIT NONE
      INTEGER, INTENT(IN)    :: NPOIN2, NDIRE, NF
      INTEGER, INTENT(INOUT) :: KNEW(NPOIN2),NEWF(NPOIN2), NEWF1(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: FR(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUX1(NPOIN2),TAUX2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FA(NPOIN2,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USTAR1
     &( USTAR , Z0    , TAUWAV,  NPOIN2)
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: TAUWAV(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: USTAR(NPOIN2), Z0(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USTAR2
     &( USTAR , NPOIN2)
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::  NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: USTAR(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE UVSTOKES
     &       (UST, VST, WST, FS, NPOIN2, XK1, ZFJ, NDIRE, ZTEL,NZ,NF)
      IMPLICIT NONE
      INTEGER, INTENT(IN)  :: NZ,NF
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2, NDIRE, NF)
      DOUBLE PRECISION, INTENT(IN) :: ZFJ(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: XK1(NPOIN2, NF)
      DOUBLE PRECISION, INTENT(INOUT) :: UST(NPOIN2, NZ)
      DOUBLE PRECISION, INTENT(INOUT) :: VST(NPOIN2, NZ)
      DOUBLE PRECISION, INTENT(INOUT) :: WST(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: ZTEL(NPOIN2, NZ)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE VENUTI
     &(NVEN,FMTVEN)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NVEN
      CHARACTER(LEN=8), INTENT(IN)    :: FMTVEN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_WIND
     &(NVEN,FMTVEN)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NVEN
      CHARACTER(LEN=8), INTENT(IN)    :: FMTVEN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE VITFON(VIFOND,F,XK,NF,NPOIN2,NDIRE)
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NF,NDIRE,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: VIFOND(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE WAC
     &(PART, NIT_ORI)
      IMPLICIT NONE
      INTEGER,           INTENT(IN)      :: PART
      INTEGER, OPTIONAL, INTENT(IN)      :: NIT_ORI
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE WINDISS1
     &  (FWX, FWY, NPOIN2, XK, NDIRE, FS,NF)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE,NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FWX(NPOIN2), FWY(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE WINDISS2
     &  (FWX, FWY, NPOIN2, XK, NDIRE, FS,NF)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE,NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FWX(NPOIN2), FWY(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE WINDISS3
     &  (FWX, FWY, NPOIN2, XK, NDIRE, FS,NF)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN2, NDIRE,NF
      DOUBLE PRECISION, INTENT(IN) :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN) :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FWX(NPOIN2), FWY(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE WAC_INIT
     & (PART,IMPRES, DEBRES,DATE,TIME)
      IMPLICIT NONE
      LOGICAL, INTENT(INOUT) :: IMPRES, DEBRES
      INTEGER, INTENT(IN) :: PART
      INTEGER, INTENT(INOUT) :: DATE(3),TIME(3)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE WIPJ
     & (  WIP, FS, NPOIN2, XK, WIPDX, WIPDY,NDIRE,NF)
      INTEGER, INTENT(IN)    :: NPOIN2,NDIRE,NF
      DOUBLE PRECISION, INTENT(IN)    :: FS(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: WIP(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: WIPDX(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: WIPDY(NPOIN2)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE WNSCOU
     &( CK2   , FREQ  , DEPTH )
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN)    :: FREQ  , DEPTH
      DOUBLE PRECISION, INTENT(INOUT) :: CK2
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE WPOWER
     &( F     , CG    , NF    , NDIRE , NPOIN2)
      IMPLICIT NONE
      INTEGER, INTENT(IN)    ::          NF    , NDIRE , NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(IN)    :: CG(NPOIN2,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_ANACOS
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_ANAMAR
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_ANAVEN
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_DUMP2D
     &(XF1, NP1)
      IMPLICIT NONE
      INTEGER, INTENT(IN)          :: NP1
      DOUBLE PRECISION, INTENT(IN) :: XF1(NP1)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_LIMWAC
     &(F     , FBOR  , NPTFR , NDIRE , NF    , NPOIN2,
     & KENT  , PRIVE , NPRIV , IMP_FILE)
      USE BIEF_DEF, ONLY : BIEF_FILE
      IMPLICIT NONE
      INTEGER, INTENT(IN)            :: NPTFR,NDIRE,NF,NPOIN2,NPRIV
      INTEGER, INTENT(IN)            :: KENT
      DOUBLE PRECISION, INTENT(IN)   :: PRIVE(NPOIN2,NPRIV)
      TYPE(BIEF_FILE), INTENT(IN)    :: IMP_FILE
      DOUBLE PRECISION, INTENT(INOUT):: F(NPOIN2,NDIRE,NF)
      DOUBLE PRECISION, INTENT(INOUT):: FBOR(NPTFR,NDIRE,NF)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_TOM_CORFON
          IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!=======================================================================
!
      END MODULE INTERFACE_TOMAWAC
