!                   ***********************
                    MODULE INTERFACE_WAQTEL
!                   ***********************
!
!
!***********************************************************************
! WAQTEL      V8P4
!***********************************************************************
!
!
!-----------------------------------------------------------------------
!
!     DEFINITION OF INTERFACES
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ALGAE_DEATH
     &(ALD,MP,CMOR,TR,TRESP,GT,TOX,NPOIN)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER         , INTENT(IN):: NPOIN
      DOUBLE PRECISION, INTENT(IN):: CMOR(2),TR(NPOIN),TOX,TRESP
      DOUBLE PRECISION, INTENT(INOUT)::ALD(NPOIN),MP(NPOIN)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) ::GT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ALGAE_GROWTH
     &(ALG,CMAX,RAY,GT,NUTR,TOX,NPOIN )
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER         , INTENT(IN):: NPOIN
      DOUBLE PRECISION, INTENT(IN):: CMAX,RAY(NPOIN),NUTR(NPOIN),TOX
      DOUBLE PRECISION, INTENT(INOUT)::ALG(NPOIN)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) ::GT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECWAQPAR
     &(IFIC,NPOIN,NBRECH,OPTNBR,TDECBR,DURBR,ZFINBR,NUMPSD,MESH,
     & ZDECBR,NBNDBR,INDBR,ZCRBR)
        USE BIEF_DEF
        IMPLICIT NONE
        INTEGER          , INTENT(IN)    :: IFIC
        INTEGER          , INTENT(IN)    :: NPOIN
        INTEGER          , INTENT(INOUT)    :: NBRECH
        TYPE(BIEF_OBJ), INTENT(INOUT) :: OPTNBR,TDECBR,DURBR,ZFINBR
        TYPE(BIEF_OBJ), INTENT(INOUT) :: ZDECBR
        TYPE(BIEF_OBJ), INTENT(INOUT) :: NUMPSD,NBNDBR,INDBR,ZCRBR
        TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS2D_BIOMASS
     &  (NPOIN,WATTEMP,TN,TEXP,RAYEFF,HPROP,T1,T2,T3,T4,T5,T6,DEBUG)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN,DEBUG
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF,T1,T2,T3,T4,T5,T6
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_BIOMASS
     &  (NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,RAYEFF,ZPROP,
     &   T1,T2,T3,T4,T5,T6)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN2,NPOIN3
      INTEGER          , INTENT(IN   ) :: NPLAN
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,ZPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
!
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS2D_EUTRO
     &  (NPOIN,WATTEMP,TN,TEXP,TIMP,RAYEFF,HPROP,T1,T2,T3,T4,
     &   T5,T6,T7,T8,T9,T10,T11,T12,DEBUG,UN,VN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN,DEBUG
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TIMP,TEXP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8,T9,T10
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T11,T12
!
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_EUTRO
     &  (NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,TIMP,RAYEFF,HPROP,
     &   ZPROP,T1,T21,T22,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,DEBUG,UN,VN,
     &   DT)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN3,NPOIN2,NPLAN
      INTEGER          , INTENT(IN   ) :: DEBUG
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP,DT
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,ZPROP,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T21,T3,T4,T5,T6,T7,T8,T9
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T10,T11,T12,T22
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS2D_MICROPOL (NPOIN,TN,TEXP,TIMP,
     &                             HPROP,CF,UN,VN,T1,T2,T3,T4,T5,T6)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,T1,T2,T3,T4,T5,T6,TIMP
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_MICROPOLV (NPOIN2,TN,TEXP,
     &        TIMP,ZPROP,CF,UN,VN,T2_1,T2_2,T2_3,T3_1,T3_2,T3_3)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN2
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,ZPROP,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T2_1,T2_2,T2_3,T3_1,T3_2,T3_3
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_MICROPOLS (NPOIN2,BTABOF,TN,CF,UN,VN,T2_1)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN2
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: BTABOF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T2_1
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS2D_O2
     & (NPOIN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,TIMP,T2,T3,T4,HPROP,UN,VN,DEBUG)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: FORMK2,NPOIN,DEBUG
      DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP
      DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44
      DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,T2,T3,T4
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_O2
     & (NPOIN3,NPOIN2,NPLAN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,TIMP,T31,T32,T21,HPROP,ZPROP,UN,VN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN2,NPOIN3,NPLAN
      INTEGER          , INTENT(IN   ) :: FORMK2
      DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP
      DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44
      DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,UN,VN,ZPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,T31,T32,T21
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS2D_THERMIC
     & (NPOIN,TN,TEXP,HPROP,PATMOS)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPOIN
      TYPE(BIEF_OBJ), INTENT(IN)      :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TEXP
      TYPE(BIEF_OBJ), INTENT(IN)      :: HPROP
      TYPE(BIEF_OBJ), INTENT(IN)      :: PATMOS
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_THERMICS
     & (NPOIN2,NPOIN3,TA,ATABOS,BTABOS,PATMOS,ATMOSEXCH,
     &  WINDX,WINDY,RHO)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3
      INTEGER, INTENT(IN)             :: ATMOSEXCH
      TYPE(BIEF_OBJ), INTENT(IN)      :: TA,WINDX,WINDY
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: ATABOS,BTABOS
      TYPE(BIEF_OBJ), INTENT(IN)      :: PATMOS,RHO
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_THERMICV(NPOIN2,NPOIN3,NPLAN,Z,RHO,TA,TEXP,
     &                              LONGIT,LATIT,AT,MARDAT,MARTIM)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,NPLAN
      INTEGER, INTENT(IN)             :: MARDAT(3),MARTIM(3)
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN3),LATIT,LONGIT,AT
      DOUBLE PRECISION, INTENT(IN)    :: RHO(NPOIN3)
      TYPE(BIEF_OBJ), INTENT(IN)      :: TA
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TEXP
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DEPOS_FX
     &  (SEDP,TAUB,CSUS,TAUS,VITCHU,NPOIN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: TAUS,VITCHU
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TAUB,CSUS
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: SEDP
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE EROSION_FX
     & (SEDERO,TAUB,SF,TAUR,ERO,ZZERO,NPOIN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: TAUR,ERO,ZZERO
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TAUB,SF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: SEDERO
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECDON_WAQTEL
     & (FILE_DESC,PATH,NCAR,CAS_FILE,DICO_FILE)
        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
        CHARACTER(LEN=PATH_LEN), INTENT(INOUT) ::
     & FILE_DESC(4,MAXKEYWORD)
        INTEGER, INTENT(IN)               :: NCAR
        CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
        CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
        CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NAMETRAC_WAQTEL
     &  (NAMETRAC,NTRAC,MAXTRA,PROCESS)
        IMPLICIT NONE
      INTEGER          , INTENT(IN   )::  PROCESS,MAXTRA
      INTEGER          , INTENT(INOUT)::  NTRAC
      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NUTEFF
     &(LNUT,TRR,NPOIN,IPO4,INO3,KP,KN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)    :: NPOIN,IPO4,INO3
      DOUBLE PRECISION, INTENT(IN)    :: KN,KP
      DOUBLE PRECISION, INTENT(INOUT) :: LNUT(NPOIN)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: TRR
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE POINT_WAQTEL
     &  (MESH,IELM1,MESH3D,IELM3)
        USE BIEF_DEF
        IMPLICIT NONE
!
        INTEGER,         INTENT(IN   ) :: IELM1
        TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
        TYPE(BIEF_MESH), INTENT(INOUT),OPTIONAL :: MESH3D
        INTEGER,         INTENT(IN   ),OPTIONAL :: IELM3
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RAY_EFFECT
     &(SECCHI,TRR,NPOIN,MEXT,I0,IK,KPE,EFF,H,T1,T2)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)    :: NPOIN,MEXT
      DOUBLE PRECISION, INTENT(IN)    :: I0,IK,SECCHI,KPE
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: H,TRR
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: EFF,T1,T2
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RAY_EFFECT_3D
     &(SECCHI,TRR,NPOIN2,NPLAN,MEXT,I0,IK,KPE,EFF,ZPROP,T1,T2,T3)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)    :: NPOIN2,NPLAN,MEXT
      DOUBLE PRECISION, INTENT(IN)    :: KPE,I0,IK,SECCHI,ZPROP(*)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: TRR
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: EFF,T1,T2,T3
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE REAER
     &(FORMK2,K2,K22,NPOIN,NPLAN,UN,VN,H,EPS)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: FORMK2,NPOIN,NPLAN
      DOUBLE PRECISION, INTENT(IN)     :: EPS,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: H,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: K2
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE REAER_WEIR
     &(FORMRS,H1,H2,ABRS,WATTEMP,EPS,O2SATU,TRUP,TN,IND_O2,IR)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: FORMRS,IR,IND_O2
      DOUBLE PRECISION, INTENT(IN)     :: EPS,H1,H2,ABRS(2),WATTEMP
      DOUBLE PRECISION, INTENT(IN)     :: TRUP,O2SATU
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SATUR_O2
     &(SATO2,FORMCS,WATTEMP,EPS)
        IMPLICIT NONE
      INTEGER         , INTENT(IN)    :: FORMCS
      DOUBLE PRECISION, INTENT(IN)    :: WATTEMP,EPS
      DOUBLE PRECISION, INTENT(INOUT) :: SATO2
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SOURCE_WAQ
     &(NPOIN3,NPOIN2,TEXP,TIMP,TN,HPROP,U,V,CF,
     & T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T21,T22,T23,
     & PATMOS,DIMM,NPLAN,
     & LATIT,LONGIT,AT,MARDAT,MARTIM,ZPROP,DT,RHO)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN)    :: NPOIN3,DIMM
      INTEGER          , INTENT(IN)    :: NPLAN,NPOIN2
      INTEGER          , INTENT(IN)    :: MARDAT(3),MARTIM(3)
      DOUBLE PRECISION, INTENT(IN )    :: LATIT,LONGIT,AT,DT
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: TN,HPROP,U,V,CF,PATMOS
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: ZPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,T1,T2,T3,T4
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T5,T6,T7,T8,T9,T10,T11,T12
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T21,T22,T23
      TYPE(BIEF_OBJ), INTENT(IN),OPTIONAL :: RHO
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE TAUB_WAQTEL
     &(CF,DENSITY,TAUB,NPOIN,UN,VN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: DENSITY
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TAUB
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE YASMI_WAQ
     &  (YASMI)
        IMPLICIT NONE
      LOGICAL          , INTENT(INOUT)::  YASMI(*)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      END MODULE INTERFACE_WAQTEL
