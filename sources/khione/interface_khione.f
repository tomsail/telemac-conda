!                   ***********************
                    MODULE INTERFACE_KHIONE
!                   ***********************
!
!
!***********************************************************************
! KHIONE
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
        SUBROUTINE BORDER_ICOVER
     &(U,V,MESH)
      USE BIEF_DEF
      IMPLICIT NONE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U,V
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE BORICE
     &(H,U,V,F,AT,LT,DT,TRA05,TRA06,LIUBOR,NPTFR,NUMLIQ,KLOG,MASK,MESH,
     & S)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: KLOG, NPTFR
      INTEGER,          INTENT(IN)    :: NUMLIQ(NPTFR)
      INTEGER,          INTENT(IN)    :: LIUBOR(NPTFR)
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: H,U,V,F,S
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: MASK, TRA05,TRA06
      INTEGER,          INTENT(IN)    :: LT
      DOUBLE PRECISION, INTENT(IN)    :: AT,DT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CONDICE
     &( NPOIN,RECORD,AT,LISTIN )
      USE BIEF_DEF
      IMPLICIT NONE
      LOGICAL         , INTENT(IN)    :: LISTIN
      INTEGER         , INTENT(IN)    :: NPOIN
      INTEGER         , INTENT(INOUT) :: RECORD
      DOUBLE PRECISION, INTENT(INOUT) :: AT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FRAZIL_MASS_ON_BAR
     &    (RFR0,RFR1,DB,BAR,NBAR,ANG1,FM1,FMT)
      IMPLICIT NONE
      INTEGER         , INTENT(IN)    :: NBAR
      DOUBLE PRECISION, INTENT(IN)    :: RFR0,RFR1,BAR,ANG1,DB
      DOUBLE PRECISION, INTENT(INOUT) :: FM1,FMT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FRICTION_KHIONE
     &( NPOIN, KFROT, GRAV,KARMAN, CHESTR, CF, H,U,V )
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: NPOIN, KFROT
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, KARMAN
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CHESTR,H,U,V
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CF
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECDON_KHIONE
     & (FILE_DESC,PATH,NCAR)
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
      INTEGER, INTENT(IN)                    :: NCAR
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NAMETRAC_KHIONE
     &  (NAMETRAC,NTRAC,MAXTRA)
      IMPLICIT NONE
      INTEGER          , INTENT(INOUT)::  NTRAC
      INTEGER,           INTENT(IN)   ::  MAXTRA
      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NOMVAR_KHIONE
     &(TEXTE,TEXTPR,MNEMO,NADVAR,NAMES_ADVAR)
      IMPLICIT NONE
      INTEGER, INTENT(IN)              :: NADVAR
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8),  INTENT(INOUT) :: MNEMO(*)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMES_ADVAR(*)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NOMVAR3D_KHIONE
     &(TEXTE,TEXTPR,MNEMO)
      IMPLICIT NONE
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8),  INTENT(INOUT) :: MNEMO(*)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE OUTPUT_KHIONE
     &(TITLE,NPOIN,AT,LT,MESH,TELSOR,DATE,MARTIM,ISHEAD,ISMESH,ISVARS,
     & TN,NPOIN3,NPLAN)
      USE BIEF_DEF
      IMPLICIT NONE
      CHARACTER(LEN=72),     INTENT(IN) :: TITLE
      TYPE(BIEF_MESH),       INTENT(IN) :: MESH
      TYPE(BIEF_OBJ),        INTENT(IN) :: TELSOR,TN
      INTEGER, DIMENSION(3), INTENT(IN) :: DATE
      INTEGER, DIMENSION(3), INTENT(IN) :: MARTIM
      DOUBLE PRECISION,      INTENT(IN) :: AT
      INTEGER,               INTENT(IN) :: LT,NPOIN
      LOGICAL,               INTENT(IN) :: ISHEAD,ISMESH,ISVARS
      INTEGER, OPTIONAL,     INTENT(IN) :: NPOIN3,NPLAN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE OUTPUT3D_KHIONE
     &(TITLE,AT,LT,MESH,DATE,MARTIM,ISHEAD,ISMESH,ISVARS,
     & TN,NPOIN3,NPLAN)
      USE BIEF_DEF
      IMPLICIT NONE
      CHARACTER(LEN=72),     INTENT(IN) :: TITLE
      TYPE(BIEF_MESH),       INTENT(IN) :: MESH
      TYPE(BIEF_OBJ),        INTENT(IN) :: TN
      INTEGER, DIMENSION(3), INTENT(IN) :: DATE
      INTEGER, DIMENSION(3), INTENT(IN) :: MARTIM
      DOUBLE PRECISION,      INTENT(IN) :: AT
      INTEGER,               INTENT(IN) :: LT
      LOGICAL,               INTENT(IN) :: ISHEAD,ISMESH,ISVARS
      INTEGER,               INTENT(IN) :: NPOIN3,NPLAN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE POINT_KHIONE
     &( MESH,IELMX, MESH3D,IELM3 )
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER,         INTENT(IN) :: IELMX
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
      TYPE(BIEF_MESH), INTENT(INOUT),OPTIONAL :: MESH3D
      INTEGER,         INTENT(IN   ),OPTIONAL :: IELM3
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PRERES_KHIONE
     &(NPOIN,LT,TELSOR,TN,NPOIN3,NPLAN)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER,               INTENT(IN) :: LT,NPOIN
      TYPE(BIEF_OBJ),        INTENT(IN) :: TELSOR,TN
      INTEGER, OPTIONAL,     INTENT(IN) :: NPOIN3,NPLAN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PRERES3D_KHIONE
     &(LT,TN,NPOIN3,MESH)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER,               INTENT(IN) :: LT
      TYPE(BIEF_OBJ),        INTENT(IN) :: TN
      INTEGER,               INTENT(IN) :: NPOIN3
      TYPE(BIEF_MESH),       INTENT(IN) :: MESH
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SECTION_KHIONE
     &(MESH,IKLE,NELMAX,IFABOR,F,U,V,QVC,CA,CV,H,S,LT)
      USE BIEF_DEF
      USE DECLARATIONS_KHIONE, ONLY: NSECLOG
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NELMAX,LT
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),IFABOR(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: QVC(NSECLOG)
      DOUBLE PRECISION, INTENT(INOUT) :: CA(NSECLOG),CV(NSECLOG)
      TYPE(BIEF_OBJ), INTENT(IN) :: F,U,V,H,S
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SOURCE_ICOVER
     &(NPOIN,TEXP,TN,HN,DT)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)            :: NPOIN
      DOUBLE PRECISION,INTENT(IN)    :: DT
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TEXP
      TYPE(BIEF_OBJ), INTENT(IN)     :: HN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SOURCE_FRAZIL
     &( NPOIN,TEXP,TIMP,TN,HN,U,V,DT,CF,AK,EP,ITURB_TEL,LT,NPLAN)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)            :: NPOIN,ITURB_TEL,LT
      INTEGER, INTENT(IN), OPTIONAL  :: NPLAN
      DOUBLE PRECISION,INTENT(IN)    :: DT
      TYPE(BIEF_OBJ), INTENT(IN)     :: AK,EP
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TEXP,TIMP
      TYPE(BIEF_OBJ), INTENT(IN)     :: HN,U,V,CF
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SOURCE_HYDRO_ICOVER
     &(NPOIN,FU,FV,H,U,V,T1,T2,T3,S,MESH,MSK,MASKEL,GRAV,
     & KARMAN,CHESTR,DT,AT,KFROT,T4,T5,T6,CF,UNSV2D,IFV)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: NPOIN,KFROT,IFV
      LOGICAL,          INTENT(IN)    :: MSK
      DOUBLE PRECISION, INTENT(IN)    :: DT,AT,GRAV,KARMAN
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FU,FV
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CHESTR,H,U,V,CF,S
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,UNSV2D
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SOURCE_RAD
     & (NPOIN3,NPOIN2,NPLAN,Z,RHOT3D,TEXP)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)           :: NPOIN2,NPOIN3,NPLAN
      DOUBLE PRECISION, INTENT(IN)  :: Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: RHOT3D(NPOIN3)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TEXP
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SOURCE_THERMAL
     &( NPOIN,TEXP,TN,HN,U,V,T1,S,MESH,DT,AT,MARDAT,MARTIM,LAMBD0,
     &  NPOIN3)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN)            :: NPOIN
      INTEGER, INTENT(IN)            :: MARDAT(3),MARTIM(3)
      INTEGER, INTENT(IN), OPTIONAL  :: NPOIN3
      DOUBLE PRECISION,INTENT(IN)    :: DT,AT
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: T1,S
      TYPE(BIEF_MESH),INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)     :: U,V
      DOUBLE PRECISION, INTENT(IN)   :: LAMBD0,HN(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: TEXP(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE YASMI_KHIONE
     &  (YASMI)
      IMPLICIT NONE
      LOGICAL, INTENT(INOUT)::  YASMI(*)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      END MODULE INTERFACE_KHIONE
