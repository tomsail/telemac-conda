!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!                  INTERFACES FOR ARTEMIS SUBROUTINES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
!#######################################################################
!
      MODULE INTERFACE_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
!
!-----------------------------------------------------------------------
!
!     DEFINES INTERFACES
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE BERKHO(LT)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: LT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCFW
     &(I,H,K,HMU,NPOIN,OMEGA,GRAV,VISCO,
     & DIAM90,DIAM50,MVSED,MVEAU,
     & REGIDO,RICOEF,ENTREG,ENTRUG,FFW)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(IN) :: H(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: K(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: HMU(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: GRAV
      DOUBLE PRECISION, INTENT(IN) :: OMEGA
      DOUBLE PRECISION, INTENT(IN) :: VISCO
      DOUBLE PRECISION, INTENT(IN) :: DIAM90
      DOUBLE PRECISION, INTENT(IN) :: DIAM50
      DOUBLE PRECISION, INTENT(IN) :: MVSED
      DOUBLE PRECISION, INTENT(IN) :: MVEAU
      DOUBLE PRECISION, INTENT(IN) :: RICOEF
      INTEGER, INTENT(IN) :: REGIDO
      LOGICAL, INTENT(IN) :: ENTREG
      LOGICAL, INTENT(IN) :: ENTRUG
      DOUBLE PRECISION, INTENT(INOUT) :: FFW
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ART_READ_BIN_FRLIQ
     &(NOM)
      IMPLICIT NONE
      CHARACTER(LEN=16),INTENT(IN)            :: NOM
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCMU
     &(ITERMU)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: ITERMU
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCUE
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCTM
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCMN
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ARTEMIS
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ARTEMIS_CONSTANTS
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ART_CORFON
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCQB
     &(Q1,Q2,Q3)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(INOUT) :: Q1,Q2,Q3
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALRES
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALRE2
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE MASQUE_ARTEMIS
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALUEB2
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LISSAGE
     &(NVAL,ENTREE,SORTIE)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NVAL
      DOUBLE PRECISION, INTENT(INOUT) :: ENTREE(NVAL)
      DOUBLE PRECISION, INTENT(INOUT) :: SORTIE(NVAL)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE POINT_ARTEMIS
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PHBOR
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CONDIH
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SOLVELAMBDA
     &(XK,XUC,XVC,XKX,XKY,XH)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(INOUT) :: XK
      DOUBLE PRECISION, INTENT(IN)    :: XUC,XVC,XH,XKX,XKY
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE TWCALE
     &(ISPEC)
      IMPLICIT NONE
      INTEGER, INTENT(IN)           :: ISPEC
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE TWCCLOSEST
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE TWCAL2
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DEALL_ARTEMIS
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE GET_TOMSPEC_VALUE1
     &(SPEC)
      USE BIEF_DEF
      IMPLICIT NONE
      TYPE(SPECTRUM)   , INTENT(INOUT)        :: SPEC
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE GET_TOMSPEC_VALUES
     &(CHAINTWC, SPEC)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER                                 :: CHAINTWC
      TYPE(SPECTRUM)   , INTENT(INOUT)        :: SPEC
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE GET_TOMSPEC_VALUE2
     &(SPEC)
      USE BIEF_DEF
      IMPLICIT NONE
      TYPE(SPECTRUM)   , INTENT(INOUT)        :: SPEC
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE GET_TOMSPEC_DIMENSIONS
     &(NSPEC1,NDIR1,NF1)
      IMPLICIT NONE
      INTEGER, INTENT(OUT)         :: NSPEC1  ! NUMBER OF SPECTRA
      INTEGER, INTENT(OUT)         :: NDIR1   ! NUMBER OF DIRECTIONS
      INTEGER, INTENT(OUT)         :: NF1     ! NUMBER OF FREQUENCIES
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALDIR
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALTETAP(TETA,XSGBOR,YSGBOR,ADIR,NPTFR)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPTFR
      DOUBLE PRECISION, INTENT(INOUT) :: TETA(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: ADIR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)  :: XSGBOR(NPTFR,4),YSGBOR(NPTFR,4)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CNTPRE
     &(DAM,NPOIN,IPRECO,IPREC2)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(INOUT) :: IPRECO, IPREC2
      DOUBLE PRECISION, INTENT(IN) :: DAM(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DIRALE
     &(DALE,EXPOS,TETAH,TETMIN,TETMAX,NDALE)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDALE
      DOUBLE PRECISION, INTENT(INOUT) :: DALE(NDALE)
      DOUBLE PRECISION, INTENT(IN) :: EXPOS,TETMIN,TETMAX,TETAH
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DISMOY
     &(NPOIN,NELEM,X,Y,IKLE,K,LISHHO)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN,NELEM
      INTEGER, INTENT(INOUT) :: LISHHO
      INTEGER, INTENT(IN) :: IKLE(NELEM,*)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN),K(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ENTART
     &(ITITRE,X,NBR,NBRTOT,ALEMON,ALEMUL,BALAYE)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: ITITRE,NBR
      INTEGER, INTENT(INOUT) :: NBRTOT
      DOUBLE PRECISION, INTENT(IN) :: X
      LOGICAL, INTENT(IN) :: ALEMON,ALEMUL,BALAYE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION FCTE1(KH)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: KH
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION FCTE2(KH)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: KH
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FWSPEC
     &(FW,FWCOEF,X,Y,NPOIN,PRIVE,ZF)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: FW(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN),FWCOEF
      TYPE(BIEF_OBJ), INTENT(IN) :: PRIVE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_FWSPEC
     &(FW,FWCOEF,X,Y,NPOIN,PRIVE,ZF)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: FW(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN),FWCOEF
      TYPE(BIEF_OBJ), INTENT(IN) :: PRIVE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECDON_ARTEMIS
     &(FILE_DESC,PATH,NCAR,
     & CAS_FILE,DICO_FILE)
        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
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
        SUBROUTINE NOMVAR_ARTEMIS
     &(TEXTE,TEXTPR,MNEMO)
      USE DECLARATIONS_ARTEMIS, ONLY : MAXVAR
      IMPLICIT NONE
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(MAXVAR),TEXTPR(MAXVAR)
      CHARACTER(LEN=8), INTENT(INOUT) :: MNEMO(MAXVAR)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PENTCO(II)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: II
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PERALE
     &(PALE,GAMMA,PERPIC,NPALE,PMIN,PMAX)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPALE
      DOUBLE PRECISION, INTENT(INOUT) :: PALE(NPALE)
      DOUBLE PRECISION, INTENT(IN) :: PERPIC,GAMMA
      DOUBLE PRECISION, INTENT(IN) :: PMIN  ,PMAX
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RADIA1(LISHHO)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: LISHHO
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RELAXMU
     &(ECRHMU,MODHMU,ITERMU)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: ITERMU
      DOUBLE PRECISION, INTENT(INOUT) :: ECRHMU,MODHMU
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RADIA2(LISHHO)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: LISHHO
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION SPD(TETA)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN) :: TETA
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE STIRLING
     &(NI,XI,YI,NO,XOSTEP,YO)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NI,NO
      DOUBLE PRECISION, INTENT(IN) :: XI(NI),YI(NI)
      DOUBLE PRECISION, INTENT(INOUT) :: XOSTEP,YO(NO)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION SPE(F)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN) :: F
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION STWC1(F,DIR,SPEC,I)
        USE BIEF_DEF, ONLY: SPECTRUM
        IMPLICIT NONE
        TYPE(SPECTRUM)   , INTENT(IN) :: SPEC
        DOUBLE PRECISION, INTENT(IN)  :: F,DIR
        INTEGER, INTENT(IN)           :: I
        END FUNCTION
      END INTERFACE
!
      INTERFACE
        SUBROUTINE STWC2
     &(IMIN,IMAX,N,DIR2,SDIR)
        IMPLICIT NONE
      INTEGER, INTENT(IN)           :: IMIN,IMAX,N
      DOUBLE PRECISION, INTENT(INOUT) :: SDIR(N),DIR2(N)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE XY_TOMAWAC
     &( SPEC )
      USE BIEF_DEF
      IMPLICIT NONE
      TYPE(SPECTRUM) , INTENT(INOUT) :: SPEC
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE UTIMP_ART
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_UTIMP_ART
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ALLSPEC
     &(SPEC,NOM)
        USE DECLARATIONS_ARTEMIS
        IMPLICIT NONE
        TYPE(SPECTRUM)   , INTENT(INOUT)        :: SPEC
        CHARACTER(LEN=6) , INTENT(IN)           :: NOM
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_ART_CORFON
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE BORH
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_BORH
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_CONDIH
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE USER_CONDIH_PARTICULAR
      IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!=======================================================================
!
      END MODULE INTERFACE_ARTEMIS
!
!#######################################################################
!
