!
!  DECLARATION OF GLOBAL DATA STRUCTURE IN TELEMAC-2D
!
      MODULE DECLARATIONS_STBTEL
!
!***********************************************************************
!  STBTEL VERSION 6.2
!***********************************************************************
!
!
      USE DECLARATIONS_SPECIAL
      USE UTILS_MED
      CHARACTER(LEN=MAXLENFILE) NOMGEO,NOMFO1,NOMFON,NOMFO2,NOMIMP,
     &                          NOMSOU,NOMFRC
      CHARACTER(LEN=MAXLENFILE) NOMFOR,NOMCAS,NOMLIM,NOMRES,NOMBND2
      INTEGER NGEO,NCLE,NCAS,NLIM,NFO1,NFON,NFO2,NIMP,NSOU,NFRC,NRES
      INTEGER NBND2
!
!     COMMON GEO
!
      INTEGER NELEM
      INTEGER MESH
      INTEGER NDP
      INTEGER NPOIN
      INTEGER NELMAX
      INTEGER NPMAX
!
!-----------------------------------------------------------------------
!
!       4) INTEGERS
!
!-----------------------------------------------------------------------
!
!       KEY-WORDS AND PARAMETERS
!
      INTEGER NBAT,LGVEC,NSOM,NSOM2,NBFOND,IHAUT,INOP5,NSEC2,NSEC3
      INTEGER NSEC11,NSEC12
      INTEGER MAX_SEG_PER_POINT
!
!-----------------------------------------------------------------------
!
!       5) LOGICAL VALUES
!
!-----------------------------------------------------------------------
!
      LOGICAL OPTASS,DECTRI,COLOR,ELIDEP,DIV4,FONTRI,ADDFAS,PROJEX
      LOGICAL FUSION,ELISEC,ELPSEC,STOTOT,AUTO_PRECISION
!
!-----------------------------------------------------------------------
!
!       6) REALS
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION EPSI,DM,SOM(10,2),CORTRI,SEUSEC
      DOUBLE PRECISION, ALLOCATABLE :: SOM2(:,:)
!
!-----------------------------------------------------------------------
!
!       7) STRINGS
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=3)  STD
      CHARACTER(LEN=9)  MAILLE
      CHARACTER(LEN=PATH_LEN) FOND(5)

      ! VARIABLE FOR HERMES
      INTEGER :: TYP_ELEM, TYP_BND_ELEM, NPTIR
      CHARACTER(LEN=8) :: FFORMAT, OUT_FORMAT
!
!-----------------------------------------------------------------------
!     CONVERTER
!-----------------------------------------------------------------------
!
!!!!  DICTIONARY KEYWORDS
      ! NAME OF THE INPUT FORMAT
      CHARACTER(LEN=9)  INFMT
      ! NAME OF THE OUTPUT FORMAT
      CHARACTER(LEN=9)  OUTFMT
      ! NAME OF THE INPUT FILES
      CHARACTER(LEN=MAXLENFILE) INFILE,BOUNDFILE,LOGFILE
      INTEGER NINP,NBND,NLOG
      ! NAME OF THE OUTPUT FILES
      CHARACTER(LEN=MAXLENFILE) OUTFILE,OUTBNDFILE,OUTLOGFILE
      INTEGER NOUT,NOBND,NOLOG
      ! DICTIONARY KEY WORDS
      LOGICAL CONVER,DEBUG,SRF_BND
      ! INFO FOR TRANSLATION OF MESH
      LOGICAL :: TRANSLATE
      DOUBLE PRECISION DX,DY
!!!!  PARAMETERS
      ! MAX NUMBER OF FAMILY
      INTEGER, PARAMETER :: MAXFAM = 40
      ! LENGTH OF THE STRINGS
#if defined HAVE_MED
      INTEGER, PARAMETER :: NAME_SIZE = MED_NAME_SIZE
      INTEGER, PARAMETER :: LNAME_SIZE = MED_LNAME_SIZE
      INTEGER, PARAMETER :: COMMENT_SIZE = MED_COMMENT_SIZE
      INTEGER, PARAMETER :: SNAME_SIZE = MED_SNAME_SIZE
#else
      INTEGER, PARAMETER :: NAME_SIZE = 64
      INTEGER, PARAMETER :: LNAME_SIZE = 80
      INTEGER, PARAMETER :: COMMENT_SIZE = 200
      INTEGER, PARAMETER :: SNAME_SIZE = 16
#endif
      INTEGER, PARAMETER :: TITLE_SIZE = 80
!
      SAVE
!
      CONTAINS
!
      SUBROUTINE RENAME_VECTOR(STR,N)
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT) :: STR
      INTEGER, INTENT(IN) :: N
      INTEGER :: POS
      POS = INDEX(STR,'*')
      IF (STR(POS+1:POS+1).EQ.'0') THEN
        IF(N.EQ.1)STR(POS:POS)='U'
        IF(N.EQ.2)STR(POS:POS)='V'
        IF(N.EQ.3)STR(POS:POS)='W'
      ELSE
        IF(N.EQ.1)STR(POS:POS)='X'
        IF(N.EQ.2)STR(POS:POS)='Y'
        IF(N.EQ.3)STR(POS:POS)='Z'
      ENDIF
      END SUBROUTINE RENAME_VECTOR
!
!
      CHARACTER(LEN=11) FUNCTION EXTENS
     &                               (N,IPID)
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER IPID,N
!
      IF(N.GT.0) THEN
!
        EXTENS='00000-00000'
!
        IF(N.LT.10) THEN
          WRITE(EXTENS(05:05),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(EXTENS(04:05),'(I2)') N
        ELSEIF(N.LT.1000) THEN
          WRITE(EXTENS(03:05),'(I3)') N
        ELSEIF(N.LT.10000) THEN
          WRITE(EXTENS(02:05),'(I4)') N
        ELSE
          WRITE(EXTENS(01:05),'(I5)') N
        ENDIF
!
        IF(IPID.LT.10) THEN
          WRITE(EXTENS(11:11),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(EXTENS(10:11),'(I2)') IPID
        ELSEIF(IPID.LT.1000) THEN
          WRITE(EXTENS(09:11),'(I3)') IPID
        ELSEIF(IPID.LT.10000) THEN
          WRITE(EXTENS(08:11),'(I4)') IPID
        ELSE
          WRITE(EXTENS(07:11),'(I5)') IPID
        ENDIF
!
      ELSE
!
        EXTENS='       '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION EXTENS
!
!
      ! CONVERT AN INT INTO A STRING WITH ZEROS
      SUBROUTINE NUMBER2STRING(RES,VAL)
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: VAL
      CHARACTER(LEN=*), INTENT(OUT) :: RES
      IF(VAL.LT.10) THEN
        WRITE(RES,'(A7,I1)') '0000000',VAL
      ELSE IF(VAL.LT.100) THEN
        WRITE(RES,'(A6,I2)') '000000',VAL
      ELSE IF(VAL.LT.1000) THEN
        WRITE(RES,'(A5,I3)') '00000',VAL
      ELSE IF(VAL.LT.10000) THEN
        WRITE(RES,'(A4,I4)') '0000',VAL
      ELSE IF(VAL.LT.100000) THEN
        WRITE(RES,'(A3,I5)') '000',VAL
      ELSE IF(VAL.LT.1000000) THEN
        WRITE(RES,'(A2,I6)') '00',VAL
      ELSE IF(VAL.LT.10000000) THEN
        WRITE(RES,'(A1,I7)') '0',VAL
      ELSE
        WRITE(RES,'(I8)') VAL
      ENDIF
      END SUBROUTINE NUMBER2STRING
!
!
      END MODULE DECLARATIONS_STBTEL
!
