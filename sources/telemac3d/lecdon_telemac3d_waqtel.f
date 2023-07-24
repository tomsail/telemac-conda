!                   **********************************
                    SUBROUTINE LECDON_TELEMAC3D_WAQTEL
!                   **********************************
!
     &(MOTCAR,FILE_DESC,PATH,NCAR,CAS_FILE,DICO_FILE)
!
!***********************************************************************
! TELEMAC-3D
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_DESC      |<--| STORES STRINGS 'SUBMIT' OF DICTIONARY
!| MOTCAR         |<--| VALUES OF KEY-WORDS OF TYPE CHARACTER
!| NCAR           |-->| NUMBER OF LETTERS IN STRING PATH
!| PATH           |-->| FULL PATH TO CODE DICTIONARY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, ONLY: NAMETRAC, NTRAC,MAXTRA
      USE DECLARATIONS_WAQTEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
!     API
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: K,I
      INTEGER            :: MOTINT(MAXKEYWORD)
      INTEGER            :: TROUVE(4,MAXKEYWORD)
      INTEGER            :: ADRESS(4,MAXKEYWORD)
      INTEGER            :: DIMENS(4,MAXKEYWORD)
      DOUBLE PRECISION   :: MOTREA(MAXKEYWORD)
      LOGICAL            :: DOC
      LOGICAL            :: MOTLOG(MAXKEYWORD)
      CHARACTER(LEN=250) :: NOM_CAS
      CHARACTER(LEN=250) :: NOM_DIC
      CHARACTER(LEN=72)  :: MOTCLE(4,MAXKEYWORD,2)

      INTEGER :: ID_DICO, ID_CAS
      INTEGER NPRECONC,NPRETHI
      DOUBLE PRECISION, ALLOCATABLE :: PREFRZL_TMP(:)
!
!-----------------------------------------------------------------------
!
! INITIALISES THE VARIABLES FOR DAMOCLES CALL :
!
      IF(7*INT(WAQPROCESS/7).EQ.WAQPROCESS) THEN
        DO K = 1, MAXKEYWORD
!         A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
!         (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
          MOTCAR(K)(1:1)=' '
!
          DIMENS(1,K) = 0
          DIMENS(2,K) = 0
          DIMENS(3,K) = 0
          DIMENS(4,K) = 0
        ENDDO
!
!       WRITES OUT INFO
        DOC = .FALSE.
!
!-----------------------------------------------------------------------
!     OPENS DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
        IF(NCAR.GT.0) THEN
!
          NOM_DIC=PATH(1:NCAR)//'WAQDICO'
          NOM_CAS=PATH(1:NCAR)//'WAQCAS'
!
        ELSE
!
          NOM_DIC='WAQDICO'
          NOM_CAS='WAQCAS'
!
        ENDIF
        IF((CAS_FILE(1:1).NE.' ').AND.(DICO_FILE(1:1).NE.' ')) THEN
          NOM_DIC=DICO_FILE
          NOM_CAS=CAS_FILE
        ENDIF
!
        CALL GET_FREE_ID(ID_DICO)
        OPEN(ID_DICO,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
        CALL GET_FREE_ID(ID_CAS)
        OPEN(ID_CAS,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
        CALL DAMOCLE
     &  (ADRESS, DIMENS , MAXKEYWORD  , DOC    , LNG   , LU    , MOTINT,
     &  MOTREA, MOTLOG, MOTCAR, MOTCLE , TROUVE, ID_DICO, ID_CAS,
     &  .FALSE.,FILE_DESC)
!
!-----------------------------------------------------------------------
!     CLOSES DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
        CLOSE(ID_DICO)
        CLOSE(ID_CAS)
!
!       DECODES 'SUBMIT' CHAINS
!
        CALL READ_SUBMIT(WAQ_FILES,MAXLU_WAQ,FILE_DESC,MAXKEYWORD)
!
!-----------------------------------------------------------------------
!
!       KINETIC MODEL FOR MICROPOL
        KIN_MICROPOL = MOTINT( ADRESS(1,  14) )
!
!----------------------------------------------------------------
!
      ENDIF
      CALL NAMETRAC_WAQTEL(NAMETRAC,NTRAC,MAXTRA,WAQPROCESS)
!
!----------------------------------------------------------------
!
      RETURN
      END
