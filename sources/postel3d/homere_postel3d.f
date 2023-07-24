!                         ***********************
                          PROGRAM HOMERE_POSTEL3D
!                         ***********************
!
!***********************************************************************
!  POSTEL3D VERSION 6.3
!***********************************************************************
!
!-----------------------------------------------------------------------
!
!                       PROGRAMME PRINCIPAL DE
!
!       PPPP    OOO    SSSS  TTTTT  EEEEE  L         33333  DDDD
!       P   P  O   O  S        T    E      L             3  D   D
!       PPPP   O   O   SSS     T    EEEE   L     ---   333  D   D
!       P      O   O      S    T    E      L             3  D   D
!       P       OOO   SSSS     T    EEEEE  LLLLL     33333  DDDD
!
!
!                INTERFACE ENTRE TELEMAC-3D ET RUBENS
!
!-----------------------------------------------------------------------
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_POSTEL3D
      USE INTERFACE_POSTEL3D
      USE DECLARATIONS_SPECIAL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER TDEB(8),NCAR
!
      CHARACTER(LEN=MAXLENTMPDIR) PATH
      CHARACTER(LEN=PATH_LEN) MOTCAR(MAXKEYWORD),FILE_DESC(4,MAXKEYWORD)
!
      CHARACTER(LEN=24), PARAMETER :: CODE='POSTEL3D                '
!
      CALL BIEF_INIT(PATH,NCAR,.TRUE.)
!
!     INITIAL TIME FOR COMPUTATION DURATION
!
      CALL DATE_AND_TIME(VALUES=TDEB)
!
      CALL PRINT_HEADER(CODE,'                        ')
!
      CALL LECDON_POSTEL3D(MOTCAR,FILE_DESC,PATH,NCAR)
!
      CALL BIEF_OPEN_FILES(CODE,POS_FILES,100,PATH,NCAR,1,.FALSE.)
!
      CALL POINT_POSTEL3D
      CALL POSTEL3D
      CALL BIEF_CLOSE_FILES(POS_FILES,100,.TRUE.)
!
!
      WRITE(LU,12)
!
12    FORMAT(////,' CORRECT END OF RUN',/////)
!
!-----------------------------------------------------------------------
!
      STOP 0
      END
