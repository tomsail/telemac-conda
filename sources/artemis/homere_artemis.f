!                   **********************
                    PROGRAM HOMERE_ARTEMIS
!                   **********************
!
!
!***********************************************************************
! ARTEMIS   V7P1
!***********************************************************************
!
!brief    1)  ACQUIRES DATA REQUIRED TO ALLOCATE MEMORY
!+                   (STEERING FILE + GEOMETRY)
!+
!+
!+            2)  CALLS THE REAL MAIN PROGRAM ARTEMIS.
!
!history  J-M HERVOUET (LNH)
!+        24/04/1997
!+
!+   LINKED TO BIEF 5.5
!
!history  D. AELBRECHT (LNH)
!+        19/04/1999
!+        V6P0
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER TDEB,TFIN,NCAR
!
      CHARACTER(LEN=24), PARAMETER :: CODE='ARTEMIS                 '
!
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
!
!======================================================================
!
      CHARACTER(LEN=MAXLENTMPDIR) PATH
      CHARACTER(LEN=PATH_LEN) FILE_DESC(4,MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN) DUMMY
!
!-----------------------------------------------------------------------
!
      CALL BIEF_INIT(PATH,NCAR,.TRUE.)
!
      TDEB = TIME_IN_SECONDS()
!
!     HEADING
!
      CALL PRINT_HEADER(CODE,'                        ')
!
!-----------------------------------------------------------------------
!
!     READS THE STEERING FILE
!
      DUMMY = ' '
      CALL LECDON_ARTEMIS(FILE_DESC,PATH,NCAR,DUMMY,DUMMY)
!-----------------------------------------------------------------------
!
!     OPENS THE FILES
!
      CALL BIEF_OPEN_FILES(CODE,ART_FILES,MAXLU_ART,PATH,NCAR,
     &                     1,.FALSE.)
!-----------------------------------------------------------------------
!
!     ALLOCATES MEMORY FOR BIEF_OBJ STRUCTURES (VECTORS, MATRICES)
!
!
      CALL POINT_ARTEMIS
!
!-----------------------------------------------------------------------
!
!     CALLS REAL MAIN PROGRAM
!
      CALL ARTEMIS
!
!-----------------------------------------------------------------------
!
      CALL BIEF_CLOSE_FILES(ART_FILES,MAXLU_ART,.TRUE.)

      CALL DEALL_ARTEMIS
      CALL DEALL_BIEF
!
!-----------------------------------------------------------------------
!
      WRITE(LU,11)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!-----------------------------------------------------------------------
!
      TFIN = TIME_IN_SECONDS()
      WRITE(LU,*) 'COMPUTER TIME: ',TFIN-TDEB,' SECONDS'
!
!-----------------------------------------------------------------------
!
      STOP 0
      END
