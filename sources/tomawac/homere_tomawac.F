!                   **********************
                    PROGRAM HOMERE_TOMAWAC
!                   **********************
!
!
!***********************************************************************
! TOMAWAC   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    MAIN PROGRAM FOR TOMAWAC.
!+                SOLVES THE EQUATION FOR THE
!+                MULTIDIRECTIONAL WAVE SPECTRUM.
!+
!+     1) READS IN THE NECESSARY INFORMATION FOR MEMORY ALLOCATION,
!+
!+     2) ALLOCATES THE MEMORY,
!+
!+     3) CALLS THE REAL MAIN PROGRAM WAC.
!
!history  OPTIMER
!+        12/01/01
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
!history  C VILLARET (HRW+EDF) & J-M HERVOUET (EDF - LNHE)
!+        18/09/2014
!+        V7P0
!+   Adding STRA01 for the variable ORBVEL_TEL in wac arguments
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
      INTEGER TDEB(8),TFIN(8),NCAR
!
      CHARACTER(LEN=24), PARAMETER :: CODE='TOMAWAC                '
!
      CHARACTER(LEN=MAXLENTMPDIR) PATH
      CHARACTER(LEN=PATH_LEN) FILE_DESC(4,MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN) DUMMY
!
!======================================================================
!
#if defined COMPAD
      CALL AD_TOMAWAC_MAIN_INIT
#endif
      DUMMY = REPEAT(' ',PATH_LEN)
!
!-----------------------------------------------------------------------
!
      CALL BIEF_INIT(PATH,NCAR,.TRUE.)
!
!     INITIAL TIME FOR COMPUTATION DURATION
!
      CALL DATE_AND_TIME(VALUES=TDEB)
!
!-----------------------------------------------------------------------
!     EN TETE   -  HEADING
!-----------------------------------------------------------------------
!
      CALL PRINT_HEADER(CODE,'                        ')
!
!-----------------------------------------------------------------------
!
!     READS THE STEERING FILE
!
      CALL LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,DUMMY,DUMMY,PART=-1)
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TOMAWAC_MAIN_AFTER_LECDON_TOMAWAC
#endif
!
!-----------------------------------------------------------------------
!
!     OPENS THE FILES
!
      CALL BIEF_OPEN_FILES(CODE,WAC_FILES,MAXLU_WAC,PATH,NCAR,
     &                     1,.FALSE.)
!
!-----------------------------------------------------------------------
!
!     ALLOCATES MEMORY
!
      CALL POINT_TOMAWAC
!
!-----------------------------------------------------------------------
!
!     CALLS THE REAL MAIN PROGRAM
!
      CALL WAC(PART=-1)
!
!-----------------------------------------------------------------------
!
!     CLOSES THE FILES
!
      CALL BIEF_CLOSE_FILES(WAC_FILES,MAXLU_WAC,.TRUE.)
      CALL DEALL_TOMAWAC()
      CALL DEALL_BIEF()
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TOMAWAC_MAIN_FINALIZE
#endif
!
!-----------------------------------------------------------------------
!
      WRITE(LU,20)
20    FORMAT(1X,////,1X,'CORRECT END OF RUN',/////)
!
!-----------------------------------------------------------------------
!
!     TIME OF END OF COMPUTATION
!
      CALL DATE_AND_TIME(VALUES=TFIN)
      CALL ELAPSE(TDEB,TFIN)
!
!-----------------------------------------------------------------------
!
      STOP 0
      END
