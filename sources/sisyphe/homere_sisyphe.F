!                   **********************
                    PROGRAM HOMERE_SISYPHE
!                   **********************
!
!
!***********************************************************************
! SISYPHE   V7P0
!***********************************************************************
!
!brief    1) ACQUIRES THE DATA REQUIRED TO DEFINE THE POINTERS:
!+                   STEERING FILE + GEOMETRY FILE (PARTIALLY ONLY).
!+
!+            2) CALLS THE SUBROUTINE SISYPHE.
!
!history  C LE NORMANT (LNH)
!+        09/04/2009
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
!+   Adding T1 for the variable ORBVEL_TEL in sisyphe arguments
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
      USE INTERFACE_SISYPHE
!
      IMPLICIT NONE
!
      INTEGER TDEB,TFIN,NCAR,DUMINT
      LOGICAL DUMLOG, DUMLOG2, DUMLOG3, DUMLOG4
!
      CHARACTER(LEN=24), PARAMETER :: CODE='SISYPHE                 '
!
      CHARACTER(LEN=MAXLENTMPDIR) PATH
      CHARACTER(LEN=PATH_LEN) MOTCAR(MAXKEYWORD),FILE_DESC(4,MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN) DUMMY
!
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
!
!======================================================================
!
#if defined COMPAD
      CALL AD_SISYPHE_MAIN_INIT
#endif
!
!-----------------------------------------------------------------------
!
      CALL BIEF_INIT(PATH,NCAR,.TRUE.)
!
      TDEB = TIME_IN_SECONDS()
!
!  HEADING TO THE LISTING
!
      CALL PRINT_HEADER(CODE,'                        ')
!
!-----------------------------------------------------------------------
!
! READS THE STEERING FILE
!
      DUMMY = ' '
      CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,
     &                      NCAR,CODE,DUMMY,DUMMY)
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_SISYPHE_MAIN_AFTER_LECDON_SISYPHE
#endif
!
!-----------------------------------------------------------------------
!
      CALL BIEF_OPEN_FILES(CODE,SIS_FILES,MAXLU_SIS,
     &                     PATH,NCAR,2,.FALSE.)
!
!-----------------------------------------------------------------------
!
! ALLOCATES VECTORS, MATRICES AND BLOCKS
!
      CALL POINT_SISYPHE
!
!-----------------------------------------------------------------------
!
!  CALLS THE SUBROUTINE SISYPHE
!  -1 GOES THROUGH THE WHOLE SUBROUTINE BECAUSE THERE IS NO COUPLING
!  THE OTHER VARIABLES ARE ONLY USED WHEN COUPLING
!
!     INOUT VARIABLES IN SISYPHE CANNOT BE HARD-CODED
      DUMINT=1
      DUMLOG=.FALSE.
      DUMLOG2=.FALSE.
      DUMLOG3=.FALSE.
      DUMLOG4=.FALSE.
!
      CALL SISYPHE(-1,0,0,0,0,T1,T1,T1,T1,T1,T1,T1,T1,
     &             DUMLOG,DUMINT,DUMLOG4,CODE,1,
     &             T1,T1,0.D0,T1,0.D0,DUMLOG3,DUMLOG2,
     &             T1,1,T1,T1,T1,T1,T1,T1,T1,T1,.TRUE.)
!
!-----------------------------------------------------------------------
!
      CALL BIEF_CLOSE_FILES(SIS_FILES,MAXLU_SIS,.TRUE.)

      CALL DEALL_SISYPHE
      CALL DEALL_BIEF
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_SISYPHE_MAIN_FINALIZE
#endif
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
      END PROGRAM HOMERE_SISYPHE
