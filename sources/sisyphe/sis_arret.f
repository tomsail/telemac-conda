!                   **********************
                    SUBROUTINE SIS_ARRET !
!                   **********************
!
     &(ESM,EMAX,HN,VARSOR,NPOIN,MN,NRES,FMTRES,MAXVAR,AT0,RC,
     & TEXTE,SORLEO,SORIMP,T1,T2)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    STOP TEST IN CASE EVOLUTION BECOMES TOO IMPORTANT.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+        V5P1
!+
!
!history  F. HUVELIN
!+        05/01/2004
!+        V5P8
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT0            |-->|TIME
!| EMAX           |-->|MAXIMUM BED EVOLUTION
!| ESM            |-->|CUMULATED BED EVOLUTION
!| FMTRES         |-->|FORMAT OF RESULT FILE
!| HN             |-->| WATER DEPTH
!| MAXVAR         |-->| MAXIMUM NUMBER OF VARIABLES
!| MN             |-->| NUMBER OF TIME STEP
!| NPOIN          |-->| NUMBER OF POINTS
!| NRES           |-->| LOGICAL UNIT OF THE RESULTS FILE
!| RC             |-->| CRITICAL EVOLUTION RATIO
!| SORIMP         |-->| ARRAY OF LOGICAL SAYING IF VARIABLES MUST BE PUT
!|                |   | IN THE LISTING
!| SORLEO         |-->| ARRAY OF LOGICAL SAYING IF VARIABLES MUST BE PUT
!|                |   | IN THE RESULTS FILE
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| TEXTE          |-->| NAMES AND UNITS OF VARIABLES
!| VARSOR         |-->| BLOCK WITH VARIABLES TO BE PRINTED OR COPIED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_SIS_ARRET => SIS_ARRET
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),    INTENT(IN)    :: ESM, EMAX, HN, VARSOR
      INTEGER,           INTENT(IN)    :: NPOIN, MN, NRES, MAXVAR
      DOUBLE PRECISION,  INTENT(IN)    :: AT0, RC
      CHARACTER(LEN=32), INTENT(IN)    :: TEXTE(MAXVAR)
      CHARACTER(LEN=8),  INTENT(IN)    :: FMTRES
      LOGICAL,           INTENT(IN)    :: SORLEO(*), SORIMP(*)
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: T1, T2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: IMIN
      DOUBLE PRECISION :: XMIN
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! ************************************ !
      ! I - COMPUTES F ABSOLUTE VALUE        !
      ! ************************************ !
      CALL OS('X=ABS(Y)', X=T1, Y=ESM)
      ! ************************************************** !
      ! II - COMPUTES THE DIFFERENCE BETWEEN THE AUTHORISED! (_IMP_)
      !      VALUE FOR DEPOSITION AND F ABSOLUTE VALUE     !
      ! ************************************************** !
      CALL OS('X=Y-Z   ', X=T2, Y=EMAX, Z=T1)
      ! ************************************************************** !
      ! III - COMPUTES THE MINIMUM VALUE OF THIS DIFFERENCE (NPOIN)    !
      ! ************************************************************** !
      CALL MINI(XMIN, IMIN, T2%R, NPOIN)
      ! ************************************************************* !
      ! IV - IF THE MINIMUM VALUE IS NEGATIVE, COMPUTATION IS STOPPED !
      ! ************************************************************* !
      IF (XMIN < 0.D0) THEN
        ! IV.1 - PRINTS THE VALUES
        ! --------------------------
        WRITE(LU,401) MN
        WRITE(LU,*) ' '
        WRITE(LU,403) IMIN
        WRITE(LU,405) HN%R(IMIN)
        WRITE(LU,407) RC
        WRITE(LU,409) EMAX%R(IMIN)
        WRITE(LU,411) ESM%R(IMIN)
        WRITE(LU,413) AT0
        WRITE(LU,*) ' '
        WRITE(LU,*) 'LAST RESULT SAVED'
!
        ! IV.2 - SAVES THE LAST RESULT
        ! -----------------------------
        CALL PREDES(1,AT0,.TRUE.,'TELEMAC2D               ')
        CALL BIEF_DESIMP(FMTRES,VARSOR,NPOIN,NRES,AT0,
     &                   1,1,1,SORLEO,SORIMP,MAXVAR,TEXTE,1,1)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      !----------------------------------------------------------------!
401   FORMAT(1X,/,' TOO MUCH EVOLUTION AT COMPUTATION: ',1I6)
403   FORMAT(' NODE NUMBER                     : ',1I6)
405   FORMAT(' WATER DEPTH                     : ',G16.7)
407   FORMAT(' CRITICAL EVOLUTION RATIO        : ',G16.7)
409   FORMAT(' MAXIMAL ALLOWED EVOLUTION       : ',G16.7)
411   FORMAT(' COMPUTED CUMULATED EVOLUTION    : ',G16.7)
413   FORMAT(' TIME                            : ',G16.7)
      !----------------------------------------------------------------!
!======================================================================!
!======================================================================!
      RETURN
      END
