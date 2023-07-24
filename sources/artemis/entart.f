!                   *****************
                    SUBROUTINE ENTART
!                   *****************
!
     &(ITITRE,X,NBR,NBRTOT,ALEMON,ALEMUL,BALAYE)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES HEADER LINES FOR VARIOUS AGITATION COMPUTATIONS
!+                IN THE LISTING FILE.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        02/06/1999
!+        V5P1
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
!| ALEMON         |-->| TRUE IF MONODIRECTIONAL RANDOM WAVES
!| ALEMUL         |-->| TRUE IF MULTIDIRECTIONAL RANDOM WAVES
!| BALAYE         |-->| TRUE IF PERIOD SCANNING
!| ITITRE         |-->| TYPE OF TITLE TO PRINT
!| NBR            |-->| NUMBER OF CURRENT PERIOD OR DIRECTION
!| NBRTOT         |-->| TOTAL NUMBER OF PERIOD OR DIRECTION
!| X              |-->| REAL TO PRINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_ARTEMIS, EX_ENTART => ENTART
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ITITRE,NBR
      INTEGER, INTENT(INOUT) :: NBRTOT
      DOUBLE PRECISION, INTENT(IN) :: X
      LOGICAL, INTENT(IN) :: ALEMON,ALEMUL,BALAYE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=32) :: TEXTFR(5),TEXTGB(5)
!
!-----------------------------------------------------------------------
!
!     NO DATA STATEMENT FOR TYPES WITH ALLOCATABLE COMPONENTS
      PARAMETER ( TEXTFR = (/
     &    'PERIODE                         ' ,
     &    ' SECONDES                       ' ,
     &    'DIRECTION                       ' ,
     &    ' DEGRES                         ' ,
     &    '                                ' /) )
      PARAMETER ( TEXTGB = (/
     &    'PERIOD                          ' ,
     &    ' SECONDS                        ' ,
     &    'DIRECTION                       ' ,
     &    ' DEGREES                        ' ,
     &    '                                ' /) )
!
! REGULAR WAVES
!
      IF (.NOT.ALEMON .AND. .NOT.ALEMUL .AND. .NOT.BALAYE) THEN
        NBRTOT = 1
      ENDIF
!
!-----------------------------------------------------------------------
!
!   WRITES OUT THE COMPUTED WAVE PERIOD
!
!
      IF (ITITRE.EQ.1) THEN
        IF(LNG.EQ.LNG_FR) WRITE(LU,100) TEXTFR(1),NBR,NBRTOT,X,TEXTFR(2)
        IF(LNG.EQ.LNG_EN) WRITE(LU,100) TEXTGB(1),NBR,NBRTOT,X,TEXTGB(2)
      ENDIF
!
100   FORMAT(/,80('='),/,7X,A8,I2,'/',I2,' : ',F12.4,A10,/)
!
!
!-----------------------------------------------------------------------
!
!   WRITES OUT THE COMPUTED WAVE DIRECTION
!
!
      IF (ITITRE.EQ.2) THEN
        IF(LNG.EQ.LNG_FR) WRITE(LU,110) TEXTFR(3),NBR,NBRTOT,X,TEXTFR(4)
        IF(LNG.EQ.LNG_EN) WRITE(LU,110) TEXTGB(3),NBR,NBRTOT,X,TEXTGB(4)
      ENDIF
!
110   FORMAT(/,7X,A10,I2,'/',I2,' : ',F12.4,A10,/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
