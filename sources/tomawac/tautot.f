!                   *****************
                    SUBROUTINE TAUTOT
!                   *****************
!
     &( TAUT  , UVENT , TAUW  , SEUIL ,
     &  ITR   , ITRMIN, ITRMAX)
!
!***********************************************************************
! TOMAWAC   V6P1                                   28/06/2011
!***********************************************************************
!
!brief    COMPUTES THE TOTAL STRESS FROM THE WIND VELOCITY
!+                UVENT AT THE ELEVATION ZVENT (IN PRINCIPLE ZVENT=10 M)
!+                AND FROM THE WAVE STRESS TAUW.
!+
!+            THEORY DEVELOPED BY JANSSEN (1989 AND 1991) AND USED
!+                IN WAM-CYCLE 4 (SUBROUTINE STRESS OF PREPROC).
!
!note     TAUT IS COMPUTED FROM UVENT AND TAUW (SOLVES THE EQUATION
!+          IMPLICITLY USING AN ITERATIVE METHOD - NEWTON).
!
!reference  JANSSEN P.A.E.M (1989) :
!+                     "WIND-INDUCED STRESS AND THE DRAG OF AIR FLOW
!+                      OVER SEA WAVES". JPO, VOL 19, PP 745-754.
!reference JANSSEN P.A.E.M (1991) :
!+                     "QUASI-LINEAR THEORY OF WIND-WAVE GENERATION
!+                      APPLIED TO WAVE FORECASTING". JPO, VOL 21, PP 1631-1642.
!
!history  M. BENOIT (EDF/DER/LNH)
!+        25/04/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        28/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ITR            |<--| NOMBRE D'ITERATIONS EFFECTUES
!| ITRMAX         |-->| MAXIMUM NUMBER OF ITERATIONS
!| ITRMIN         |-->| MINIMUM NUMBER OF ITERATIONS
!| SEUIL          |-->| CONVERGENCY THRESHOLD - NEWTON METHOD
!| TAUT           |<--| TOTAL STRESS
!| TAUW           |-->| STRESS DUE TO THE WAVES
!| UVENT          |-->| WIND SPEED AT HEIGHT ZVENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : GRAVIT, XKAPPA, ALPHA, CDRAG,
     &                ZVENT
      USE INTERFACE_TOMAWAC, EX_TAUTOT => TAUTOT
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    ::  ITRMIN, ITRMAX
      INTEGER, INTENT(INOUT) ::  ITR
      DOUBLE PRECISION, INTENT(IN)    :: UVENT , TAUW
      DOUBLE PRECISION, INTENT(IN)    :: SEUIL
      DOUBLE PRECISION, INTENT(INOUT) :: TAUT
! Variables in TOMAWAC MODULE
!   XKAPPA          VON KARMAN CONSTANT
!   ALPHA           CHARNOCK CONSTANT
!   ZVENT    ELEVATION FOR WIND MEASUREMENTS
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      DOUBLE PRECISION TAUMIN, XNUAIR, AUX   , USTO  , TAUO   , TAUN
      DOUBLE PRECISION USTN  , X     , ZE    , DIFF  , FOLD   , DFOLD
!
      ITR   =0
      TAUMIN=1.D-5
      XNUAIR=1.D-5
!
!.....INITIAL VALUES
!     """"""""""""""""""
      USTO  =UVENT*SQRT(CDRAG)
      TAUO  =MAX(USTO**2,TAUW+TAUMIN)
  190 CONTINUE
      ITR   = ITR+1
!
!.....ITERATION BY THE METHOD OF NEWTON
!     """""""""""""""""""""""""""""""""""
      USTO  = SQRT(TAUO)
      X     = TAUW/TAUO
      IF(ABS(1.D0-X).LT.1.D-30) THEN
        WRITE(LU,*) 'IN ROUTINE TAUTOT I DIVIDE BY 1-X'
        WRITE(LU,*) 'AND X=',X,' WIND MODELE =1 ?'
        WRITE(LU,*) 'MAY BE YOU CAN TRY WITH MODELE=2 ?'
        CALL PLANTE(3)
        STOP
      ENDIF
      ZE    = MAX(0.1D0*XNUAIR/USTO,ALPHA*TAUO/(GRAVIT*SQRT(1.D0-X)))
      AUX   = LOG(ZVENT/ZE)
      FOLD  = USTO-XKAPPA*UVENT/AUX
      DFOLD = 1.D0-2.D0*XKAPPA*UVENT*(1.D0-1.5D0*X)/AUX**2/USTO/(1.D0-X)
      USTN  = USTO-FOLD/DFOLD
      TAUN  = MAX(USTN**2,TAUW+TAUMIN)
!
!.....CONVERGENCE CRITERIA
!     """"""""""""""""""""""""
      DIFF=ABS(TAUN-TAUO)/TAUO
      TAUO=TAUN
      IF (ITR.LT.ITRMIN) GOTO 190
      IF ((DIFF.GT.SEUIL).AND.(ITR.LT.ITRMAX)) GOTO 190
!
!.....APPLIES THE SOLUTION
!     """""""""""""""""""""""""""""""""""
      TAUT=TAUN
!
      RETURN
      END
