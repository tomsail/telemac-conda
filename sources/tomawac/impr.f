!                   ***************
                    SUBROUTINE IMPR
!                   ***************
!
     &(LISPRD,LT,AT,ISITS,ICOD)
!
!***********************************************************************
! TOMAWAC   V6P1                                   20/06/2011
!***********************************************************************
!
!brief    WRITES OUT TO THE LISTING.
!
!history  F.MARCOS (LNH)
!+        01/02/95
!+
!+
!
!history  JMH
!+        08/06/2010
!+        V6P0
!+   PRINT REPLACED BY WRITE
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
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| ICOD           |-->| CODE FOR OUTPUTS
!| ISITS          |-->| NUMBER OF ITERATIONS FOR THE SOURCE TERMS
!| LISPRD         |-->| PERIOD FOR LISTING PRINTOUTS
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_IMPR => IMPR

      IMPLICIT NONE
!
      INTEGER,INTENT(IN)           :: LT,ICOD,LISPRD,ISITS
      DOUBLE PRECISION,INTENT(IN)  :: AT
!
      INTEGER LTT
      LOGICAL IMP
      CHARACTER(LEN=45) :: TEXIMP(9)
!
      PARAMETER ( TEXIMP = (/
     &    'CALCUL DU CHAMP CONVECTEUR ET REMONTEE DES   ' ,
     &    '    CARACTERISTIQUES                         ' ,
     &    'SAUVEGARDE DE L''ETAT FINAL A T=              ' ,
     &    'INTERPOLATION AUX PIEDS DES CARACTERISTIQUES ' ,
     &    'TEMPS :                                      ' ,
     &    ' SECONDES                                    ' ,
     &    'IEME  ITERATION                              ' ,
     &    ' SOUS-ITERATION(S)                           ' ,
     &    'PRISE EN COMPTE DES TERMES SOURCES EN        '  /) )
!
!-----------------------------------------------------------------------
!
      IMP=.FALSE.
      LTT=(LT/LISPRD)*LISPRD
      IF(LT.EQ.LTT) IMP=.TRUE.
!
      IF (.NOT.IMP) RETURN
!
      IF (ICOD.EQ.1) THEN
        WRITE(LU,101) TEXIMP(1)
      ENDIF
!
      IF (ICOD.EQ.2) THEN
        WRITE(LU,102) TEXIMP(2)
      ENDIF
!
      IF (ICOD.EQ.3) THEN
        WRITE(LU,103) TEXIMP(5),AT,TEXIMP(6),LT,TEXIMP(7)
      ENDIF
!
      IF (ICOD.EQ.4) THEN
        WRITE(LU,104) TEXIMP(9),ISITS,TEXIMP(8)
      ENDIF
!
      IF (ICOD.EQ.5) THEN
        WRITE(LU,105) TEXIMP(4)
      ENDIF
!
      IF (ICOD.EQ.6) THEN
        WRITE(LU,106) TEXIMP(3),AT,TEXIMP(6)
      ENDIF
!
!-----------------------------------------------------------------------
!
101   FORMAT(80('-'),/,7X,A44)
102   FORMAT(7X,A44)
103   FORMAT(/,80('='),/,7X,A8,F12.4,A10,23X,I5,A17,/,80('-'))
104   FORMAT(7X,A37,I3,A18)
105   FORMAT(7X,A44,/,80('-'))
106   FORMAT(80('-'),/,7X,A32,F12.4,A10,/,/,80('='))
!
!-----------------------------------------------------------------------
!
      RETURN
      END
