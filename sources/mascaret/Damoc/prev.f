                        INTEGER FUNCTION PREVMASC
C                       *************************
C
     *( ICOL , LIGNE )
C
C***********************************************************************
C DAMOCLES VERSION 5.0    16/08/94   J.M. HERVOUET (LNH)   30 87 80 18
C                                      A. YESSAYAN
C                          15/12/93    O. QUIQUEMPOIX (LNH)  30 87 78 70
C
C Copyright EDF 1994
C
C
C***********************************************************************
C
C FONCTION  : RETOURNE L' INDICE DU PREMIER CARACTERE NON BLANC, NON
C             TABULATION ET NON COMMENTAIRE QUE L'ON TROUVE AVANT LA
C             COLONNE ICOL DE LA LIGNE. LA COLONNE ICOL EST EXCLUE.
C             SI ON N'EN TROUVE PAS PREVMASC = ICOL
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !  ICOL          ! -->! POSITION COURANTE DU POINTEUR DANS LA LIGNE  !
C !  LIGNE         ! -->! LIGNE EN COURS DE DECODAGE                   !
C !________________!____!______________________________________________!
C
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C     - APPELE PAR : DAMOC
C
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER       ICOL
      CHARACTER*(*) LIGNE
C
C-----------------------------------------------------------------------
C
      INTEGER       I,J
      CHARACTER*1   TABUL
      INTRINSIC CHAR
C
C***********************************************************************
C                                    MARQUAGE RCS ET SCCS
C
C***********************************************************************
C
      TABUL = CHAR(9)
      PREVMASC   = ICOL
      I      = ICOL
C
  100 CONTINUE
      I = I - 1
      IF ( I.LT.1 ) GO TO 1000
C
      IF (LIGNE(I:I).EQ.' '.OR.LIGNE(I:I).EQ.TABUL) GOTO 100
C
C-----------------------------------------------------------------------
C          ELIMINATION DES COMMENTAIRES :
C
           IF ( LIGNE(I:I).NE.'/' ) THEN
                PREVMASC = I
                GO TO 1000
           ELSE
                IF ( I.LE.1 ) GO TO 1000
                DO 110 J = I-1 , 1 , -1
                     IF ( LIGNE(J:J).EQ.'/' ) THEN
                          I = J
                          GO TO 100
                     ENDIF
  110           CONTINUE
           ENDIF
C-----------------------------------------------------------------------
C
 1000 CONTINUE
C
      RETURN
      END
