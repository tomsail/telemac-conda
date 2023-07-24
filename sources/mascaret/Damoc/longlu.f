                        INTEGER FUNCTION LONGLUMASC
C                       ***************************
C
     *( LIGNE )
C
C***********************************************************************
C DAMOCLES VERSION 5.0     16/08/94   J.M. HERVOUET (LNH)   30 87 80 18
C                                       A. YESSAYAN
C                                       L. LEGUE
C                          15/12/93     O. QUIQUEMPOIX (LNH) 30 87 78 70
C
C Copyright EDF 1994
C
C
C***********************************************************************
C
C FONCTION: RENVOIE LA POSITION DU DERNIER CARACTERE NON BLANC ET NON
C           TABULATION DE L'ARGUMENT LIGNE
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !  LIGNE         ! -->! ARGUMENT A ANALYSER                          !
C !________________!____!______________________________________________!
C
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C     - PORTABILITE : IBM,CRAY,HP,SUN
C
C     - APPELE PAR :  CARLU,CMD,DAMOC,DICO,INFLU,MAJUS
C
C***********************************************************************
C
      IMPLICIT NONE
C
      CHARACTER*(*) LIGNE
C
C-----------------------------------------------------------------------
C
      INTEGER       I,LONG
      CHARACTER*1   TABUL
      INTRINSIC CHAR
C
C***********************************************************************
C                                    MARQUAGE RCS ET SCCS
C
C***********************************************************************
C
      TABUL = CHAR(9)
      LONG = LEN(LIGNE)
      IF (LONG .EQ. 0) THEN
        I = 0
        GO TO 110
      ENDIF
      DO 100 I = LONG , 1 , -1
      IF (LIGNE(I:I).NE.' '.AND.LIGNE(I:I).NE.TABUL) GO TO 110
100   CONTINUE
110   CONTINUE
      LONGLUMASC = I
C
C-----------------------------------------------------------------------
C
      RETURN
      END
