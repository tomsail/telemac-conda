                        INTEGER FUNCTION NEXTMASC
C                       *************************
C
     *( ICOL , LIGNE )
C
C***********************************************************************
C
C DAMOCLES VERSION 5.0     16/08/94   J.M. HERVOUET (LNH)   30 87 80 18
C                                       A. YESSAYAN
C                          15/12/93     O. QUIQUEMPOIX (LNH) 30 87 78 70
C
C Copyright EDF 1994
C
C
C***********************************************************************
C
C FONCTION  : RETOURNE L' INDICE DU PREMIER CARACTERE NON BLANC, NON
C             TABULATION ET NON COMMENTAIRE QUE L'ON TROUVE A PARTIR DE
C             LA COLONNE ICOL DE LA LIGNE.
C             SI ON N'EN TROUVE PAS ON CHERCHE SUR LA LIGNE SUIVANTE
C             SI ON N'EN TROUVE VRAIMENT PAS NEXTMASC = LONGLI + 1
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !  ICOL          !<-->! POSITION COURANTE DU POINTEUR DANS LA LIGNE  !
C !  LIGNE         !<-->! LIGNE EN COURS DE DECODAGE                   !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !   /COMMON/     !    !                                              !
C !                !    !                                              !
C !    DCINFO      !    !                                              !
C !  . LNG         ! -->! NUMERO DE LA LANGUE DE DECODAGE              !
C !  . LU          ! -->! NUMERO DE L'UNITE LOGIQUE DES SORTIES        !
C !                !    !                                              !
C !    DCMLIG      !    !                                              !
C !  . NLIGN       !<-->! NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU!
C !  . LONGLI      ! -->! LONGUEUR DES LIGNES                          !
C !                !    !                                              !
C !    DCRARE      !    !                                              !
C !  . ERREUR      !<-->! SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR   !
C !  . RETOUR      !<-->! SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE  !
C !                !    ! FIN DE FICHIER OU D'ERREUR DE LECTURE.       !
C !                !    !                                              !
C !    DCCHIE      !    !                                              !
C !  . NFIC        ! -->! NUMERO DE CANAL DU FICHIER EN COURS DE LECT. !
C !________________!____!______________________________________________!
C
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C     - PORTABILITE :          IBM,CRAY,HP,SUN
C
C     - APPELE PAR :           DAMOC,INTLU,LOGLU,REALU,CARLU
C
C***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER       ICOL
      CHARACTER*(*) LIGNE*(*)
C
      INTEGER       LNG,LU
      INTEGER       NLIGN,LONGLI
      INTEGER       NFIC
      LOGICAL       ERREUR,RETOUR
C
C-----------------------------------------------------------------------
C
      INTEGER       I,J
      CHARACTER*1   TABUL
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR,RETOUR
      COMMON / DCMLIG / NLIGN,LONGLI
      COMMON / DCCHIE / NFIC
C
      INTRINSIC CHAR
C
C***********************************************************************
C                                    MARQUAGE RCS ET SCCS
C
C***********************************************************************
C
      TABUL = CHAR(9)
      NEXTMASC  = LONGLI + 1
      I     = ICOL -1
C
100   CONTINUE
      I = I + 1
C
      IF(I.GT.LONGLI) THEN
99        NLIGN = NLIGN + 1
          READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE
C         UNE LIGNE QUI COMMENCE PAR UN / N'EST PAS TRAITEE
          IF(LIGNE(1:1).EQ.'/') GO TO 99
          I = 0
          GO TO 100
      ENDIF
C
C
C CAS DU BLANC OU TABULATION
      IF (LIGNE(I:I).EQ.' '.OR.LIGNE(I:I).EQ.TABUL) GOTO 100
C
C-----------------------------------------------------------------------
C
C          ELIMINATION DES COMMENTAIRES :
C
C          IF ( LIGNE(I:I).NE.'/'.OR.I.GE.LONGLI ) THEN
           IF (LIGNE(I:I).NE.'/') THEN
                NEXTMASC = I
                GO TO 1000
           ELSE
                DO 110 J = I+1 , LONGLI
                     IF ( LIGNE(J:J).EQ.'/' ) THEN
                          I = J
                          GO TO 100
                     ENDIF
110             CONTINUE
                I = LONGLI
                GO TO 100
            ENDIF
C
C-----------------------------------------------------------------------
C
998   CONTINUE
      IF(LNG.EQ.1) WRITE(6,999) NFIC,NLIGN
      IF(LNG.EQ.2) WRITE(6,1999) NFIC,NLIGN
999   FORMAT(1X,'UNITE LOGIQUE ',1I2,'   ERREUR LIGNE ',1I6)
1999  FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR LINE ',1I6)
900   CONTINUE
      RETOUR = .TRUE.
C
1000  CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
