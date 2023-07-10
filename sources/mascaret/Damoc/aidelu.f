                        SUBROUTINE AIDELUMASC
C                       *********************
C
     *( ICOL , LIGNE, DOC )
C
C***********************************************************************
C DAMOCLES VERSION 5.0     16/08/94   J.M. HERVOUET (LNH)   30 87 80 18
C                                       A. YESSAYAN
C                                       L. LEGUE
C
C Copyright EDF 1994
C
C
C***********************************************************************
C
C FONCTION  : DECODE UNE CHAINE DE CARACTERES A PARTIR DE LA COLONNE
C             ICOL+1 DE LA LIGNE. MAXIMUM 80 CARACTERES PAR LIGNE.
C             CETTE CHAINE PEUT ETRE SUR PLUSIEURS LIGNES.
C             AIDELU, NE SERT A DECODER QUE LE CHAMP AIDE DU
C             DICTIONNAIRE ET LES MOTS IGNORES POUR EDAMOX.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !  ICOL          !<-->! INDICE DU CARACTERE COURANT DANS LA LIGNE    !
C !  LIGNE         !<-->! LIGNE EN COURS DE DECODAGE.                  !
C !  DOC           ! -->! LOGIQUE DE DOCUMENTATION DE LA SORTIE        !
C !                !    ! = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)   !
C !                !    ! = FAUX : N'IMPRIME PAS L'AIDE                !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !   /COMMON/     !    !                                              !
C !                !    !                                              !
C !    DCINFO      !    !                                              !
C !  . LNG         ! -->! NUMERO DE LA LANGUE DE DECODAGE              !
C !  . LU          ! -->! NUMERO DE L'UNITE LOGIQUE DES SORTIES        !
C !                !    !                                              !
C !    DCRARE      !    !                                              !
C !  . ERREUR      !<-- ! SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR   !
C !  . RETOUR      !<-- ! SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE  !
C !                !    ! FIN DE FICHIER OU D'ERREUR DE LECTURE.       !
C !                !    !                                              !
C !    DCMLIG      !    !                                              !
C !  . NLIGN       !<-->! NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU!
C !  . LONGLI      ! -->! LONGUEUR DES LIGNES                          !
C !                !    !                                              !
C !    DCCHIE      !    !                                              !
C !  . NFIC        ! -->! NUMERO DE CANAL DU FICHIER EN COURS DE LECT. !
C !________________!____!______________________________________________!
C
C-----------------------------------------------------------------------
C
C     - PRECAUTIONS D'EMPLOI :    ON SUIT ICI LA CONVENTION FORTRAN : ''
C                                 EST COMME ETANT ' DANS UNE CHAINE DE
C                                 CARACTERES SI CETTE CHAINE EST ECRITE
C                                 ENTRE COTES
C                                 ATTENTION, LES COTES PLACEES EN DEBUT
C                                 ET EN FIN DE LIGNES SONT SOURCES
C                                 POSSIBLES D'ERREURS
C
C     - PORTABILITE :             IBM,CRAY,HP,SUN
C
C     - APPELE PAR :              DAMOC
C
C     - FONCTIONS APPELEES :      NEXTMASC,PRECARMASC
C
C***********************************************************************
C
      IMPLICIT NONE
C
C
      INTEGER       ICOL
      LOGICAL       DOC
      CHARACTER*(*) LIGNE
C
      INTEGER  NEXTMASC,PRECARMASC
      EXTERNAL NEXTMASC,PRECARMASC
C
      INTEGER       LNG,LU
      INTEGER       NLIGN,LONGLI
      INTEGER       NFIC
      LOGICAL       ERREUR,RETOUR
C
C-----------------------------------------------------------------------
C
      INTEGER       IDEB,IFIN,JCOL
      CHARACTER*1   QUOTE,TABUL,PTVIRG
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR,RETOUR
      COMMON / DCMLIG / NLIGN,LONGLI
      COMMON / DCCHIE / NFIC
C
      INTRINSIC CHAR
c
C***********************************************************************
C                                   MARQUAGE RCS ET SCCS
C
C***********************************************************************
C
      QUOTE  = ''''
      PTVIRG = ';'
      TABUL =CHAR(9)
9     ICOL   = NEXTMASC( ICOL+1 , LIGNE )
C
C        //// CALCUL DES EXTREMITES DE LA CHAINE ////
C
C    NOTE : LA CHAINE PEUT ETRE PLACEE ENTRE COTES OU SANS COTES
C           SI ELLE N'EST PAS ENTRE COTES, ELLE NE PEUT CONTENIR
C           DE CARACTERE BLANC.
C
      IF ( LIGNE(ICOL:ICOL).NE.QUOTE ) THEN
           IDEB = ICOL
C                 PRECARMASC : MEME ROLE QUE PREVAL, MAIS NE SAUTE PAS
C                          LES ZONES COMMENTAIRES
           ICOL = PRECARMASC (ICOL+1,LIGNE,' ',PTVIRG,TABUL) - 1
           IFIN = ICOL
           IF (DOC) WRITE(LU,10) LIGNE(IDEB:IFIN)
10         FORMAT(1X,A)
      ELSE
C
C SI LA CHAINE EST ENTRE QUOTES
C
         IDEB = ICOL + 1
C
C TANT QU'IL N'Y A PAS DE QUOTE SUR LA LIGNE
C
100      ICOL = PRECARMASC(ICOL+1,LIGNE,QUOTE,QUOTE,QUOTE)
         IF (ICOL.GT.LONGLI) THEN
C         PAS DE COTE SUR LA LIGNE, ON L'IMPRIME ET ON PASSE A LA SUITE
          IF (DOC) WRITE(LU,10) LIGNE(IDEB:LONGLI)
C         ON LIT LA LIGNE SUIVANTE.
          READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE
          NLIGN = NLIGN + 1
          ICOL = 1
          IDEB = 1
          GO TO 100
         ELSEIF(ICOL.EQ.LONGLI) THEN
C         COTE EN BOUT DE LIGNE, ON IMPRIME LA LIGNE SAUF LA COTE
C         ET C'EST TOUT.
          IF (DOC) WRITE(LU,10) LIGNE(IDEB:ICOL-1)
         ELSE
C         QUOTE SUIVANTE
          JCOL = PRECARMASC(ICOL+1,LIGNE,QUOTE,QUOTE,QUOTE)
C         S'IL Y A UNE DOUBLE QUOTE, ON L'ENLEVE
          IF ((JCOL-ICOL).EQ.1) THEN
            ICOL=JCOL
            LIGNE(JCOL:LONGLI)=LIGNE(JCOL+1:LONGLI) // ' '
            GO TO 100
          ELSE
C           ON IMPRIME L'AIDE LUE EN ENLEVANT LA DERNIERE QUOTE
            IF (DOC) WRITE(LU,10) LIGNE(IDEB:ICOL-1)
          ENDIF

         ENDIF
      ENDIF
      ICOL = NEXTMASC(ICOL+1,LIGNE)
      IF(LIGNE(ICOL:ICOL).EQ.PTVIRG(1:1)) THEN
        GO TO 9
      ENDIF
      GO TO 1000
C
C IMPRESSION DES ERREURS
C
998   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,999) NFIC, NLIGN
999     FORMAT(1X,'UNITE LOGIQUE ',1I2,'   ERREUR LIGNE ',1I6)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,1999) NFIC, NLIGN
1999    FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR ON LINE ',1I6)
      ENDIF
900   CONTINUE
      RETOUR = .TRUE.
C
C FIN DE L'IMPRESSION DES ERREURS
C
1000  CONTINUE
C
C DEUX LIGNES SAUTEES POUR LA MISE EN PAGE
C
      IF (DOC) WRITE(LU,*) ' '
      IF (DOC) WRITE(LU,*) ' '
C
C-----------------------------------------------------------------------
C
      RETURN
      END
