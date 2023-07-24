                   CHARACTER*144 FUNCTION CARLUMASC
C                  ********************************
C
     *( LCAR   , ICOL  , LIGNE  , EXTREM , MOTCLE , SIZE , MOTIGN ,
     *  LONIGN , NMAXR , NFICDA , LGVAR  )
C
C***********************************************************************
C DAMOCLES VERSION 5.0     16/08/94   J.M. HERVOUET (LNH)   30 87 80 18
C                                    A. YESSAYAN
C                        14/12/93    O. QUIQUEMPOIX (LNH)   30 87 78 70
C
C Copyright EDF 1994
C
C
C***********************************************************************
C
C FONCTION  : DECODE UNE CHAINE DE CARACTERES A PARTIR DE LA COLONNE
C             ICOL+1 DE LA LIGNE COURANTE. MAXIMUM LGA CARACTERES.
C             SI LA CHAINE N'EST PAS TERMINEE, RECHERCHE SUR LA LIGNE
C             SUIVANTE, SI BESOIN.
C             AVANCE LE POINTEUR ICOL SUR LE DERNIER CARACTERE DECODE
C             OU A ICOL=0 SI ON A LU INUTILEMENT LA LIGNE SUIVANTE
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !  LCAR          !<-- ! LONGUEUR DE LA CHAINE DE CARACTERES          !
C !  ICOL          !<-->! POSITION COURANTE DU POINTEUR DANS LA LIGNE  !
C !  LIGNE         !<-->! LIGNE EN COURS DE DECODAGE                   !
C !  EXTREM        ! -->! SEPARATEUR DE CHAINE = ' OU "                !
C !  MOTCLE        ! -->! TABLEAU DES MOTS CLES ACTIFS                 !
C !  SIZE          ! -->! TABLEAU DES LONGUEURS DES MOTS CLES          !
C !  MOTIGN        ! -->! TABLEAU DES MOTS CLES DUS A EDAMOX A IGNORER !
C !  LONIGN        ! -->! TABLEAU DES LONGUEURS DES MOTS EDAMOX        !
C !  NMAXR         ! -->! TABLEAU DES INDEX MAXIMUM REELS PAR TYPES    !
C !  NFICDA        ! -->! NUMERO DE CANAL DU FICHIER DES DONNEES       !
C !  LGVAR         ! -->! LONGUEUR MAXIMUM DE LA CHAINE A LIRE         !
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
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C     - PRECAUTIONS D'EMPLOI :    ON SUIT ICI LA CONVENTION FORTRAN :
C                                 '' EST LU COMME ETANT ' DANS UNE
C                                 CHAINE DE CARACTERES.
C                                 LES CHAINES SANS ' OU SANS " NE PEUVEN
C                                 CONTENIR DE SEPARATEURS.
C
C     - PORTABILITE :             IBM,CRAY,HP,SUN
C
C     - APPELE PAR :              DAMOC,INFLU
C
C     - FONCTIONS APPELEES :      NEXTMASC,PRECARMASC,LONGLUMASC
C
C***********************************************************************
C
      IMPLICIT NONE
C
C
      INTEGER       LCAR,ICOL,NMAXR(4),NFICDA,LGVAR,SIZE(4,*)
      INTEGER       LONIGN(100)
      CHARACTER*(*) LIGNE
      CHARACTER*1   EXTREM
      CHARACTER*72  MOTIGN(100),MOTCLE(4,*)
C
      INTEGER  NEXTMASC,PRECARMASC,LONGLUMASC
      EXTERNAL NEXTMASC,PRECARMASC,LONGLUMASC
C
      INTEGER       LNG,LU
      INTEGER       NLIGN,LONGLI
      INTEGER       NFIC
      LOGICAL       ERREUR , RETOUR
C
C-----------------------------------------------------------------------
C
      INTEGER       I,IDEB,IFIN,NCAR,ICOL2,NLIGN2,ITYP,K,LGLU,LONPRO(15)
      INTEGER       QCAS
      LOGICAL       COTE,LISUIV,LUFIC,LUCOTE
      CHARACTER*1   QUOTE,TABUL
      CHARACTER*9   MOTPRO(15)
      CHARACTER*72  LIGNE2
      CHARACTER*144 LIGNED
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
C-----------------------------------------------------------------------
C
      DATA MOTPRO/'NOM','TYPE','INDEX','TAILLE','DEFAUT','AIDE',
     * 'CHOIX','RUBRIQUE','NIVEAU','MNEMO','COMPOSE','COMPORT',
     * 'CONTROLE','APPARENCE','SUBMIT'/
C LONGUEUR DES MOTS PROTEGES
      DATA LONPRO /3,4,5,6,6,4,5,8,6,5,7,7,8,9,6/
C
C***********************************************************************
C                                    MARQUAGE RCS ET SCCS
C
C***********************************************************************
C
      COTE   = .FALSE.
      LISUIV = .FALSE.
      LUFIC  = .FALSE.
      LUCOTE = .FALSE.
      LCAR   = 1
      CARLUMASC  = ' '
      QUOTE  = ''''
      TABUL  = CHAR(9)
      NLIGN2 = NLIGN
      ICOL2  = ICOL
      LIGNE2 = LIGNE
      LIGNED = ' '
      LGLU   = 0
      QCAS   = 0
C
      ICOL   = NEXTMASC( ICOL+1 , LIGNE )
C
C        //// CALCUL DES EXTREMITES DE LA CHAINE ////
C
C    NOTE : LA CHAINE PEUT ETRE PLACEE ENTRE COTES OU SANS COTES
C           SI ELLE N'EST PAS ENTRE COTES, ELLE NE PEUT CONTENIR
C           DE CARACTERE BLANC.
C
      IF ( LIGNE(ICOL:ICOL).NE.EXTREM ) THEN
           IDEB = ICOL
C                 PRECARMASC : MEME ROLE QUE PREVAL, MAIS NE SAUTE PAS
C                          LES ZONES COMMENTAIRES.
           ICOL = PRECARMASC ( ICOL+1 , LIGNE , ' ' , ';' , TABUL) - 1
           IFIN = ICOL
           LIGNED = LIGNE(IDEB:IFIN)
           LGLU = IFIN-IDEB+1
C
C FICHIER CAS : SI ON ARRIVE EN BOUT DE LIGNE, VOIR LA SUIVANTE
C
290        IF (IFIN.GE.LONGLI) THEN
             LISUIV = .TRUE.
             LUFIC = .TRUE.
             READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
             ICOL2 = 0
             IF (LIGNE2(1:1).EQ.'&'.OR.
     *           LIGNE2(1:1).EQ.'='.OR.LIGNE2(1:1).EQ.':'.OR.
     *           LIGNE2(1:1).EQ.';'.OR.LIGNE2(1:1).EQ.'/' ) THEN
                LISUIV = .FALSE.
                GO TO 96
             ENDIF
C
C REGARDER SI C'EST UN MOT CLE CONNU POUR LE FICHIER CAS
C
            IF (NFIC.EQ.NFICDA) THEN
             DO 300 ITYP = 1,4
              DO 310 I=1,NMAXR(ITYP)
C                K=LONGLUMASC(MOTCLE(ITYP,I))
                 K=SIZE(ITYP,I)
                 IF (K.GT.0.AND.LIGNE2(1:K).EQ.MOTCLE(ITYP,I)(1:K)) THEN
                    LISUIV = .FALSE.
                    GO TO 96
                 ENDIF
 310          CONTINUE
 300         CONTINUE
             DO 320 I=1,100
C               K = LONGLUMASC(MOTIGN(I))
                K = LONIGN(I)
                IF(K.GT.0.AND.LIGNE2(1:K).EQ.MOTIGN(I)(1:K)) THEN
                  LISUIV = .FALSE.
                  GO TO 96
                ENDIF
 320         CONTINUE
            ELSE
             DO 330 I=1,15
C               K = LONGLUMASC(MOTPRO(I))
                K = LONPRO(I)
                IF(K.GT.0.AND.LIGNE2(1:K).EQ.MOTPRO(I)(1:K)) THEN
                  LISUIV = .FALSE.
                  GO TO 96
                ENDIF
 330         CONTINUE
            ENDIF
C
C SI ON ARRIVE ICI, ON DOIT DONC UTILISER LA LIGNE SUIVANTE
C
        ICOL2 =PRECARMASC (1 , LIGNE2 , ' ' , TABUL ,' ') - 1
C
        LGLU = LGLU + ICOL2
C
        IF (LGLU.GT.LGVAR) THEN
             ERREUR = .TRUE.
             IF (LONGLUMASC(LIGNED).GT.0) THEN
               LIGNED = LIGNED(1:LONGLUMASC(LIGNED))//LIGNE2(1:ICOL2)
             ELSE
               LIGNED = LIGNE2(1:ICOL2)
             ENDIF
             IF (LGLU.GT.0) WRITE(LU,'(1X,A)') LIGNED(1:LGLU)
             WRITE(LU,*) ' '
             IF (LNG.EQ.1) THEN
               WRITE(LU,'(1X,A6,I4,1X,A27)') 'LIGNE: ',NLIGN,
     *                'ERREUR : CHAINE TROP LONGUE'
             ELSEIF (LNG.EQ.2) THEN
               WRITE(LU,'(1X,A5,I4,1X,A23)') 'LINE: ',NLIGN,
     *                'ERROR : STRING TOO LONG'
             ENDIF
             ICOL = ICOL -1
             GO TO 1000
         ELSE
C Il FAUT LIRE ENCORE UNE AUTRE LIGNE - ON SIMULE UN DECALAGE DE LIGNE
             LISUIV = .FALSE.
             LIGNE = LIGNE2
             IF (LONGLUMASC(LIGNED).GT.0) THEN
               LIGNED = LIGNED(1:LONGLUMASC(LIGNED))//LIGNE2(1:LONGLI)
             ELSE
              LIGNED = LIGNE2(1:LONGLI)
             ENDIF
             NLIGN = NLIGN2
             ICOL = ICOL2
             IFIN = LONGLI+1
             GO TO 290
        ENDIF
  96    IF (LISUIV) THEN
            IF (LONGLUMASC(LIGNED).GT.0) THEN
               LIGNED = LIGNED(1:LONGLUMASC(LIGNED))//LIGNE2(1:ICOL2)
             ELSE
              LIGNED = LIGNE2(1:LONGLI)
             ENDIF
            IFIN = LGLU+ICOL2
            IDEB = 1
        ENDIF
       ENDIF
C
           GO TO 901
 900       CONTINUE
           RETOUR = .TRUE.
 901       CONTINUE
           DO 90 I = 1 , LGLU
             IF (LIGNED(I:I).EQ.QUOTE.OR.LIGNED(I:I).EQ.'&'.OR.
     *          LIGNED(I:I).EQ.'='.OR.LIGNED(I:I).EQ.':'.OR.
     *          LIGNED(I:I).EQ.'/') THEN
                IF (NLIGN2.NE.NLIGN.AND.(.NOT.(LUFIC)))
     *                 WRITE(LU,'(1X,A)') LIGNE2(1:LONGLI)
                IF (LGLU.GT.0) WRITE(LU,'(1X,A)') LIGNED(1:LGLU)
              IF(LNG.EQ.1) THEN
                WRITE(LU,'(1X,A6,I4,A45,A)') 'LIGNE: ',NLIGN,
     *         ' ERREUR : CARACTERE INTERDIT DANS UNE CHAINE ',
     *         'SANS APOSTROPHES'
              ENDIF
              IF(LNG.EQ.2) THEN
                  WRITE(LU,'(1X,A5,I4,A)') 'LINE: ',NLIGN,
     *         ' ERROR: UNEXPECTED CHARACTER IN A STRING WITHOUT QUOTES'
              ENDIF
              ERREUR = .TRUE.
              GO TO 1000
            ENDIF
90         CONTINUE
C
      ELSE
C
C CAS DE LA LECTURE AVEC DES QUOTES
C
           IDEB = ICOL + 1
C
C LA PREMIERE QUOTE EST EN DERNIERE COLONNE (QCAS=4 OU QCAS=5)
           IF (ICOL.EQ.LONGLI) QCAS=45
C
 100       ICOL=PRECARMASC(ICOL+1,LIGNE,EXTREM,EXTREM,EXTREM)
           IF (ICOL.EQ.LONGLI) ICOL = LONGLI+1
C
C CAS DES DOUBLES QUOTES DANS LA PREMIERE LIGNE SAUF EN COLONNE 72
C
           IF (ICOL.LT.LONGLI.AND.LIGNE(ICOL+1:ICOL+1).EQ.EXTREM.AND.
     *         EXTREM.EQ.QUOTE) THEN
              ICOL = ICOL + 1
C LA QUOTE EN 72 EST LA DEUXIEME QUOTE D'UNE DOUBLE (QCAS=3)
              IF (ICOL.EQ.LONGLI) QCAS=3
              COTE = .TRUE.
              GO TO 100
           ENDIF
C
           LGLU = MAX(0,ICOL-IDEB)
           IF (LGLU.GT.0) LIGNED = LIGNE(IDEB:ICOL-1)
C
C SI ON N'A PAS TROUVE LA FIN OU SI UNE QUOTE EST EN COLONNE 72
C
           IF (ICOL.GT.LONGLI) THEN
390             LISUIV = .TRUE.
                LUFIC = .TRUE.
                READ(NFIC,END=905,ERR=998,FMT='(A)') LIGNE2
C
C CAS OU LA LIGNE PRECEDENTE SE TERMINE PAR UNE QUOTE
C
                IF (LIGNE(LONGLI:LONGLI).EQ.QUOTE) THEN
C LA QUOTE DE LA COLONNE 72 OUVRE LA CHAINE OU EST LA 2EME D'UNE DOUBLE
                  IF (QCAS.EQ.45.OR.QCAS.EQ.3) THEN
                      QCAS=0
                  ELSEIF (LIGNE2(1:1).EQ.QUOTE) THEN
                    COTE = .TRUE.
                    LUCOTE = .TRUE.
                    QCAS=0
                 ELSE
                    LGLU=LGLU-1
                    IF (LGLU.GT.0) LIGNED = LIGNED(1:LGLU)
                    LISUIV = .FALSE.
                    QCAS=0
                    GO TO 920
                  ENDIF
                ENDIF
C
                ICOL2 = 0
                IF (LIGNE2(1:1).EQ.QUOTE.AND.LUCOTE) THEN
                   LUCOTE = .FALSE.
                   ICOL2=1
                ENDIF
 110            ICOL2 =PRECARMASC (ICOL2+1,LIGNE2,EXTREM,EXTREM,EXTREM)
                IF (ICOL2.LT.LONGLI.AND.LIGNE2(ICOL2+1:ICOL2+1).EQ.
     *             EXTREM.AND.EXTREM.EQ.QUOTE) THEN
C                   ICOL2 = PRECARMASC(ICOL2+1,LIGNE2,EXTREM,EXTREM,EXTREM)
                    ICOL2=ICOL2+1
                    COTE=.TRUE.
                    IF (ICOL2.EQ.LONGLI) QCAS=3
                   GO TO 110
                ENDIF
                IF (ICOL2.EQ.LONGLI) ICOL2=ICOL2+1
                IF (LGLU.GT.0) THEN
                  LIGNED = LIGNED(1:LGLU)//LIGNE2(1:ICOL2-1)
                ELSE
                  LIGNED = LIGNE2(1:ICOL2-1)
                ENDIF
                LGLU = LGLU + ICOL2-1
C
                IF (LGLU.GT.LGVAR) GO TO 910
C
C REGARDER LA LIGNE SUIVANTE SI PAS FINI OU SI QUOTE EN 72
C
                IF (ICOL2.GE.LONGLI) THEN
                  LISUIV = .FALSE.
                  LIGNE = LIGNE2
                  NLIGN = NLIGN2
                  ICOL = ICOL2
                  IFIN = ICOL2
                  GO TO 390
                ENDIF
C LA C'EST BON
                GO TO 920
C
 905            CONTINUE
                RETOUR = .TRUE.
C
 910            CONTINUE
                WRITE(LU,'(1X,A)') LIGNED(1:MAX(1,LGLU))
                WRITE(LU,*)
                IF(LNG.EQ.1) THEN
                WRITE(LU,'(1X,A6,I4,A)') 'LIGNE: ',NLIGN,
     *         ' ERREUR : COTE MANQUANTE EN FIN DE CHAINE DE CARACTERES'
                WRITE(LU,*)'OU CHAINE TROP LONGUE ... '
                ENDIF
                IF(LNG.EQ.2) THEN
                WRITE(LU,'(1X,A5,I4,A)') 'LINE: ',NLIGN,
     *         ' ERROR: QUOTE MISSING AT THE END OF THE STRING'
                WRITE(LU,*)'OR STRING TOO LONG ... '
                ENDIF
                ERREUR = .TRUE.
                ICOL = LONGLI
                GO TO 1000
C
            ENDIF
           IFIN   = ICOL - 1
      ENDIF
C
 920  CONTINUE
      IF ( LGLU.NE.0  ) THEN
           LCAR = MIN(LGLU,LGVAR)
           CARLUMASC = LIGNED(1:LGLU)
      ENDIF
C
C  REMPLACEMENT DES DOUBLES COTES PAR DES SIMPLES COTES
C
      IF(COTE) THEN
         NCAR = LCAR
         I = 1
 200     CONTINUE
         IF(I.GT.NCAR) THEN
            LCAR = NCAR
            GO TO 1000
         ENDIF
         IF(CARLUMASC(I:I).EQ.QUOTE.AND.
     & CARLUMASC(I+1:I+1).EQ.QUOTE) THEN
            CARLUMASC(I+1:LCAR) = CARLUMASC(I+2:LCAR)//' '
            NCAR = NCAR - 1
         ENDIF
         I = I + 1
         GO TO 200
      ENDIF
C
1000  CONTINUE
C
      IF (LUFIC) THEN
        NLIGN = NLIGN + 1
        LIGNE = LIGNE2
        IF (LISUIV) THEN
          ICOL = ICOL2
        ELSE
          ICOL = 0
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
C
998   CONTINUE
      IF(LNG.EQ.1) WRITE(6,999) NFIC,NLIGN+1
      IF(LNG.EQ.2) WRITE(6,1999) NFIC,NLIGN+1
999   FORMAT(1X,'UNITE LOGIQUE ',1I2,'   ERREUR LIGNE ',1I6)
1999  FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR LINE ',1I6)
      RETOUR = .TRUE.
      RETURN
C
      END
