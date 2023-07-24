                        LOGICAL FUNCTION LOGLUMASC
C                       **************************
C
     *( ICOL , LIGNE )
C
C***********************************************************************
C DAMOCLES VERSION 5.0     16/08/94   J.M. HERVOUET (LNH)   30 87 80 18
C                                      A. YESSAYAN
C                          15/12/93    O. QUIQUEMPOIX (LNH)  30 87 78 70
C
C Copyright EDF 1994
C
C
C***********************************************************************
C
C FONCTION  : LIT UNE VALEUR LOGIQUE QUE L'ON A REPERE DANS LA LIGNE
C             ET QUI COMMENCE A LA COLONNE ICOL+1.
C             SI LA CHAINE N'EST PAS TERMINEE, RECHERCHE SUR LA LIGNE
C             SUIVANTE.
C             AVANCE LE POINTEUR ICOL SUR LE DERNIER CARACTERE DECODE.
C             OU A ICOL=0 SI ON A LU LA LIGNE SUIVANTE
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
C     - PRECAUTIONS D'EMPLOI :    LES NOTATIONS ACCEPTEES SONT
C                                 VRAI OUI TRUE  YES .TRUE.  1
C                                 FAUX NON FALSE NO  .FALSE. 0
C                                 (EN MAJUSMASCCULES OU MINUSCULES)
C
C     - PORTABILITE :             IBM,CRAY,HP,SUN
C
C     - APPELE PAR :              DAMOC
C
C     - SOUS PROGRAMMES APPELES : MAJUSMASC
C
C     - FONCTIONS APPELEES :      NEXTMASC,PRECARMASC
C
C***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER       ICOL
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
      INTEGER       I1,I2
      CHARACTER*1   TABUL
      CHARACTER*7   L
      CHARACTER*72  LIGNE2
      LOGICAL       LUFIC,LISUIV
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR , RETOUR
      COMMON / DCMLIG / NLIGN , LONGLI
      COMMON / DCCHIE / NFIC
C
      INTRINSIC CHAR
C
C***********************************************************************
C                                    MARQUAGE RCS ET SCCS
C
C***********************************************************************
C
      LUFIC  = .FALSE.
      LISUIV = .FALSE.
      LIGNE2 = ' '
      TABUL  = CHAR(9)
C
      I1     = NEXTMASC( ICOL+1 , LIGNE )
      L(1:7) = LIGNE(I1:I1+6)
      I2 = PRECARMASC(I1,LIGNE,' ',';',TABUL)
C
C CAS OU ON POURRAIT AVOIR A LIRE LA LIGNE SUIVANTE
C
      IF (I2.GT.LONGLI.AND.(I1+6).GT.LONGLI) THEN
         LUFIC=.TRUE.
         READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
         IF (I1.LE.LONGLI) THEN
           L(1:7)=LIGNE(I1:LONGLI)//LIGNE2(1:(7-(LONGLI-I1+1)))
         ELSE
           L(1:7)=LIGNE2(1:7)
         ENDIF
         I2 = 0
         I2 = PRECARMASC(I2+1,LIGNE2,' ',';',TABUL)
      ENDIF
      CALL MAJUSMASC(L)
      GO TO 910
C
 900  CONTINUE
      RETOUR = .TRUE.
C
 910  CONTINUE
C
C ORDONNER DANS L'ORDRE LE PLUS PROBABLE : SOUVENT NON OUI NO YES 0 1 ..
C
      IF (L(1:3).EQ.'NON') THEN
            LOGLUMASC = .FALSE.
            ICOL = I1 + 2
      ELSE IF (L(1:2).EQ.'NO') THEN
            LOGLUMASC = .FALSE.
            ICOL = I1 + 1
      ELSE IF ( L(1:3).EQ.'OUI' ) THEN
            LOGLUMASC = .TRUE.
            ICOL = I1 + 2
      ELSE IF ( L(1:3).EQ.'YES' ) THEN
            LOGLUMASC = .TRUE.
            ICOL = I1 + 2
      ELSE IF (L(1:1).EQ.'0') THEN
            LOGLUMASC = .FALSE.
            ICOL = I1
      ELSE IF (L(1:1).EQ.'1') THEN
            LOGLUMASC = .TRUE.
            ICOL = I1
      ELSE IF (L(1:7).EQ.'.FALSE.' ) THEN
            LOGLUMASC = .FALSE.
            ICOL = I1 + 6
      ELSE IF (L(1:5).EQ.'FALSE' ) THEN
            LOGLUMASC = .FALSE.
            ICOL = I1 + 4
      ELSE IF (L(1:4).EQ.'FAUX') THEN
            LOGLUMASC = .FALSE.
            ICOL = I1 + 3
      ELSE IF ( L(1:6).EQ.'.TRUE.' ) THEN
            LOGLUMASC = .TRUE.
            ICOL = I1 + 5
      ELSE IF ( L(1:4).EQ.'TRUE' ) THEN
            LOGLUMASC = .TRUE.
            ICOL = I1 + 3
      ELSE IF ( L(1:4).EQ.'VRAI' ) THEN
            LOGLUMASC = .TRUE.
            ICOL = I1 + 3
      ELSE
C
C     ERREUR : CA N'EST PAS UNE VALEUR LOGIQUE
C
            ERREUR = .TRUE.
            WRITE(LU,'(1X,A)') LIGNE(1:LONGLI)
            IF (LUFIC) WRITE(LU,'(1X,A)') LIGNE2(1:LONGLI)
            WRITE(LU,*) ' '
            IF(LNG.EQ.1) THEN
              WRITE(LU,'(1X,A6,I4,A)') 'LOGLU (UTILE) : LIGNE: ',NLIGN,
     *                                 ' ERREUR, LOGIQUE MAL CODE'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,'(1X,A6,I4,A)') 'LOGLU (UTILE) : LINE: ',NLIGN,
     *                                 ' WRONG LOGICAL VALUE'
            ENDIF
            LOGLUMASC = .FALSE.
            GO TO 1000
C
      ENDIF
C
C        //// MISE A JOUR DU POINTEUR ////
C
      IF (LUFIC) THEN
        NLIGN = NLIGN + 1
        LIGNE = LIGNE2
        IF (ICOL.GT.LONGLI) LISUIV = .TRUE.
        IF (LISUIV) THEN
          ICOL = I2-1
        ELSE
          ICOL = 0
        ENDIF
      ELSE
        ICOL = I2 - 1
      ENDIF
C
1000  CONTINUE
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
      END
