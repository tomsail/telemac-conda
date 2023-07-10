                        SUBROUTINE CLASSEMASC
C                       *********************
C
     *(DIMENS , SIZE   , MOTCLE , UTINDX , NMAX   ,
     * OFFSET , ADRESS , INDIC  , LUIGN  ,
     * MOTINT , MOTREA , MOTLOG , MOTCAR , MOTATT ,
     * DEFCAR , DEFINT , DEFLOG , DEFREA , DEFATT )
C
C***********************************************************************
C DAMOCLES VERSION 5.0     16/08/94   L. LEGUE
C                          14/12/93   O. QUIQUEMPOIX  (LNH)  30 87 78 70
C
C Copyright EDF 1994
C
C
C***********************************************************************
C
C FONCTION  :  LE SOUS PROGRAMME 'CLASSE' RANGE DANS LES TABLEAUX
C              MOTINT, MOTREA, MOTLOG, MOTCAR ET MOTATT LES VALEURS LUES
C              POUR UN MOT CLE. IGNORE LES MOTS RENVOYES PAR EDAMOX DANS
C              LE FICHIER DONNEES
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !  DIMENS        !<-->! TABLEAU DES DIMENSIONS DES MOTS CLES         !
C !  SIZE          !<-->! TABLEAU DES LONGUEURS DES MOTS CLES          !
C !  MOTCLE        ! -->! TABLEAU DES MOTS CLES ACTIFS                 !
C !  UTINDX        !<-->! TABLEAU DE LOGIQUES D'UTILISATION DES INDEX  !
C !  NMAX          ! -->! TAILLE MAXIMALE AUTORISEE POUR LES TABLEAUX  !
C !  OFFSET        !<-->! TABLEAUX DES PROCHAINES ADRESSES LIBRES      !
C !  ADRESS        !<-->! TABLEAU DES ADRESSES DES MOTS CLES           !
C !  INDIC         !<-->! TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES   !
C !                !    ! = 0 : PAS DE SUBMIT & NON TABLEAU            !
C !                !    ! = 1 : PAS DE SUBMIT & TABLEAU                !
C !                !    ! = 2 : AVEC   SUBMIT & NON TABLEAU            !
C !                !    ! = 3 : AVEC   SUBMIT & NON TABLEAU            !
C !  LUIGN         ! -->! LOGIQUE POUR LES MOTS A NE PAS CLASSER       !
C !  MOTINT        !<-->! TABLEAU DES VALEURS ENTIERES                 !
C !  MOTREA        !<-->! TABLEAU DES VALEURS REELLES                  !
C !  MOTLOG        !<-->! TABLEAU DES VALEURS LOGIQUES                 !
C !  MOTCAR        !<-->! TABLEAU DES VALEURS CARACTERES               !
C !  MOTATT        !<-->! TABLEAU DES SUBMITS                          !
C !  DEFINT        !<-->! TABLEAU DES VALEURS ENTIERES PAR DEFAUT      !
C !  DEFREA        !<-->! TABLEAU DES VALEURS REELLES PAR DEFAUT       !
C !  DEFLOG        !<-->! TABLEAU DES VALEURS LOGIQUES PAR DEFAUT      !
C !  DEFCAR        !<-->! TABLEAU DES VALEURS CARACTERES PAR DEFAUT    !
C !  DEFATT        !<-->! TABLEAU DES SUBMITS PAR DEFAUT               !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !   /COMMON/     !    !                                              !
C !                !    !                                              !
C !    DCINFO      !    !                                              !
C !  . LNG         ! -->! NUMERO DE LA LANGUE DE DECODAGE              !
C !  . LU          ! -->! NUMERO DE L'UNITE LOGIQUE DES SORTIES        !
C !                !    !                                              !
C !    DCNGE       !    !                                              !
C !  . INDX        !<-->! INDEX DU MOT CLE EN COURS                    !
C !  . NTYP        !<-->! TYPE DU MOT CLE EN COURS                     !
C !  . ITAI        !<-->! TAILLE DU MOT CLE EN COURS                   !
C !  . LONGU       !<-->! LONGUEUR DU MOT CLE EN COURS                 !
C !  . NMOT        !<-->! TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE      !
C !  . DEFLU       !<-->! NOMBRE DE VALEURS LUES POUR LE MOT CLE       !
C !                !    !                                              !
C !    DCMLIG      !    !                                              !
C !  . NLIGN       ! -->! NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU!
C !  . LONGLI      ! -->! LONGUEUR DES LIGNES                          !
C !                !    !                                              !
C !    DCNGEC      !    !                                              !
C !  . PARAM       !<-->! NOM DU MOT CLE EN COURS                      !
C !________________!____!______________________________________________!
C
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C     - PORTABILITE : IBM,CRAY,HP,SUN
C
C     - APPELE PAR :  DAMOC
C
C***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER          LNG,LU
      INTEGER          INDX,NTYP,ITAI,LONGU,NMOT(4),DEFLU
      INTEGER          NLIGN,LONGLI
      CHARACTER*72     PARAM
C
      INTEGER          NMAX,MOTINT(*),ADRESS(4,*),DIMENS(4,*)
      INTEGER          SIZE(4,*),OFFSET(4),DEFINT(*),INDIC(4,*)
      LOGICAL          UTINDX(4,*),DEFLOG(*),MOTLOG(*),LUIGN
      CHARACTER*72     MOTCLE(4,*)
      CHARACTER*144    MOTCAR(*),DEFCAR(*)
      CHARACTER*144    MOTATT(4,*),DEFATT(*)
      DOUBLE PRECISION MOTREA(*),DEFREA(*)
C
C-----------------------------------------------------------------------
C
      INTEGER          I
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCNGE  / INDX,NTYP,ITAI,LONGU,NMOT,DEFLU
      COMMON / DCMLIG / NLIGN , LONGLI
      COMMON / DCNGEC / PARAM
C
C***********************************************************************
C                                    MARQUAGE RCS ET SCCS
C
C***********************************************************************
C
                IF (LUIGN) GO TO 1600
C
C               TRAITEMENT GLOBAL DU MOT CLE
C
                IF (INDX .GT. NMAX) THEN
                  WRITE(LU,*) '****************************************'
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*) 'ERREUR A LA LIGNE :',NLIGN,
     *                          ' DU DICTIONNAIRE'
                    WRITE(LU,*) 'INDEX INVALIDE : ',INDX,' MAX = ',NMAX
                  ELSEIF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'ERROR AT LINE:',NLIGN,
     *                          ' OF THE DICTIONARY'
                    WRITE(LU,*) 'INVALID INDEX: ',INDX,' MAX = ',NMAX
                  ENDIF
                  WRITE(LU,*) '****************************************'
                  STOP 'ERREUR DAMOCLES 6'
                ENDIF
C
                IF (NMOT(NTYP) .GT. NMAX) THEN
                  WRITE(LU,*)'*****************************************'
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*) 'ERREUR A LA LIGNE :',NLIGN,
     *                          ' DU DICTIONNAIRE'
                    WRITE(LU,*)'TROP DE MOTS CLES. MAXIMUM : ',NMAX
                  ELSEIF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'ERROR AT LINE:',NLIGN,
     *                          ' OF THE DICTIONARY'
                    WRITE(LU,*) 'TOO MANY KEY-WORDS, MAXIMUM : ',NMAX
                  ENDIF
                  WRITE(LU,*)'*****************************************'
                  STOP 'ERREUR DAMOCLES 7'
                ENDIF
C
C REDONDANT AVEC LUIGN ? LAISSE PAR DEFAUT- A VERIFIER
                IF (INDX .LE. 0) GO TO 1600
C
                IF (UTINDX(NTYP,INDX)) THEN
                  WRITE(LU,*)'*****************************'
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*) 'ERREUR A LA LIGNE : ',NLIGN
                    WRITE(LU,*) 'L''INDEX  : ',INDX,
     *             ' A DEJA ETE UTILISE POUR LE TYPE : ',NTYP
                  ELSEIF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'ERROR AT LINE: ',NLIGN
                    WRITE(LU,*) 'THE INDEX: ',INDX,
     *             ' IS USED TWO TIMES FOR THE TYPE : ',NTYP
                  ENDIF
                  WRITE(LU,*)'*****************************'
                  STOP 'ERREUR DAMOCLES 8'
                ELSE
                   UTINDX(NTYP,INDX) = .TRUE.
                ENDIF
C
                IF (ITAI .LE. 0) THEN
                   ITAI = 1
                ELSE
C POUR INTERDIRE LE DYNAMIQUE SUR UN NON TABLEAU
                   INDIC(NTYP,INDX)=INDIC(NTYP,INDX)+1
                ENDIF
C
C AJOUT CF JMH - PASSE EN WARNING POUR LES DICOS ESTET - N3S
C WARNING SI VALEURS PAR DEFAUT DEFINIES EN NOMBRE DIFFERENT
C DE LA TAILLE
C
               IF(DEFLU.GT.0.AND.DEFLU.NE.ITAI) THEN
                  WRITE(LU,*) ' '
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*)'ATTENTION ! A LA LIGNE ',NLIGN,
     *                         ' DU DICTIONNAIRE :'
                    WRITE(LU,*)'LE NOMBRE DE VALEURS PAR DEFAUT ',
     *                          DEFLU,' EST DIFFERENT DE LA TAILLE ',
     *                          'ANNONCEE ',ITAI
                  ELSEIF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'WARNING !  AT LINE ',NLIGN,
     *                          ' OF THE DICTIONARY :'
                    WRITE(LU,*) 'NUMBER OF DEFAULT VALUES ',DEFLU,
     *                           ' IS DIFFERENT FROM THE SIZE ',ITAI
                  ENDIF
                  WRITE(LU,*) ' '
               ENDIF
C
                IF (DEFLU .EQ. 0) THEN
                   IF     (NTYP .EQ. 1) THEN
                      DEFINT(1) = 0
                   ELSEIF (NTYP .EQ. 2) THEN
                      DEFREA(1) = 0.0
                   ELSEIF (NTYP .EQ. 3)THEN
                      DEFLOG(1) = .FALSE.
                   ELSEIF (NTYP .EQ. 4) THEN
                      DEFCAR(1) = ' '
                   ENDIF
                ENDIF
C
                IF (ITAI .NE. DEFLU) THEN
                   IF (ITAI .GT. DEFLU) THEN
                      DO 100 I = DEFLU + 1 , ITAI
                         IF     (NTYP .EQ. 1) THEN
                            DEFINT(I) = DEFINT(MAX(1,DEFLU))
                         ELSEIF (NTYP .EQ. 2) THEN
                            DEFREA(I) = DEFREA(MAX(1,DEFLU))
                         ELSEIF (NTYP .EQ. 3) THEN
                            DEFLOG(I) = DEFLOG(MAX(1,DEFLU))
                         ELSEIF (NTYP .EQ. 4) THEN
                            DEFCAR(I) = DEFCAR(MAX(1,DEFLU))
                         ENDIF
C                        DEFATT(NYTP,I) = DEFATT(NYTP,MAX(1,DEFLU))
 100                  CONTINUE
                   ENDIF
                   DEFLU = ITAI
                ENDIF
C
C   RANGEMENT DANS LES TABLEAUX DES ATTRIBUTS DU MOT CLE
C   NOMBRE DE MOTS CLES RENCONTRES DE TYPE NTYP
C
                NMOT(NTYP) = NMOT(NTYP) + 1
C
C   PROCHAINE ADRESSE DE LIBRE POUR LE MOT CLE DE TYPE NTYP
C
                ADRESS(NTYP,INDX) = OFFSET(NTYP)
C
C   MOT CLE RANGE
C
                MOTCLE(NTYP,INDX) = PARAM(1:LONGU)
C
C   NBRE DE VALEURS ASSOCIEES AU MOT CLE DE TYPE NTYP
C
                DIMENS(NTYP,INDX) = ITAI
C
C   LONGUEUR DU MOT CLE EN CARACTERES
C
                SIZE(NTYP,INDX) = LONGU
C
C   RANGEMENT DES VALEURS DANS LES TABLEAUX
C
                IF (((ADRESS(NTYP,INDX)+ITAI-1) .GT. NMAX)
     *             .OR. (OFFSET(NTYP) .GT. NMAX)) THEN
                     IF(LNG.EQ.1) THEN
                       WRITE(LU,*) 'ADRESSE SUPERIEURE A NMAX = ',NMAX
                       WRITE(LU,*) 'TROP DE VALEURS DE TYPE : ',NTYP
     *                             ,' DECLAREES.'
                       WRITE(LU,*) 'ARRET AU MOT CLE D''INDEX : ',INDX
                     ELSEIF(LNG.EQ.2) THEN
                       WRITE(LU,*) 'ADRESS GREATER THAN NMAX = ',NMAX
                       WRITE(LU,*) 'TOO MANY VALUES OF TYPE : ',NTYP
     *                             ,' DECLARED.'
                       WRITE(LU,*) 'STOP AT KEY-WORD OF INDEX: ',INDX
                     ENDIF
                     STOP 'ERREUR DAMOCLES 9'
                   ENDIF
C
                DO 200 I = 1 , ITAI
                   IF (NTYP .EQ. 1) THEN
                      MOTINT(ADRESS(NTYP,INDX)+I-1) = DEFINT(I)
                   ELSE IF (NTYP .EQ. 2) THEN
                      MOTREA(ADRESS(NTYP,INDX)+I-1) = DEFREA(I)
                   ELSE IF (NTYP .EQ. 3) THEN
                      MOTLOG(ADRESS(NTYP,INDX)+I-1) = DEFLOG(I)
                   ELSE IF (NTYP .EQ. 4) THEN
                      MOTCAR(ADRESS(NTYP,INDX)+I-1) = DEFCAR(I)
                   ENDIF
                   IF (INDIC(NTYP,INDX).GE.2)
     *                 MOTATT(NTYP,ADRESS(NTYP,INDX)+I-1) = DEFATT(I)
 200            CONTINUE
C
C   MISE A JOUR DE LA PROCHAINE ADRESSE LIBRE
C
                OFFSET(NTYP) = OFFSET(NTYP) + ITAI
C
C   INITIALISATION DES VARIABLES TEMPORAIRES
C
1600            CONTINUE
                PARAM  = ' '
                LONGU  = 0
                NTYP   = -100
                INDX   = 123456
                ITAI   = -100
                DEFLU  = 0
C
C-----------------------------------------------------------------------
C
       RETURN
       END
