                        SUBROUTINE DAMOCMASC
C                       ********************
C
     *          ( ADRESS , DIMENS , NMAX   , DOC    , LLNG   , LLU    ,
     *            MOTINT , MOTREA , MOTLOG , MOTCAR , MOTATT ,
     *            DEFINT , DEFREA , DEFLOG , DEFCAR , DEFATT ,
     *            USRINT , USRREA , USRLOG , USRCAR , USRATT ,
     *            MOTCLE , SIZE   , TROUVE , UTINDX , NFICMO , NFICDA ,
     *            INDIC  , GESTD  , NBLANG )
C
C***********************************************************************
C DAMOCLES VERSION 5.0     16/08/94   J.M. HERVOUET (LNH)   30 87 80 18
C                                      A. YESSAYAN
C                                      L. LEGUE
C                          14/12/93    O. QUIQUEMPOIX (LNH)  30 87 78 70
C
C Copyright EDF 1994
C
C
C***********************************************************************
C
C FONCTION  : CORPS PRINCIPAL DE LA BIBILIOTHEQUE DAMOCLES
C             APPELE PAR L'EXECUTABLE DAMOCLES (DAMOCLE.f)
C             APPELE PAR LES CODES DE CALCULS DU LNH
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !  ADRESS        !<-- ! TABLEAU DES ADRESSES DES MOTS CLES           !
C !  DIMENS        !<-- ! TABLEAU DES DIMENSIONS DES MOTS CLES         !
C !  NMAX          ! -->! TAILLE MAXIMALE AUTORISEE POUR LES TABLEAUX  !
C !  DOC           ! -->! LOGIQUE DE DOCUMENTATION DE LA SORTIE        !
C !                !    ! = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)   !
C !                !    ! = FAUX : N'IMPRIME PAS L'AIDE                !
C !  LLNG          ! -->! NUMERO DE LA LANGUE DE DECODAGE              !
C !  LLU           ! -->! NUMERO DE L'UNITE LOGIQUE DES SORTIES        !
C !  MOTINT        !<-- ! TABLEAU DES VALEURS ENTIERES                 !
C !  MOTREA        !<-- ! TABLEAU DES VALEURS REELLES                  !
C !  MOTLOG        !<-- ! TABLEAU DES VALEURS LOGIQUES                 !
C !  MOTCAR        !<-- ! TABLEAU DES VALEURS CARACTERES               !
C !  MOTATT        !<-- ! TABLEAU DES SUBMITS                          !
C !  DEFINT        !<-- ! TABLEAU DES VALEURS ENTIERES PAR DEFAUT      !
C !  DEFREA        !<-- ! TABLEAU DES VALEURS REELLES PAR DEFAUT       !
C !  DEFLOG        !<-- ! TABLEAU DES VALEURS LOGIQUES PAR DEFAUT      !
C !  DEFCAR        !<-- ! TABLEAU DES VALEURS CARACTERES PAR DEFAUT    !
C !  DEFATT        !<-- ! TABLEAU DES SUBMITS PAR DEFAUT               !
C !  USRINT        !<-- ! TABLEAU DES VALEURS ENTIERES A USAGE LOCAL   !
C !  USRREA        !<-- ! TABLEAU DES VALEURS REELLES A USAGE LOCAL    !
C !  USRLOG        !<-- ! TABLEAU DES VALEURS LOGIQUES A USAGE LOCAL   !
C !  USRCAR        !<-- ! TABLEAU DES VALEURS CARACTERES A USAGE LOCAL !
C !  USRATT        !<-- ! TABLEAU DES SUBMITS A USAGE LOCAL            !
C !  MOTCLE        !<-- ! TABLEAU DES MOTS CLES ACTIFS                 !
C !  SIZE          !<-- ! TABLEAU DES LONGUEURS DES MOTS CLES          !
C !  TROUVE        !<-- ! INDICATEUR D'ETAT DES MOTS CLES              !
C !                !    ! = 0 : AUCUNE VALEUR TROUVEE                  !
C !                !    ! = 1 : VALEUR PAR DEFAUT TROUVEE              !
C !                !    ! = 2 : VALEUR TROUVEE (FICHIER DE DONNEES)    !
C !                !    ! = 3 : AUCUNE VALEUR TROUVEE (OPTIONNELLE)    !
C !                !    ! = 5 : TABLEAU DE MOTS A SUBMIT COMPACTE      !
C !                !    ! = 6 : MOT CLE A SUBMIT FORCE NON AFFECTE     !
C !                !    ! = 7 : MOT CLE A SUBMIT FORCE AFFECTE (DICOMASC)  !
C !                !    ! = 8 : MOT CLE A SUBMIT FORCE AFFECTE (CAS)   !
C !                !    ! = 9 : FICHIER DICOMASC : SUBMIT + VALEUR LANCEUR !
C !                !    ! =10 : FICHIER CAS  : SUBMIT + VALEUR LANCEUR !
C !  UTINDX        !<-- ! TABLEAU DE LOGIQUES D'UTILISATION DES INDEX  !
C !  NFICMO        ! -->! NUMERO DE CANAL DU FICHIER DES MOTS-CLES     !
C !  NFICDA        ! -->! NUMERO DE CANAL DU FICHIER DES DONNEES       !
C !  INDIC         !<-- ! TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES   !
C !                !    ! = 0 : PAS DE SUBMIT & NON TABLEAU            !
C !                !    ! = 1 : PAS DE SUBMIT & TABLEAU                !
C !                !    ! = 2 : AVEC   SUBMIT & NON TABLEAU            !
C !                !    ! = 3 : AVEC   SUBMIT & TABLEAU                !
C !  GESTD         ! -->! LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES !
C !  NBLANG        ! -->! NOMBRE DE LANGUES CONNUES                    !
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
C !    DCNGEC      !    !                                              !
C !  . PARAM       !<-->! NOM DU MOT CLE EN COURS                      !
C !                !    !                                              !
C !    DCNGE       !    !                                              !
C !  . INDX        !<-->! INDEX DU MOT CLE EN COURS                    !
C !  . NTYP        !<-->! TYPE DU MOT CLE EN COURS                     !
C !  . ITAI        !<-->! TAILLE DU MOT CLE EN COURS                   !
C !  . LONGU       !<-->! LONGUEUR DU MOT CLE EN COURS                 !
C !  . NMOT        !<-->! TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE      !
C !  . DEFLU       !<-->! NOMBRE DE VALEURS LUES POUR LE MOT CLE       !
C !                !    !                                              !
C !    DCCHIE      !    !                                              !
C !  . NFIC        ! -->! NUMERO DE CANAL DU FICHIER EN COURS DE LECT. !
C !________________!____!______________________________________________!
C
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C
C     - APPELE PAR :              DAMOCLE.f & CODES DE CALCULS
C
C     - SOUS PROGRAMMES APPELES : CMDMASC,DICOMASC,CLASSEMASC,INFLUMASC
C
C     - FONCTIONS APPELEES :      INTLUMASC,REALUMASC,LOGLUMASC,CARLUMASC,NEXTMASC,PREVALMASC,
C                                 PREVMASC,LONGLUMASC
C
C     - PRECAUTIONS D'EMPLOI :
C
C     - PORTABILITE :             IBM,CRAY,HP,SUN
C
C     - DOCUMENTATION :
C
C***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER          NMAX,LLNG,LLU,NFICMO,NFICDA,NBLANG
      INTEGER          MOTINT(*),DEFINT(*),USRINT(*)
      INTEGER          SIZE(4,*),ADRESS(4,*),DIMENS(4,*)
      INTEGER          INDIC(4,*),TROUVE(4,*)
      LOGICAL          MOTLOG(*),DEFLOG(*),USRLOG(*),UTINDX(4,*),DOC
      CHARACTER*72     MOTCLE(4,*)
      CHARACTER*144    MOTATT(4,*),DEFATT(*),USRATT(*)
      CHARACTER*144    MOTCAR(*),DEFCAR(*),USRCAR(*)
      DOUBLE PRECISION MOTREA(*),DEFREA(*),USRREA(*)
C
      INTEGER          INTLUMASC,NEXTMASC,PREVMASC,PREVALMASC,LONGLUMASC
      LOGICAL          LOGLUMASC
      CHARACTER*144    CARLUMASC
      DOUBLE PRECISION REALUMASC
C
      INTEGER          LNG,LU
      INTEGER          INDX,NTYP,ITAI,LONGU,NMOT(4),DEFLU
      INTEGER          NLIGN,LONGLI
      INTEGER          NFIC
      LOGICAL          ERREUR , RETOUR
      CHARACTER*72     PARAM
C
C-----------------------------------------------------------------------
C
      INTEGER          I,K,IVAL,LCAR,ICOL,JCOL,ILONG,ITYP,NUMERO,I2
      INTEGER          DEPLAC,ADD,J,OFFSET(4),NBMOT
      INTEGER          TYPIGN(100),LONIGN(100),NMAXR(4),ORDRE
      INTEGER          ADSRC,ADDES,NULINT,NVAL,NIGN,L1,LONPRO(15)
      LOGICAL          DYNAM,LANGUE,NULLOG,LUIGN,AIDLNG,VUMOT  ! PU2017 : Mise en commentaire de LAID,
      LOGICAL          ARRET,VUCMDMASC(5),VUCMDMASC0(5),EXECMDMASC,GESTD
      CHARACTER*1      PTVIRG,QUOTE  ! PU2017 : Mise en commentaire de TABUL
      CHARACTER*9      MOTPRO(15),TYPE
!      CHARACTER*72     MOTIGN(100),LIGNE ! Modif PU2017
      CHARACTER*72     MOTIGN(100)       ! Modif PU2017
      CHARACTER*77     LIGNE             ! Modif PU2017
      CHARACTER*144    NULCAR
      DOUBLE PRECISION NULREA
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR , RETOUR
      COMMON / DCMLIG / NLIGN , LONGLI
      COMMON / DCCHIE / NFIC
      COMMON / DCNGE  / INDX,NTYP,ITAI,LONGU,NMOT,DEFLU
      COMMON / DCNGEC / PARAM
C
      EXTERNAL         CARLUMASC,INTLUMASC,LOGLUMASC,REALUMASC
      EXTERNAL         NEXTMASC,PREVMASC,PREVALMASC,LONGLUMASC
      INTRINSIC CHAR
C
C-----------------------------------------------------------------------
C
      DATA MOTPRO /'NOM','TYPE','INDEX','TAILLE','DEFAUT','AIDE',
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
C     NUMEROTATION DES TYPES :    1 : ENTIER
C                                 2 : REEL
C                                 3 : LOGIQUE
C                                 4 : CARACTERE
C
C     MOTPRO(I)   : IEME MOT RESERVE POUR LE PROGRAMME (NOM,TYPE,...)
C     MOTCLE(I,J) : NOM DU JIEME MOT-CLE DE TYPE I
C     DIMENS(I,J) : DIMENSION DU JIEME MOT-CLE DE TYPE I
C     ADRESS(I,J) : ADRESSE DU JIEME MOT-CLE DE TYPE I DANS LE TABLEAU
C     MOTINT(I)   : IEME RESULTAT ENTIER
C     MOTREA(I)   : IEME RESULTAT REEL
C     MOTLOG(I)   : IEME RESULTAT LOGIQUE
C     MOTCAR(I)   : IEME RESULTAT CARACTERE
C     MOTATT(I,J) : JEME SUBMIT DU TYPE I
C     SIZE(I,J)   : LONGUEUR DU JIEME MOT-CLE DE TYPE I
C     TROUVE(I,J) : CONCERNE LE JIEME MOT-CLE DE TYPE I
C     INDIC(I,J)  : CONCERNE LE JIEME MOT-CLE DE TYPE I
C     LUIGN       : INDIQUE SI C'EST UN MOT POUR EDAMOX SEULEMENT
C     MOTIGN(I)   : IEME MOT LU DANS LE FICHIER CAS DONNE PAR EDAMOX
C                   ET LU COMME IGNORE DANS LE DICTIONNAIRE
C     DYNAM       : LOGIQUE POUR LE DYNAMIQUE (.TRUE. SI MODE DYNAMIQUE)
C     VUCMDMASC(I)    : TABLEAU DE LOGIQUES (MEMORISATION DES COMMANDES)
C                   I=1->&LIS;I=2->&ETA;I=3->&IND;I=4->&STO;I=5->&FIN
C     EXECMDMASC      : LOGIQUE D'ACTIVATION DES COMMANDES MEMORISEES
C     NMAXR(I)    : INDEX MAXIMUM REELLEMENT UTILISE POUR LE TYPE I
C
C-----------------------------------------------------------------------
C
C INITIALISATIONS :
C
      LU      = LLU
      LNG     = LLNG
      ARRET   = .FALSE.
      ERREUR  = .FALSE.
      RETOUR  = .FALSE.
      DYNAM   = .FALSE.
!      LAID    = .FALSE.  ! PU2017 : Mise en commentaire
      EXECMDMASC  = .FALSE.
      AIDLNG  = .FALSE.
      VUMOT   = .FALSE.
      LONGLI  = 72
      NFIC    = NFICMO
      PTVIRG  = ';'
      QUOTE   = ''''
!      TABUL   = CHAR(9)  ! PU2017 : Mise en commentaire
      NBMOT   = 0
      NIGN    = 0
      ORDRE   = 0
      PARAM  = ' '
      LONGU  = 0
      NTYP   = -100
      INDX   =  123456
      ITAI   = -100
      DEFLU  = 0
C
      DO 2 K=1, 5
       VUCMDMASC(K) = .FALSE.
       VUCMDMASC0(K) = .FALSE.
2     CONTINUE
C
      DO 3 K=1,100
       MOTIGN(K)= ' '
       TYPIGN(K)=1
       LONIGN(K)=0
3     CONTINUE
C
      DO 5 K=1, 4
       NMOT(K) = 0
       NMAXR(K) = 0
       OFFSET(K) = 1
       DO 6 I=1,NMAX
         ADRESS(K,I)  = 0
         DIMENS(K,I)  = 1
         TROUVE(K,I)  = 0
         SIZE(K,I)    = 0
         UTINDX(K,I)  = .FALSE.
         MOTINT(I)    = 0
         MOTREA(I)    = 0.
         MOTLOG(I)    = .FALSE.
         MOTCAR(I)    = ' '
         MOTATT(K,I)  = ' '
         DEFINT(I)    = 0
         DEFREA(I)    = 0.
         DEFLOG(I)    = .FALSE.
         DEFCAR(I)    = ' '
         DEFATT(I)    = ' '
         USRINT(I)    = 0
         USRREA(I)    = 0.
         USRLOG(I)    = .FALSE.
         USRCAR(I)    = ' '
         USRATT(I)    = ' '
         MOTCLE(K,I)  = ' '
         INDIC(K,I)   = 0
6      CONTINUE
5      CONTINUE
C
C CONTROLE DE LA LANGUE.
C
      IF (LNG.LT.1.OR.LNG.GT.NBLANG) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) ' CHOIX DE LA LANGUE = ',LNG,' INVALIDE.'
        WRITE(LU,*) ' ARRET DE DAMOCLES'
        WRITE(LU,*) ' '
        STOP 'ERREUR DAMOCLES 1'
      ENDIF
C
C 99 : NOUVEAU FICHIER   100 : NOUVEAU MOT-CLE
C
  99  CONTINUE
C
      ICOL   = LONGLI
      NLIGN = 0
C
C RECHERCHE DU PREMIER CARACTERE NON BLANC HORS COMMENTAIRES :
C
      ICOL = NEXTMASC(ICOL+1,LIGNE)
C
100   CONTINUE
C
C SI ON EST EN BOUT DE FICHIER :
C
      IF(RETOUR) GO TO 900
C
C REPERAGE DES COMMANDES QUI COMMENCENT PAR &
C
      IF ( LIGNE(ICOL:ICOL).EQ.'&' ) THEN
           CALL CMDMASC (ICOL,LIGNE,ADRESS,DIMENS,TROUVE,MOTCLE,NMOT,
     *          MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT,INDIC,SIZE,
     *          UTINDX,DYNAM,VUCMDMASC,EXECMDMASC,NFICDA,NMAXR)
C
C     SI ON A VU &FIN, ON VA FINIR APRES LE COMPACTAGE :
           IF (VUCMDMASC(5)) GO TO 900
C     SI ON A VU &STO, ON FINIT LA LECTURE DU FICHIER :
           IF (VUCMDMASC(4)) GO TO 1000
C
           ICOL = NEXTMASC(ICOL+1,LIGNE)
           IF(RETOUR) GO TO 900
      ELSE
C
              I2 = PREVALMASC(ICOL+1,LIGNE,'=',':','=')
C             CAS OU LE SIGNE EGAL EST SUR LA LIGNE SUIVANTE :
              IF(I2.GT.LONGLI) I2=LONGLI
              JCOL = PREVMASC  (I2,LIGNE)
              ILONG = JCOL - ICOL + 1
C
              LUIGN = .FALSE.
              IF (NFIC.EQ.NFICMO.AND.INDX.LE.0) LUIGN = .TRUE.
C
C              ILONG = 40
              CALL DICOMASC(ITYP,NUMERO,ILONG,LIGNE(ICOL:JCOL),
     *             MOTCLE,NMOT,MOTPRO,LONPRO,SIZE,UTINDX,LANGUE,
     *             AIDLNG,MOTIGN,NIGN,LUIGN,TYPIGN,LONIGN,NFICDA,
     *             NBLANG,NMAXR)
C
              IF (ERREUR) THEN
                             WRITE(LU,*)
                             WRITE(LU,*)'************************'
                IF(LNG.EQ.1) WRITE(LU,*)'* ARRET DE DAMOCLES    *'
                IF(LNG.EQ.2) WRITE(LU,*)'* DAMOCLES STOPPED     *'
                             WRITE(LU,*)'************************'
                GO TO 900
              ENDIF
C
C SI LE MOT EST INCONNU, ON ARRETE
              IF(ITYP.EQ.0) THEN
                ARRET=.TRUE.
                GOTO 1300
              ENDIF
C
              ICOL = PREVALMASC(ICOL+1,LIGNE,'=',':','=')
C             CAS OU LE SIGNE EGAL EST SUR LA LIGNE SUIVANTE :
              IF(ICOL.GT.LONGLI) THEN
                ICOL  = NEXTMASC(LONGLI,LIGNE)
                IF(RETOUR) GO TO 900
              ENDIF
C
C 1) LECTURE ET AFFECTATION D'UNE VALEUR :
C
           IF(ITYP.LE.4) THEN
C
C A PRIORI LE NOMBRE DE VALEURS A LIRE EST DIMENS(ITYP,NUMERO)
C MAIS ON TOLERE QU'IL EN MANQUE OU QU'IL Y EN AIT EN PLUS
C DANS CE CAS ON S'EN APERCOIT ... ET LES DIFFERENTS TABLEAUX
C SONT MIS A JOUR.
C
C
           IF (.NOT.(LUIGN)) THEN
            NTYP = ITYP
            INDX = NUMERO
            ITAI = DIMENS(NTYP,INDX)
            ADD  = ADRESS(NTYP,INDX)
C           PARAM = MOTCLE(NTYP,INDX)
C           LONGU = LONGLUMASC(PARAM)
            IVAL = 1
            IF (TROUVE(NTYP,INDX).EQ.2.OR.TROUVE(NTYP,INDX).EQ.8) THEN
             WRITE(LU,*) ' '
             IF(LNG.EQ.1) THEN
               WRITE(LU,*) 'LE MOT CLE : ',MOTCLE(NTYP,INDX)(1:ILONG)
               WRITE(LU,*) 'EST CITE AU MOINS 2 FOIS, SEULE LA',
     *                     ' DERNIERE VALEUR EST CONSERVEE...'
             ELSEIF(LNG.EQ.2) THEN
               WRITE(LU,*) 'THE KEY-WORD: ',MOTCLE(NTYP,INDX)(1:ILONG)
               WRITE(LU,*) 'APPPEARS AT LEAST TWICE , THE LAST',
     *                     ' VALUE WILL BE KEPT...'
             ENDIF
             WRITE(LU,*) ' '
            ENDIF
           ENDIF
 10        CONTINUE
           IF (.NOT.(LUIGN)) THEN
             IF     (NTYP.EQ.1) THEN
                     DEFINT(IVAL) = INTLUMASC(ICOL,LIGNE)
             ELSEIF (NTYP.EQ.2) THEN
                     DEFREA(IVAL) = REALUMASC(ICOL,LIGNE)
             ELSEIF (NTYP.EQ.3) THEN
                     DEFLOG(IVAL) = LOGLUMASC(ICOL,LIGNE)
             ELSEIF (NTYP.EQ.4) THEN
                     DEFCAR(IVAL) = CARLUMASC(LCAR,ICOL,LIGNE,QUOTE,
     *                                    MOTCLE,
     *                                    SIZE,MOTIGN,LONIGN,NMAXR,
     *                                    NFICDA,LEN(DEFCAR(IVAL)))
             ENDIF
C
             DEFATT(IVAL) = MOTATT(NTYP,ADD+IVAL-1)
C
C CAS DU SUBMIT VIDE OPTIONNEL- RESTE OPTIONNEL
             IF (ITAI.LE.1.AND.INDIC(NTYP,INDX).GE.2.AND.
     *           TROUVE(NTYP,INDX).EQ.3) THEN
                 L1 = LONGLUMASC(DEFCAR(IVAL))
                 IF (L1.GT.0) TROUVE(NTYP,INDX)=2
C
             ELSEIF(TROUVE(NTYP,INDX).LT.6) THEN
                    TROUVE(NTYP,INDX)=2
C
             ELSEIF (TROUVE(NTYP,INDX).EQ.6.OR.
     *              TROUVE(NTYP,INDX).EQ.7) THEN
                    TROUVE(NTYP,INDX)=8
             ENDIF
C
            ELSE
             NTYP = ITYP
             IF     (NTYP .EQ. 1) THEN
                     NULINT = INTLUMASC(ICOL,LIGNE)
             ELSEIF (NTYP .EQ. 2) THEN
                     NULREA = REALUMASC(ICOL,LIGNE)
             ELSEIF (NTYP .EQ. 3) THEN
                     NULLOG = LOGLUMASC(ICOL,LIGNE)
             ELSEIF (NTYP .EQ. 4) THEN
                     NULCAR = CARLUMASC(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,
     *                              SIZE,MOTIGN,LONIGN,NMAXR,NFICDA,
     *                              LEN(NULCAR))
             ENDIF
           ENDIF
C
           ICOL = NEXTMASC(ICOL+1,LIGNE)
           IF (LIGNE(ICOL:ICOL) .EQ. PTVIRG) THEN
             IVAL = IVAL + 1
             GO TO 10
           ENDIF
C
           IF (LUIGN) GO TO 100
C
C ON A LU TOUTES LES VALEURS D'UN MOT CLE
C
C CAS PARTICULIERS DES MOTS CLES AFFECTES D'UN SUBMIT
C INTERDIRE LE DYNAMIQUE AUX VALEURS INFERIEURES A TAILLE (CF SUBMIT)
C OU AUX MOTS CLES NON TABLEAUX
C
           IF (INDIC(NTYP,INDX).NE.1.AND.IVAL.GT.ITAI) IVAL = ITAI
C
C
C SI IL Y A PLUS DE VALEURS QUE LE PARAMETRE TAILLE ALORS ON TRONQUE
C A ITAI SI LE MOT CLE &DYN NE SE TROUVE PAS DANS LE FICHIER CAS
C SINON ON LIT TOUTES LES VALEURS DU FICHIER CAS
C
C  RECLASSEMASCR EN MODE DYNAMIQUE A CHAQUE FOIS, EST-CE BIEN UTILE ???
C  NE PEUT-ON PAS SE CONTENTER DE LA MODIFICATION DE DIMENS
C  SI LE LECDON EST BIEN ECRIT : PEUT IMPORTE S'IL Y A DES TROUS
C  DANS LES TABLEAUX RENVOYES ... A MEDITER
C
C
           IF (.NOT.(DYNAM)) THEN
             DO 400 I=1 , MIN(IVAL,ITAI)
               IF     (NTYP.EQ.1) THEN
                      MOTINT(ADD+I-1) = DEFINT(I)
               ELSEIF (NTYP.EQ.2) THEN
                      MOTREA(ADD+I-1) = DEFREA(I)
               ELSEIF (NTYP.EQ.3)THEN
                       MOTLOG(ADD+I-1) = DEFLOG(I)
               ELSEIF (NTYP.EQ.4)THEN
                       MOTCAR(ADD+I-1) = DEFCAR(I)
               ENDIF
 400         CONTINUE
           ELSE
              DO 410 I=1 ,NMAXR(NTYP)
                IF (UTINDX(NTYP,I)) THEN
                  IF(ADRESS(NTYP,I) .NE. ADD) THEN
                    IF (ADRESS(NTYP,I) .LT. ADD) DEPLAC = 0
                    IF (ADRESS(NTYP,I) .GT. ADD) DEPLAC = IVAL - ITAI
                    DO 420 J=1 , DIMENS(NTYP,I)
                      ADSRC = ADRESS(NTYP,I)+J-1
                      ADDES = ADRESS(NTYP,I)+J-1+DEPLAC
                      IF (ADDES.GT. NMAX) GO TO 1515
                      IF     (NTYP.EQ.1) THEN
                              USRINT(ADDES) = MOTINT(ADSRC)
                      ELSEIF (NTYP.EQ.2) THEN
                              USRREA(ADDES) = MOTREA(ADSRC)
                      ELSEIF (NTYP.EQ.3) THEN
                              USRLOG(ADDES) = MOTLOG(ADSRC)
                      ELSEIF (NTYP.EQ.4) THEN
                              USRCAR(ADDES) = MOTCAR(ADSRC)
                      ENDIF
                      USRATT(ADDES) = MOTATT(NTYP,ADSRC)
 420                CONTINUE
                    IF (ADRESS(NTYP,I) .GT. ADD) THEN
                      ADRESS(NTYP,I) = ADRESS(NTYP,I) + DEPLAC
                      IF (ADRESS(NTYP,I) .GT. NMAX) GO TO 1515
                    ENDIF
C
                  ELSE IF (ADRESS(NTYP,I) .EQ. ADD) THEN
                    DO 430 J=1 ,IVAL
                      IF     (NTYP.EQ.1) THEN
                             USRINT(ADD+J-1) = DEFINT(J)
                      ELSEIF (NTYP.EQ.2) THEN
                             USRREA(ADD+J-1) = DEFREA(J)
                      ELSEIF (NTYP.EQ.3) THEN
                             USRLOG(ADD+J-1) = DEFLOG(J)
                      ELSEIF (NTYP.EQ.4)THEN
                             USRCAR(ADD+J-1) = DEFCAR(J)
                      ENDIF
                      USRATT(ADD+J-1) = DEFATT(J)
 430                CONTINUE
                    DIMENS(NTYP,I) = IVAL
                  ENDIF
               ENDIF
 410         CONTINUE
C RANGEMENT DANS LES TABLEAUX DEFINITFS
             DO 440 I=1 ,NMAXR(NTYP)
               IF (UTINDX(NTYP,I)) THEN
                 ADSRC = ADRESS(NTYP,I)
                 DO 450 J=1 ,DIMENS(NTYP,I)
                  IF     (NTYP.EQ.1) THEN
                         MOTINT(ADSRC+J-1)=USRINT(ADSRC+J-1)
                  ELSEIF (NTYP.EQ.2) THEN
                         MOTREA(ADSRC+J-1)=USRREA(ADSRC+J-1)
                  ELSEIF (NTYP.EQ.3) THEN
                         MOTLOG(ADSRC+J-1)=USRLOG(ADSRC+J-1)
                  ELSEIF (NTYP.EQ.4)  THEN
                         MOTCAR(ADSRC+J-1)=USRCAR(ADSRC+J-1)
                  ENDIF
                  MOTATT(NTYP,ADSRC+J-1) = USRATT(ADSRC+J-1)
 450             CONTINUE
               ENDIF
 440         CONTINUE
           ENDIF
C
C
C          ICOL = NEXTMASC(ICOL,LIGNE)
C
C ENDIF DU IF(ITYP.LE.4) ...
           ENDIF
C
C
C 2) MOTS-CLES RESERVES :
C
C    LES MOTS-CLES RESERVES SONT POUR L'INSTANT :
C
C           'NOM'       :NUMERO = 1  (DE TYPE CARACTERE)
C           'TYPE'      :NUMERO = 2  (DE TYPE CARACTERE)
C           'INDEX'     :NUMERO = 3  (DE TYPE ENTIER)
C           'TAILLE'    :NUMERO = 4  (DE TYPE ENTIER)
C           'DEFAUT'    :NUMERO = 5  (DE TYPE VARIABLE)
C           'AIDE'      :NUMERO = 6  (DE TYPE CARACTERE)
C           'CHOIX'     :NUMERO = 7  (DE TYPE VARIABLE)
C           'RUBRIQUE'  :NUMERO = 8  (DE TYPE CARACTERE)
C           'NIVEAU'    :NUMERO = 9  (DE TYPE ENTIER)
C           'MNEMO'     :NUMERO = 10 (DE TYPE CARACTERE)
C           'COMPOSE'   :NUMERO = 11 (DE TYPE CARACTERE)
C           'COMPORT'   :NUMERO = 12 (DE TYPE CARACTERE)
C           'CONTROLE'  :NUMERO = 13 (DE TYPE ENTIER)
C           'APPARENCE' :NUMERO = 14 (DE TYPE CARACTERE)
C           'SUBMIT'    :NUMERO = 15 (DE TYPE CARACTERE)
C
       IF(ITYP.EQ.5) THEN
C
C    NOM
C
          IF(NUMERO.EQ.1) THEN
C
C IL NE FAUT PAS COMPTER PLUSIEURS FOIS LE MEME MOT EN PLUSIEURS
C LANGUE. ON LE COMPTE UNE FOIS DANS LA PREMIERE LANGUE TROUVEE
C
             IF (.NOT.(VUMOT)) NBMOT = NBMOT + 1
C
             ORDRE = 1
C
C SI ON VIENT DU MOT PRECEDENT, ON LE CLASSEMASC AVANT DE LIRE LE SUIVANT
C PUISQUE TOUTES LES INFOS SUR LE MOT PRECEDENT SONT DISPONIBLES

C
             IF (NBMOT.GT.1 .AND. (.NOT.(VUMOT)) ) THEN
                 IF (INDX.GT.NMAXR(NTYP)) NMAXR(NTYP)=INDX
                 CALL CLASSEMASC(DIMENS,SIZE,MOTCLE,UTINDX,NMAX,
     *                       OFFSET,ADRESS,INDIC,LUIGN,
     *                       MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT ,
     *                       DEFCAR,DEFINT,DEFLOG,DEFREA,DEFATT )
             ENDIF
C
C ON SIGNALE QU'ON A DEJA VU LE NOUVEAU MOT CLE DANS UNE AUTRE LANGUE
             IF (.NOT.(VUMOT)) VUMOT=.TRUE.
C
C            NOM DU MOT-CLE
             IF (LANGUE) THEN
               PARAM = CARLUMASC(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,
     *                       MOTIGN,
     *                       LONIGN,NMAXR,NFICDA,LEN(PARAM))
               LONGU = LCAR
             ELSE
C LECTURE D'UN NOM DE LANGUE NON DEMANDE (NON UTILISE)
               NULCAR = CARLUMASC(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,
     *                        MOTIGN,
     *                        LONIGN,NMAXR,NFICDA,LEN(NULCAR))
             ENDIF
C
             ICOL = NEXTMASC(ICOL+1,LIGNE)
C
C    TYPE
C
          ELSE IF(NUMERO.EQ.2) THEN
                  VUMOT = .FALSE.
                  IF (ORDRE.NE.1) GOTO 1500
                  ORDRE=2
                  TYPE = CARLUMASC(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,
     *                         MOTIGN,
     *                         LONIGN,NMAXR,NFICDA,LEN(TYPE))
                  IF(TYPE(1:6).EQ.'ENTIER'
     *           .OR.TYPE(1:8).EQ.'INTEGER') THEN
                    NTYP = 1
                  ELSEIF(TYPE(1:4).EQ.'REEL'
     *           .OR.TYPE(1:4).EQ.'REAL') THEN
                    NTYP = 2
                  ELSEIF(TYPE(1:7).EQ.'LOGIQUE'
     *           .OR.TYPE(1:7).EQ.'LOGICAL') THEN
                    NTYP = 3
                  ELSEIF(TYPE(1:9).EQ.'CARACTERE'
     *           .OR.TYPE(1:6).EQ.'STRING') THEN
C    *           .OR.TYPE(1:4).EQ.'FILE'
C    *           .OR.TYPE(1:7).EQ.'FICHIER') THEN
                    NTYP = 4
                  ELSE
C                 ERREUR : TYPE INCONNU
                  IF(LNG.EQ.1) WRITE (LU,1002) LIGNE
                  IF(LNG.EQ.2) WRITE (LU,1003) LIGNE
1002              FORMAT(1X,A72,/,1X,'TYPE INCONNU SUR CETTE LIGNE')
1003              FORMAT(1X,A72,/,1X,'UNKNOWN TYPE ON THIS LINE')
                  STOP 'ERREUR DAMOCLES 2'
                  ENDIF
                  ICOL = NEXTMASC(ICOL+1,LIGNE)
C
C    INDEX
C
          ELSE IF(NUMERO.EQ.3) THEN
                  IF (ORDRE.NE.2) GOTO 1500
                  ORDRE=3
                  INDX = INTLUMASC(ICOL,LIGNE)
                  ICOL = NEXTMASC(ICOL+1,LIGNE)
C
C CAS INDEX=-1 - MOT POUR CONSTRUCTION EDAMOX A GARDER
C
                  IF (INDX.EQ.-1) THEN
                    NIGN = NIGN + 1
                    IF (NIGN.GT.100) THEN
                      IF (LNG.EQ.1) THEN
                        WRITE(LU,*) 'TROP DE MOTS RESERVES POUR EDAMOX',
     *                              ' (100 AU MAXIMUM)'
                      ELSEIF (LNG.EQ.2) THEN
                        WRITE(LU,*) 'TOO MANY WORDS FOR EDAMOX',
     *                              ' (MAX=100)'
                      ENDIF
                      ERREUR = .TRUE.
                      GO TO 900
                    ENDIF
                    MOTIGN(NIGN)=PARAM(1:LONGU)
                    LONIGN(NIGN)=LONGU
                    TYPIGN(NIGN)=NTYP
                  ENDIF
C
C    TAILLE
C
          ELSE IF(NUMERO.EQ.4) THEN
                  IF (ORDRE.NE.3) GOTO 1500
                  ORDRE=4
                  ITAI = INTLUMASC(ICOL,LIGNE)
                  ICOL = NEXTMASC(ICOL+1,LIGNE)
C
C    VALEUR PAR DEFAUT
C    POUR LES TABLEAUX, IL N'EST PAS OBLIGATOIRE DE METTRE TOUTES LES VA
C
          ELSE IF(NUMERO.EQ.5) THEN
C
C
          IF (ORDRE.LT.3.OR.ORDRE.GT.6) GOTO 1500
          ORDRE=6
          IF (LANGUE) THEN
             DEFLU = 1
             IF (NTYP.NE.4) TROUVE(NTYP,INDX) = 1
C
C200          ICOL = NEXTMASC(ICOL+1,LIGNE) -1
200          CONTINUE
C
             IF (NTYP .EQ. 1) THEN
                DEFINT(DEFLU) = INTLUMASC(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 2) THEN
                DEFREA(DEFLU) = REALUMASC(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 3) THEN
                DEFLOG(DEFLU) = LOGLUMASC(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 4) THEN
                DEFCAR(DEFLU) = CARLUMASC(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,
     *                                SIZE,MOTIGN,LONIGN,NMAXR,NFICDA,
     *                                LEN(DEFCAR(DEFLU)))
                L1 = LONGLUMASC(DEFCAR(DEFLU))
                IF (ITAI.LE.1.AND.INDIC(NTYP,INDX).GE.2) THEN
                   IF (TROUVE(NTYP,INDX).LE.3) THEN
                       IF (L1.GT.0) TROUVE(NTYP,INDX)=1
                   ELSEIF(TROUVE(NTYP,INDX).EQ.6) THEN
C                      IF (L1.GT.0) TROUVE(NTYP,INDX)=7
                       TROUVE(NTYP,INDX)=7
                   ENDIF
                ELSE
                  TROUVE(NTYP,INDX)=1
                ENDIF
             ENDIF
C
             ICOL = NEXTMASC(ICOL+1,LIGNE)
C
             IF (LIGNE(ICOL:ICOL) .EQ. PTVIRG) THEN
                DEFLU = DEFLU + 1
                GO TO 200
             ELSE
                ICOL=ICOL-1
             ENDIF
C
C
C
          ELSE
C
C LECTURE D'UN DEFAUT DE LANGUE NON DEMANDE (NON UTILISE)
C
C210          ICOL = NEXTMASC(ICOL+1,LIGNE) -1
 210         CONTINUE
C
             IF (NTYP .EQ. 1) THEN
                NULINT = INTLUMASC(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 2) THEN
                NULREA = REALUMASC(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 3) THEN
                NULLOG = LOGLUMASC(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 4) THEN
                NULCAR = CARLUMASC(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,
     *                         MOTIGN,
     *                         LONIGN,NMAXR,NFICDA,LEN(NULCAR))
             ENDIF
C
             ICOL = NEXTMASC(ICOL+1,LIGNE)
C
             IF (LIGNE(ICOL:ICOL) .EQ. PTVIRG) THEN
                GO TO 210
             ELSE
C               DEFLU=DEFLU
                ICOL=ICOL-1
             ENDIF
          ENDIF
C
          ICOL = NEXTMASC(ICOL+1,LIGNE)
C
C    AIDE
C
          ELSE IF(NUMERO.EQ.6) THEN
C
          IF(AIDLNG.AND.DOC) THEN
            WRITE(LU,511)
511         FORMAT(1X,72('-'))
            WRITE(LU,*) PARAM(1:LONGU)
            WRITE(LU,511)
          ENDIF
          CALL AIDELUMASC(ICOL,LIGNE,DOC.AND.AIDLNG)
          AIDLNG = .FALSE.
C
C
C    CHOIX RUBRIQUE NIVEAU MNEMO COMPOSE COMPORT CONTROLE APPARENCE
C    NUMERO 7 A 14 INCLUS
C
          ELSE IF((NUMERO .GE. 7) .AND. (NUMERO .LE. 14)) THEN
            CALL AIDELUMASC(ICOL,LIGNE,.FALSE.)
C
C    DEFINITION D'UN SUBMIT TYPE
          ELSE IF (NUMERO .EQ. 15) THEN
            IF (ORDRE.NE.3.AND.ORDRE.NE.4) GOTO 1500
            ORDRE=5
            IF (.NOT.(LUIGN)) INDIC(NTYP,INDX)=INDIC(NTYP,INDX)+2
            ICOL = NEXTMASC(ICOL+1,LIGNE) -1
            CALL INFLUMASC(ICOL,LIGNE,DEFATT,TROUVE,LUIGN,MOTCLE,SIZE,
     *                 MOTIGN,LONIGN,NMAXR,NFICDA,GESTD)
            DO 890 I=1,DEFLU
               DEFINT(I)    = 0
               DEFREA(I)    = 0.
               DEFLOG(I)    = .FALSE.
               DEFCAR(I)    = ' '
 890        CONTINUE
            IF (ERREUR) GO TO 900
            ICOL = NEXTMASC(ICOL,LIGNE)
          ENDIF
C
          ENDIF
C
      ENDIF
C
      GO TO 100
900   CONTINUE
      IF (ERREUR) THEN
         WRITE(LU,*)' '
         IF (NFIC .EQ. NFICMO) THEN
            WRITE(LU,*)'-------------------------------'
            IF(LNG.EQ.1) THEN
            WRITE(LU,*)'- ERREUR DANS LE DICTIONNAIRE -'
            ENDIF
            IF(LNG.EQ.2) THEN
            WRITE(LU,*)'- ERROR IN THE DICTIONARY     -'
            ENDIF
            WRITE(LU,*)'-------------------------------'
         ELSE
            WRITE(LU,*)'-----------------------------------------'
            IF(LNG.EQ.1) THEN
            WRITE(LU,*)'- ERREUR DANS LE FICHIER DES PARAMETRES -'
            ENDIF
            IF(LNG.EQ.2) THEN
            WRITE(LU,*)'- ERROR IN THE STEERING FILE            -'
            ENDIF
            WRITE(LU,*)'-----------------------------------------'
         ENDIF
         STOP 'ERREUR DAMOCLES 3'
      ENDIF
C
      IF (NFIC .EQ. NFICMO) THEN
                IF (INDX.GT.NMAXR(NTYP)) NMAXR(NTYP)=INDX
                CALL CLASSEMASC(DIMENS,SIZE,MOTCLE,UTINDX,NMAX,
     *                      OFFSET,ADRESS,INDIC,LUIGN,
     *                      MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT ,
     *                      DEFCAR,DEFINT,DEFLOG,DEFREA,DEFATT )
      ENDIF
      IF(NFICMO.EQ.NFICDA.OR.NFIC.EQ.NFICDA) THEN
C            VRAIE FIN : 2 FICHIERS LUS OU 2 FICHIERS EN 1 LUS
             GO TO 1000
      ELSE
C            FAUSSE FIN : IL RESTE UN FICHIER
             NFIC = NFICDA
             RETOUR = .FALSE.
             GO TO 99
      ENDIF
C
1515  CONTINUE
      WRITE(LU,*)'*********************************************'
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'ADRESSE SUPERIEURE A NMAX = ',NMAX
        WRITE(LU,*)'TROP DE VALEURS DE TYPE : ',NTYP,' DECLAREES.'
        WRITE(LU,*)'ARRET DE DAMOCLES AU MOT-CLE D''INDEX : ',INDX
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*)'ADRESS GREATER THAN NMAX = ',NMAX
        WRITE(LU,*)'TOO MANY VALUES OF TYPE : ',NTYP,' DECLARED.'
        WRITE(LU,*)'STOP OF DAMOCLES AT KEY-WORD NUMBER: ',INDX
      ENDIF
      WRITE(LU,*)'*********************************************'
      STOP 'ERREUR DAMOCLES 4'
C
1000  CONTINUE
C
C COMPACTAGE DES BLANCS - REDISTRIBUTION - TEST DES RESULTATS
C
      DO 1195 K=1,NMAXR(4)
       IF (UTINDX(4,K).AND.INDIC(4,K).GE.2.AND.
     *     TROUVE(4,K).LT.3.AND.TROUVE(4,K).GT.0) THEN
         ADD = ADRESS(4,K)
         PARAM = MOTCLE(4,K)
         LONGU = SIZE(4,K)
         NVAL = DIMENS(4,K)
         I=0
 1180    CONTINUE
         I=I+1
 1185    CONTINUE
C SI C'EST UN BLANC (LONGUEUR NULLE) :
         IF (LONGLUMASC(MOTCAR(ADD+I-1)).EQ.0) THEN
           DO 1190 J=I,NVAL-1
             MOTCAR(ADD+J-1)=MOTCAR(ADD+J)
C
C ON NE FAIT PAS SUIVRE LES SUBMITS SI CETTE LIGNE EST EN COMMENTAIRE
C SINON PB VU AVEC STBTEL
C            MOTATT(4,ADD+J-1)=MOTATT(4,ADD+J)
C
 1190      CONTINUE
           NVAL = NVAL-1
           IF (I.LE.NVAL) GO TO 1185
         ENDIF
         IF (I.LT.NVAL) GO TO 1180
C
C CAS DES ALLOCATIONS REQUISES VIDES POUR LES NON TABLEAUX
C
         IF (NVAL.EQ.0.AND.INDIC(4,K).EQ.2) THEN
           IF (LNG.EQ.1) THEN
             WRITE(LU,*) 'AFFECTATION VIDE NON ADMISE POUR LE ',
     *                   'MOT CLE : ', PARAM(1:LONGU)
           ELSEIF (LNG.EQ.2) THEN
             WRITE(LU,*) 'EMPTY ALLOCATION NOT ALLOWED FOR ',
     *                   'THE KEY WORD : ', PARAM(1:LONGU)
           ENDIF
           WRITE(LU,*)
           ARRET = .TRUE.
           GO TO 1300
         ENDIF
C
C POUR LES TABLEAUX, ON A COMPACTE A LA DIMENSION NVAL (PEUT ETRE = 0)
         IF (NVAL.LT.DIMENS(4,K)) THEN
           DIMENS(4,K) = NVAL
           TROUVE(4,K) = 5
         ENDIF
       ENDIF
C
C CAS DES TABLEAUX DE SUBMITS JAMAIS AFFECTES -> DIMENSION = 0
       IF (UTINDX(4,K).AND.INDIC(4,K).EQ.3.AND.TROUVE(4,K).EQ.0.
     *     AND.DIMENS(4,K).GT.1) THEN
          DIMENS(4,K) = 0
          TROUVE(4,K) = 3
       ENDIF
C
 1195 CONTINUE
C
C ON EXECUTE LES COMMANDES ENREGISTREES AVANT LA FIN
      EXECMDMASC = .TRUE.
      LIGNE = 'NUL'
      DO 1196 K = 1,5
       VUCMDMASC0(K) = VUCMDMASC(K)
       VUCMDMASC(K) = .FALSE.
1196  CONTINUE
      DO 1198 K=1,5
        VUCMDMASC(K)=VUCMDMASC0(K)
        IF (VUCMDMASC(K).AND.(.NOT.(ERREUR))) THEN
          CALL CMDMASC (ICOL,LIGNE,ADRESS,DIMENS,TROUVE,MOTCLE,NMOT,
     *          MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT,INDIC,SIZE,
     *          UTINDX,DYNAM,VUCMDMASC,EXECMDMASC,NFICDA,NMAXR)
          VUCMDMASC(K) = .FALSE.
        ENDIF
 1198 CONTINUE
C
C
C  RECHERCHE DES MOTS-CLES OBLIGATOIRES QUI N'ONT PAS ETE LUS :
C
      WRITE(LU,*) ' '
C
      DO 1200  K = 1 , 4
      DO 1201 INDX = 1 , NMAXR(K)
        IF (UTINDX(K,INDX)) THEN
          IF (TROUVE(K,INDX).EQ.0) THEN
C
C SI PAS DE DEFAUT ET RIEN DANS CAS, POUR LES TABLEAUX, DIMENS = 0
            IF (DIMENS(K,INDX).NE.1) THEN
              IF (DYNAM) DIMENS(K,INDX) = 0
            ELSE
              WRITE(LU,*)'----------------------------------------'
              ARRET= .TRUE.
      IF(LNG.EQ.1) WRITE(LU,1101) MOTCLE(K,INDX)(1:SIZE(K,INDX))
      IF(LNG.EQ.2) WRITE(LU,1102) MOTCLE(K,INDX)(1:SIZE(K,INDX))
 1101         FORMAT(1X,'ATTENTION, LE MOT-CLE :',1X,A,/,1X,
     *        'N''A PAS RECU DE VALEUR')
 1102         FORMAT(1X,'BEWARE, THE KEY-WORD:',1X,A,/,1X,
     *        'HAS BEEN GIVEN NO VALUE')
            ENDIF
          ENDIF
        ENDIF
1201  CONTINUE
1200  CONTINUE
C
1300  CONTINUE
      IF(ARRET) THEN
        WRITE(LU,*)  ' '
        IF(LNG.EQ.1) WRITE(LU,*) 'INTERRUPTION DU SOUS-PROGRAMME DAMOC'
        IF(LNG.EQ.2) WRITE(LU,*) 'DAMOC IS STOPPED'
        STOP 'ERREUR DAMOCLES 5'
      ENDIF
C
      RETURN
C
C TRAITEMENT DES ERREURS D'ORDRE DE DECLARATION DANS LE DICOMASC
C
1500  ERREUR=.TRUE.
      WRITE(LU,'(/,1X,A72,/)') LIGNE
      IF (LNG.EQ.1) THEN
        WRITE(LU,*) 'A LA LIGNE ',NLIGN,
     *               ', ORDRE DE DEFINITION OBLIGATOIRE NON RESPECTE'
        WRITE(LU,*)
        WRITE(LU,*) 'L''ORDRE ATTENDU EST LE SUIVANT :'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'AT LINE ',NLIGN,', PRIORITY ORDER NOT RESPECTED'
        WRITE(LU,*)
        WRITE(LU,*) 'EXPECTED ORDER IS :'
      ENDIF
      WRITE(LU,*) 'NOM, TYPE, INDEX, (TAILLE), (SUBMIT), (DEFAUT)'
      GOTO 900
C
      END
