                        SUBROUTINE INFLUMASC
C                       ********************
C
     *( ICOL   , LIGNE  , DEFATT , TROUVE , LUIGN , MOTCLE , SIZE,
     *  MOTIGN , LONIGN , NMAXR  , NFICDA , GESTD )
C
C***********************************************************************
C DAMOCLES VERSION 5.0     16/08/94  O. QUIQUEMPOIX (LNH)   30 87 78 70
C
C Copyright EDF 1994
C
C
C***********************************************************************
C
C FONCTION  : DECODE LE CHAMP SUBMIT A PARTIR DE LA COLONNE
C             ICOL+1 DE LA LIGNE COURANTE. TESTE LA PRESENCE DES 4
C             CHAMPS LUS. RECONNAIT LE CHAMP2.
C             AVANCE LE POINTEUR ICOL SUR LE DERNIER CARACTERE DECODE.
C
C             PRESENTATION DES CHAMPS DES SUBMITS :
C
C               SUBMIT = 'chp1;chp2;chp3;chp4'
C
C               * CHP1 INDIQUE LE TYPE POUR LE LANCEUR POUR LE CODE :
C                     - IN   : ENTREE
C                     - OUT  : SORTIE
C                     - LIB  : BIBILIOTHEQUE
C                     - QSUB : COMMANDE CRAY
C                     - DIC  : DICTIONNAIRE
C                     - CAS  : FICHIER DE DONNEES
C                     - FORTRAN : REPERTOIRE CONTENANT LES FORTRANS
C                     - DIROUT : REPERTOIRE DE SORTIE
C                     - ACCT : COMMANDE CRAY
C                     - USER : COMMANDE CRAY ...
C
C                 NB :
C                     - NUL  : REMPLACE LES CHAMPS IN, OUT, FORTRAN ET
C                              DIROUT. LE MODE DEVIENT FORCE .
C                              CECI EST FAIT SI LA VARIABLE GESTD DU
C                              GESTIONNAIRE D'ETUDES EST TRUE.
C
C               * CHP2 INDIQUE LE COMPORTEMENT POUR DAMOCLES :
C                     - OPT : AFFECTATION OPTIONNELLE
C                     - REQ : AFFECTATION OBLIGATOIRE
C                     - FOR : AFFECTATION FORCEE
C
C               * CHP3 INDIQUE LE NOM DU FICHIER POUR LE GESTIONAIRE
C                 D'ETUDES :
C                     - *.CL : FICHIER DE CONDITIONS AUX LIMITES ...
C
C               * CHP4 INDIQUE AU LANCEUR EN FONCTION DU CHP1 :
C                     - L'ALLOCATION POUR LES FICHIERS
C                     - LES COMMANDES CRAY
C                     - LES BIBLIOTHEQUES ...
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !  ICOL          !<-->! POSITION COURANTE DU POINTEUR DANS LA LIGNE  !
C !  LIGNE         !<-->! LIGNE EN COURS DE DECODAGE                   !
C !  DEFATT        !<-- ! TABLEAU DES SUBMITS PAR DEFAUT               !
C !  TROUVE        !<-->! INDICATEUR D'ETAT DES MOTS CLES              !
C !                !    ! = 0 : AUCUNE VALEUR TROUVEE                  !
C !                !    ! = 1 : VALEUR PAR DEFAUT TROUVEE              !
C !                !    ! = 2 : VALEUR TROUVEE (FICHIER DE DONNEES)    !
C !                !    ! = 3 : AUCUNE VALEUR TROUVEE (OPTIONNELLE)    !
C !                !    ! = 5 : TABLEAU DE MOTS A SUBMIT COMPACTE      !
C !                !    ! = 6 : MOT CLE A SUBMIT FORCE NON AFFECTE     !
C !                !    ! = 7 : MOT CLE A SUBMIT FORCE AFFECTE (DICO)  !
C !                !    ! = 8 : MOT CLE A SUBMIT FORCE AFFECTE (CAS)   !
C !                !    ! = 9 : FICHIER DICO : SUBMIT + VALEUR LANCEUR !
C !                !    ! =10 : FICHIER CAS  : SUBMIT + VALEUR LANCEUR !
C !  LUIGN         ! -->! LOGIQUE POUR LES MOTS A NE PAS CLASSER       !
C !  MOTCLE        ! -->! TABLEAU DES MOTS CLES ACTIFS                 !
C !  SIZE          ! -->! TABLEAU DES LONGUEURS DES MOTS CLES          !
C !  MOTIGN        ! -->! TABLEAU DES MOTS CLES DUS A EDAMOX A IGNORER !
C !  LONIGN        ! -->! TABLEAU DES LONGUEURS DES MOTS EDAMOX        !
C !  NMAXR         ! -->! TABLEAU DES INDEX MAXIMUM REELS PAR TYPES    !
C !  NFICDA        ! -->! NUMERO DE CANAL DU FICHIER DES DONNEES       !
C !  GESTD         ! -->! LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES !
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
C !  . PARAM       ! -->! NOM DU MOT CLE EN COURS                      !
C !                !    !                                              !
C !    DCNGE       !    !                                              !
C !  . INDX        ! -->! INDEX DU MOT CLE EN COURS                    !
C !  . NTYP        ! -->! TYPE DU MOT CLE EN COURS                     !
C !  . ITAI        ! -->! TAILLE DU MOT CLE EN COURS                   !
C !  . LONGU       ! -->! LONGUEUR DU MOT CLE EN COURS                 !
C !  . NMOT        ! -->! TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE      !
C !  . DEFLU       ! -->! NOMBRE DE VALEURS LUES POUR LE MOT CLE       !
C !                !    !                                              !
C !________________!____!______________________________________________!
C
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C     - PORTABILITE          :  IBM,CRAY,HP,SUN
C
C     - PRECAUTIONS D'EMPLOI :  SI LE PREMIER CHAMP EST INCONNU, ON NE
C                               FAIT AUCUN TEST SAUF SUR LE DEUXIEME
C                               QUI CARACTERISE LE COMPORTEMENT DU MOT
C                               CLE POUR DAMOCLES. CECI PERMET D'ETENDRE
C                               LA COMPATIBILITE DE DAMOCLES SANS
C                               MODIFIER DIRECTEMENT LE FORTRAN
C
C     - APPELE PAR :            DAMOC
C
C     - SOUS-PROGRAMME APPELE : MAJUSMASC
C
C     - FONCTIONS APPELEES :    CARLUMASC,NEXTMASC,PRECARMASC,LONGLUMASC
C
C***********************************************************************
C
      IMPLICIT NONE
C
      EXTERNAL NEXTMASC,PRECARMASC,CARLUMASC,LONGLUMASC
C
      INTEGER       TROUVE(4,*),ICOL,NMAXR(4),NFICDA,SIZE(4,*)
      INTEGER       LONIGN(100)
      LOGICAL       LUIGN,GESTD
      CHARACTER*72  MOTIGN(100),MOTCLE(4,*)
      CHARACTER*144 DEFATT(*)
      CHARACTER*(*) LIGNE
C
      INTEGER       NEXTMASC,PRECARMASC,LONGLUMASC
      CHARACTER*144 CARLUMASC
C
      INTEGER       LNG,LU
      INTEGER       INDX,NTYP,ITAI,LONGU,NMOT(4),DEFLU
      INTEGER       NLIGN,LONGLI
      LOGICAL       ERREUR,RETOUR
      CHARACTER*72  PARAM
C
C-----------------------------------------------------------------------
C
      INTEGER       NBCHP1
      PARAMETER (NBCHP1=12)
C
      INTEGER       I,LCAR,ICOLA,JCOLA,CHAMP(4),LGA,II
      INTEGER       LGMOTG(NBCHP1),GECHP1(NBCHP1)
      CHARACTER*1   PTVIRG,QUOTE,GUILLT
      CHARACTER*72  MESERR(2*NBCHP1)
      CHARACTER*10  MOTCH1(NBCHP1)
      CHARACTER*144 NULATT,ANALYS,FIELD,FIELD0
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCNGE  / INDX,NTYP,ITAI,LONGU,NMOT,DEFLU
      COMMON / DCNGEC / PARAM
      COMMON / DCRARE / ERREUR,RETOUR
      COMMON / DCMLIG / NLIGN,LONGLI
C
C-----------------------------------------------------------------------
C
C ************** BASE DE DONNEES DU SOUS PROGRAMME ******************
C
C DEFINITION DES CHAMPS 1
      DATA MOTCH1 /'IN','OUT','CAS','DIC','QSUB','LIB','FORTRAN',
     *            'DIROUT','USER','ACCT','PRE','POST'/
C LONGUEURS DES CHAINES DES CHAMPS 1 DEFINIS AU DESSUS
      DATA LGMOTG /2,3,3,3,4,3,7,6,4,4,3,4/
C A PASSER EN 'nul;for' SI GESTD=.TRUE. ? : 1-OUI, 0-NON
      DATA GECHP1 /1,1,0,0,0,0,1,1,0,0,0,0/
C NUMEROS DES CHAMPS A DONNER A CES MOTS --> MESSAGES D'ERREUR
C      DATA NOCHMP /1,2,3,4,5,6,7,8,9,10,11,12/
C MESSAGES D'ERREUR ASSOCIES AUX NUMEROS DE CHAMPS
      DATA MESERR /
     * 'PAS D''ALLOCATION DE FICHIER D''ENTREE !!',
     * 'NO ALLOCATION FOR INPUT FILE !!',
     * 'PAS D''ALLOCATION DE FICHIER DE SORTIE !!',
     * 'NO ALLOCATION FOR OUTPUT FILE !!',
     * 'PAS D''ALLOCATION POUR LE FICHIER CAS !!',
     * 'NO ALLOCATION FOR THE STEERING FILE !!',
     * 'PAS D''ALLOCATION POUR LE DICTIONNAIRE !!',
     * 'NO ALLOCATION FOR THE DICTIONARY !!',
     * 'PAS DE COMMANDE CRAY !!','NO INSTRUCTION FOR CRAY !!',
     * 'PAS DE LIBRAIRIE !!', 'NO LIBRARY !!',
     * 'PAS DE VALEUR POUR LE REPERTOIRE FORTRAN !!',
     * 'NO VALUE FOR THE FORTRAN DIRECTORY !!',
     * 'PAS DE VALEUR POUR LE REPERTOIRE DE SORTIE !!',
     * 'NO VALUE FOR THE OUTPUT DIRECTORY !!',
     * 'PAS DE COMMANDE CRAY !!','NO INSTRUCTION FOR CRAY !!',
     * 'PAS DE COMMANDE CRAY !!','NO INSTRUCTION FOR CRAY !!',
     * 'PAS DE COMMANDE PRE !!','NO INSTRUCTION FOR PRE !!',
     * 'PAS DE COMMANDE POST !!','NO INSTRUCTION FOR POST !!'
     * /
C
C***********************************************************************
C                                    MARQUAGE RCS ET SCCS
C
C***********************************************************************
C
C  INITIALISATIONS
C
      PTVIRG = ';'
      QUOTE  = ''''
      GUILLT = '"'
      DEFLU  = 0
C
100   DEFLU = DEFLU +1
      IF(.NOT.(LUIGN)) THEN
        DEFATT(DEFLU)=CARLUMASC(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,
     *            MOTIGN,LONIGN,NMAXR,NFICDA,LEN(DEFATT(DEFLU)))
      ELSE
        NULATT = CARLUMASC(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,
     *            MOTIGN,LONIGN,NMAXR,NFICDA,LEN(NULATT))
      ENDIF
C
      ICOL = NEXTMASC(ICOL+1,LIGNE)
C
      IF (LIGNE(ICOL:ICOL) .EQ. PTVIRG) GO TO 100
C
C SI A IGNORER, PAS D'ANALYSE ...
      IF (LUIGN) GO TO 1300
C
      IF (DEFLU .LT. ITAI) THEN
         ERREUR = .TRUE.
         IF(LNG.EQ.1) THEN
           WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
           WRITE(LU,*)'PAS ASSEZ DE VALEURS DEFINIES POUR SUBMIT...'
         ELSEIF(LNG.EQ.2) THEN
           WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
           WRITE(LU,*)'NOT ENOUGH DATAS DEFINED FOR SUBMIT...'
         ENDIF
         WRITE(LU,*)' '
         GO TO 1300
      ENDIF
C
C  EXAMEN DES CHAMPS DES SUBMITS
C
      DO 1140 I = 1 , DEFLU
 200     ICOLA = 0
         ANALYS = DEFATT(I)
C
C   *** CHAMP 1 ***
C
         LGA = MAX(LONGLUMASC(ANALYS),1)
         IF (ANALYS(ICOLA+1:ICOLA+1).EQ.';') THEN
           LCAR = 0
         ELSE
           JCOLA = PRECARMASC(ICOLA+1,ANALYS,';',';',';')
           LCAR = LONGLUMASC(ANALYS(ICOLA+1:JCOLA-1))
           IF (LCAR.GT.0) THEN
             FIELD0 = CARLUMASC(LCAR,ICOLA,ANALYS,GUILLT,MOTCLE,SIZE,
     *                 MOTIGN,LONIGN,NMAXR,NFICDA,LEN(FIELD0))
             LCAR = LONGLUMASC(FIELD0(1:LCAR))
          ENDIF
         ENDIF
         IF (LCAR.LE.0) THEN
           IF (LNG.EQ.1) THEN
              WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
              WRITE(LU,*)'SUBMIT INCORRECT : ',ANALYS(1:LGA)
              WRITE(LU,*)'PAS DE PREMIER CHAMP !!'
           ELSEIF (LNG.EQ.2) THEN
              WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
              WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
              WRITE(LU,*)'NO FIRST FIELD !!'
           ENDIF
           ERREUR = .TRUE.
           GO TO 1300
         ENDIF
         IF (ERREUR) GO TO 1300
         FIELD = FIELD0
         CALL MAJUSMASC(FIELD)
C
         CHAMP(1)=100
         DO 300 II=1,NBCHP1
         IF (LCAR.EQ.LGMOTG(II).AND.
     *       FIELD(1:LCAR).EQ.MOTCH1(II)(1:LCAR)) THEN
            IF (GESTD.AND.GECHP1(II).EQ.1) THEN
              DEFATT(I) = 'nul;for'//DEFATT(I)(JCOLA+4:MAX(LGA,JCOLA+4))
              GO TO 200
            ELSE
C             CHAMP(1)=NOCHMP(II)
              CHAMP(1)=II
              GOTO 400
            ENDIF
         ENDIF
300      CONTINUE
C
C   *** CHAMP 2 ***
C
400      ICOLA = JCOLA
         IF (ICOLA.GE.LONGLI) THEN
           LCAR = 0
         ELSEIF (ANALYS(ICOLA+1:ICOLA+1).EQ.';') THEN
           LCAR = 0
         ELSE
           JCOLA = PRECARMASC(ICOLA+1,ANALYS,';',';',';')
           LCAR = LONGLUMASC(ANALYS(ICOLA+1:JCOLA-1))
           IF (LCAR.GT.0) THEN
             FIELD0 = CARLUMASC(LCAR,ICOLA,ANALYS,GUILLT,MOTCLE,SIZE,
     *                 MOTIGN,LONIGN,NMAXR,NFICDA,LEN(FIELD0))
             LCAR = LONGLUMASC(FIELD0(1:LCAR))
           ENDIF
         ENDIF
         IF (LCAR.LE.0) THEN
           IF (LNG.EQ.1) THEN
              WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
              WRITE(LU,*)'SUBMIT INCORRECT : ',ANALYS(1:LGA)
              WRITE(LU,*)'PAS DE DEUXIEME CHAMP !!'
           ELSEIF (LNG.EQ.2) THEN
              WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
              WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
              WRITE(LU,*)'NO SECOND FIELD !! '
           ENDIF
           ERREUR = .TRUE.
           GO TO 1300
         ENDIF
C
         IF (ERREUR) GO TO 1300
         FIELD = FIELD0
         CALL MAJUSMASC(FIELD)
C
         IF (FIELD(1:3).EQ.'OPT') THEN
            CHAMP(2) = 1
         ELSEIF (FIELD(1:3).EQ.'REQ') THEN
            CHAMP(2) = 2
         ELSEIF (FIELD(1:3).EQ.'FOR') THEN
            CHAMP(2) = 3
         ELSE
            ERREUR = .TRUE.
            IF (LNG.EQ.1) THEN
              WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
              WRITE(LU,*)'SUBMIT INCORRECT : ',ANALYS(1:LGA)
              WRITE(LU,*)'DEUXIEME CHAMP INCONNU : ',FIELD0(1:LCAR)
            ELSEIF (LNG.EQ.2) THEN
              WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
              WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
              WRITE(LU,*)'SECOND FIELD UNKNOWN : ',FIELD0(1:LCAR)
            ENDIF
            GO TO 1300
         ENDIF
C
C ON AFFECTE A TROUVE LA VALEUR INITIALE EN FONCTION DE CHAMP(1)
         IF (ITAI.LE.1.AND.I.LE.MAX(ITAI,1)) THEN
          IF (CHAMP(2) .EQ. 1) TROUVE(NTYP,INDX)=3
          IF (CHAMP(2) .EQ. 3) TROUVE(NTYP,INDX)=6
C         IF (CHAMP(1) .EQ. 3) TROUVE(NTYP,INDX)=10
          IF (CHAMP(1) .EQ. 4) TROUVE(NTYP,INDX)=9
        ENDIF
C
C SI LE PREMIER CHAMP N'EST PAS CONNU, ON IGNORE LA SUITE
C POUR ETRE COMPATIBLE AVEC DES EVOLUTIONS DU LANCEUR
        IF (CHAMP(1).EQ.100) GO TO 1140
C
C   *** CHAMP 3 ***
C
        ICOLA = JCOLA
        IF (JCOLA.GE.LONGLI) THEN
           IF (LNG.EQ.1) THEN
              WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
              WRITE(LU,*)'SUBMIT INCORRECT : ',ANALYS(1:LGA)
              WRITE(LU,*)'PAS DE TROISIEME CHAMP !!'
           ELSEIF (LNG.EQ.2) THEN
              WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
              WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
              WRITE(LU,*)'NO THIRD FIELD !! '
           ENDIF
           ERREUR = .TRUE.
           GO TO 1300
        ENDIF
        JCOLA = PRECARMASC(ICOLA+1,ANALYS,';',';',';')
C
C   *** CHAMP 4 ***
C
        ICOLA = JCOLA
        IF (ICOLA.GE.LONGLI) THEN
          LCAR = 0
        ELSEIF (ANALYS(ICOLA+1:ICOLA+1).EQ.';') THEN
           LCAR = 0
        ELSE
           JCOLA = PRECARMASC(ICOLA+1,ANALYS,';',';',';')
           LCAR = LONGLUMASC(ANALYS(ICOLA+1:JCOLA-1))
        ENDIF
        IF (LCAR.LE.0) THEN
             IF (LNG.EQ.1) THEN
              WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
              WRITE(LU,*)'SUBMIT INCORRECT : ',ANALYS(1:LGA)
            ELSEIF (LNG.EQ.2) THEN
              WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
              WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
            ENDIF
            ERREUR = .TRUE.
C
C ECRITURE DU MESSAGE D'ERREUR CORRESPONDANT
            WRITE(LU,*) MESERR(2*(CHAMP(1)-1)+LNG)
            GO TO 1300
        ENDIF
1140  CONTINUE
C
C-----------------------------------------------------------------------
C
1300  CONTINUE
      RETURN
      END
