C                       *******************
                        SUBROUTINE DICOMASC
C                       *******************
C
     *( ITYP   , NUMERO , ILONG  , CHAINE , MOTCLE , NMOT   , MOTPRO ,
     *  LONPRO , SIZE   , UTINDX , LANGUE , AIDLNG , MOTIGN , NIGN   ,
     *  LUIGN  , TYPIGN , LONIGN , NFICDA , NBLANG , NMAXR )
C
C***********************************************************************
C DAMOCLES VERSION 5.0     16/08/94   J.M. HERVOUET (LNH)   30 87 80 18
C                                      A. YESSAYAN
C                                      L. LEGUE
C                          15/12/93    O. QUIQUEMPOIX (LNH)  30 87 78 70
C
C COPYRIGHT EDF 1994
C
C
C***********************************************************************
C
C FONCTION  : RECHERCHE D'UNE CHAINE DE CARACTERES DANS LE DICTIONNAIRE
C             DES NOMS DONNES PAR L'UTILISATEUR.
C
C             POUR LE FICHIER DICTIONNAIRE, ON CHERCHE PARMI LES MOTS
C             RESERVES
C
C             POUR LE FICHIER CAS, ON CHERCHE PARMI LES MOTS CLES ACTIFS
C             ET PARMI LES MOTS IGNORES DANS LE DICTIONNAIRE MAIS ECRITS
C             PAR EDAMOX
C
C             RETOURNE  1) LE TYPE DU MOT-CLE : ITYP .
C                       2) LE NUMERO D'ORDRE DE CE MOT-CLE PARMI CEUX
C                          DE SON TYPE  : NUMERO .
C                       3) AIDLNG = VRAI SI L'AIDE EST CELLE DE LA
C                          LANGUE CHOISIE
C                       4) LANGUE = VRAI SI ON A TROUVE UN MOT-CLE
C                          DANS UNE LANGUE DE 1 A 9, SINON = FAUX
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !  ITYP          !<-- ! TYPE DU MOT-CLE  :    1  ENTIER              !
C !                !    !                       2  REEL                !
C !                !    !                       3  LOGIQUE             !
C !                !    !                       4  CARACTERES          !
C !                !    !                       5  MOT RESERVE         !
C !                !    !                       0  MOT INCONNU         !
C !  NUMERO        !<-- ! ORDRE DU MOT-CLE PARMI CEUX DE SON TYPE      !
C !  ILONG         ! -->! LONGUEUR DE LA CHAINE A ANALYSER             !
C !  CHAINE        ! -->! CHAINE A ANALYSER                            !
C !  MOTCLE        ! -->! TABLEAU DES MOTS CLES ACTIFS                 !
C !  NMOT          !<-->! TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE      !
C !                !    !  NMOT(1) ENTIERS                             !
C !                !    !  NMOT(2) REELS                               !
C !                !    !  NMOT(3) LOGIQUES                            !
C !                !    !  NMOT(4) CARACTERES                          !
C !  MOTPRO        ! -->! TABLEAU DES MOTS CLES RESERVES AU PROGRAMME  !
C !  LONPRO        ! -->! LONGUEURS DES MOTS CLES DE MOTPRO            !
C !  SIZE          ! -->! TABLEAU DES LONGUEURS DES MOTS CLES          !
C !  UTINDX        ! -->! TABLEAU DE LOGIQUES D'UTILISATION DES INDEX  !
C !  LANGUE        !<-- ! LOGIQUE=.TRUE. SI LA CHAINE EST RECONNUE     !
C !  AIDLNG        !<-- ! LOGIQUE .TRUE. SI L'AIDE EST CELLE DE LNG    !
C !  MOTIGN        ! -->! TABLEAU DES MOTS CLES DUS A EDAMOX A IGNORER !
C !  NIGN          ! -->! NOMBRE DE MOTS CLES DUS A EDAMOX A IGNORER   !
C !  LUIGN         ! -->! LOGIQUE POUR LES MOTS A NE PAS CLASSER       !
C !  TYPIGN        ! -->! TABLEAU DES TYPES DES MOTS EDAMOX A IGNORER  !
C !  LONIGN        ! -->! TABLEAU DES LONGUEURS DES MOTS DE MOTIGN     !
C !  NFICDA        ! -->! NUMERO DE CANAL DU FICHIER DES DONNEES       !
C !  NBLANG        ! -->! NOMBRE DE LANGUES CONNUES                    !
C !  NMAXR         ! -->! TABLEAU DES INDEX MAXIMUM REELS PAR TYPES    !
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
C !  . NLIGN       ! -->! NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU!
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
C
C     - PRECAUTIONS D'EMPLOI:  L'AJOUT DE LANGUES DOIT SE TRADUIRE PAR
C                              L'INCREMENTATION DU NOMBRE DE BOUCLES DE
C                              RECHERCHE PARMI LES LANGUES DISPONIBLES
C                              (ICI, 2 LANGUES : F ET GB )
C
C     - APPELE PAR :           DAMOC
C
C     - PORTABILITE:           IBM,CRAY,HP,SUN
C
C     - DOCUMENTATION:
C
C***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER       NMOT(4),SIZE(4,*),ITYP,NUMERO,ILONG,NBLANG,NMAXR(4)
      INTEGER       NIGN,NFICDA,TYPIGN(100),LONIGN(100),LONPRO(15)
      LOGICAL       UTINDX(4,*),LANGUE,LUIGN,AIDLNG
      CHARACTER*(*) MOTCLE(4,*),MOTPRO(*),CHAINE
      CHARACTER*1   LNGPRO(9)
      CHARACTER*9   RUBPRO(5),MOTLNG
      CHARACTER*72  MOTIGN(100)
C
      INTEGER       LNG,LU
      INTEGER       NLIGN,LONGLI
      INTEGER       NFIC
      LOGICAL       ERREUR,RETOUR
C
C-----------------------------------------------------------------------
C
      INTEGER       INDX,LGRUB(5),I,K,LNGINT,VALNUM(5)
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR,RETOUR
      COMMON / DCMLIG / NLIGN,LONGLI
      COMMON / DCCHIE / NFIC
C
C-----------------------------------------------------------------------
C
      DATA LNGPRO /'1','2','3','4','5','6','7','8','9'/
      DATA RUBPRO /'NOM','DEFAUT','AIDE','CHOIX','RUBRIQUE'/
C NOMBRE DE LETTRES DANS LES NOMS DE RUBPRO
      DATA LGRUB  /3,6,4,5,8/
C CORRESPONDANCES ENTRE LES DATAS RUBPRO ET MOTPRO
      DATA VALNUM /1,5,6,7,8/
C
C***********************************************************************
C                                    MARQUAGE RCS ET SCCS
C
C***********************************************************************
C
C LANGUE SERT POUR LA LECTURE DU DICTIONNAIRE SEULEMENT. IL N'EST PAS
C UTILISE POUR LA LECTURE DU FICHIER UTILISATEUR
C
      LANGUE = .FALSE.
      AIDLNG = .FALSE.
C
      LNGINT = LNG - 1
C
C*******************************************************
C  1) RECHERCHE PARMI LES MOTS-CLES DE L'UTILISATEUR :
C*******************************************************
C
      IF (NFIC.EQ.NFICDA) THEN
        DO 100 ITYP = 1,4
          DO 110 INDX=1,NMAXR(ITYP)
            IF (UTINDX(ITYP,INDX)) THEN
              K=SIZE(ITYP,INDX)
! MODIF PU2017
!              IF(K.EQ.ILONG.AND.
!     *           CHAINE(1:K).EQ.MOTCLE(ITYP,INDX)(1:K)) THEN
              IF(K.EQ.ILONG) THEN
                IF (CHAINE(1:K).EQ.MOTCLE(ITYP,INDX)(1:K)) THEN
                  NUMERO=INDX
                  GO TO 1000
                ENDIF
              ENDIF
            ENDIF
 110      CONTINUE
 100    CONTINUE
C
C SINON, ON REGARDE SI C'EST UN MOT CLE EDAMOX D'INDEX = -1
C
        DO 900 I=1,NIGN
          IF (LONIGN(I).EQ.ILONG.AND.
     *        CHAINE(1:ILONG).EQ.MOTIGN(I)(1:ILONG)) THEN
             ITYP = TYPIGN(I)
              LUIGN = .TRUE.
             GO TO 1000
          ENDIF
900     CONTINUE
C
C FIN DE LA RECHERCHE DANS LES MOTS-CLES DE L'UTILISATEUR
       GO TO 910
      ENDIF
C
C
C*********************************************
C  2) RECHERCHE PARMI LES MOTS RESERVES :
C*********************************************
C
C  AIDLNG = LOGIQUE VRAI SI L'AIDE EST CELLE DE LA LANGUE CHOISIE
C
C SI C'EST UN MOT ANGLAIS, INUTILE DE LE CHERCHER PARMI LES FR
C CE QUI FAIT GAGNER 50 TESTS PAR MOTS POUR TELEMAC PAR EX
C EVALUE DONC A 6500 TESTS POUR TELEMAC
      IF (CHAINE(ILONG:ILONG).EQ.'1') GOTO 125
C
      DO 120 I=1,15
       IF (ILONG.EQ.LONPRO(I)) THEN
         IF (CHAINE(1:ILONG).EQ.MOTPRO(I)(1:ILONG)) THEN
C           SI 'AIDE' ET LNG=FRANCAIS, ON VA EDITER L'AIDE SI DOC
            IF (I.EQ.6 .AND. LNGINT .EQ. 0) AIDLNG = .TRUE.
            LANGUE = .TRUE.
            NUMERO = I
            ITYP   = 5
            GO TO 1000
         ENDIF
       ENDIF
120   CONTINUE
C
C  SINON ON RECHERCHE PARMI LES MOTS RESERVES POUR LES LANGUES
C     DIFFERENTES DU FRANCAIS. (MAX NBLANG LANGUES ET NBLANG<=10)
C
C LNG EST LE PARAMETRE DE LANGUE EXTERNE (1 = FRANCAIS, 2 = ANGLAIS ...)
C LNGINT EST LE PARAMETRE DE LANGUE INTERNE A DAMOCLE
C C'EST A DIRE (0 = FRANCAIS, 1 = ANGLAIS ...)
C
C  AIDLNG = NUMERO DE LA LIGNE D'AIDE DE LA LANGUE DESIREE
C
125   CONTINUE
      IF (NBLANG.GE.2) THEN
      DO 130 I=1,5
      DO 131 K=1,NBLANG-1
        IF (LGRUB(I)+1.EQ.ILONG) THEN
        MOTLNG = RUBPRO(I)(1:LGRUB(I))//LNGPRO(K)(1:1)
        IF (CHAINE(1:ILONG).EQ.MOTLNG(1:ILONG)) THEN
          NUMERO=VALNUM(I)
C
          IF (I.EQ.3 .AND. K.EQ.LNGINT) AIDLNG = .TRUE.
C
          ITYP = 5
C
C ON RENVOIE LANGUE = .TRUE. SEULEMENT POUR LES MOTS CLES DE DAMOCLE
C SAUF POUR 'AIDE' CAR LANGUE NE SERT PAS.
C CAR ON NE FAIT PAS LE MEME TRAITEMENT EN FONCTION DE LA LANGUE CHOISIE
          IF (K.EQ.LNGINT.AND.I.GE.1.AND.I.LE.3) LANGUE = .TRUE.
          GO TO 1000
        ENDIF
        ENDIF
131   CONTINUE
130   CONTINUE
      ENDIF
C
C  6) ERREUR : ON N'A PAS TROUVE LE MOT-CLE CORRESPONDANT
C
 910  CONTINUE
      ERREUR = .TRUE.
      ITYP = 0
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'*************************************************'
        WRITE(LU,*)'A LA LIGNE ',NLIGN,' LE MOT CLE SUIVANT : ',
     *              CHAINE(1:ILONG),' EST INCONNU ...'
        WRITE(LU,*)'*************************************************'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*)'*************************************************'
        WRITE(LU,*)'AT LINE    ',NLIGN,' THE KEY-WORD       : ',
     *              CHAINE(1:ILONG),' IS UNKNOWN...'
        WRITE(LU,*)'*************************************************'
      ENDIF
C
1000  CONTINUE
C
      RETURN
      END
