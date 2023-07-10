                       INTEGER FUNCTION PRECARMASC
C                      ***************************
C
     *( ICOL , LIGNE , CAR1 , CAR2 , CAR3 )
C
C***********************************************************************
C DAMOCLES VERSION 5.0     16/08/94   J.M. HERVOUET (LNH)   30 87 80 18
C                                       A. YESSAYAN
C                          15/12/93     O. QUIQUEMPOIX (LNH) 30 87 78 70
C
C Copyright EDF 1994
C
C
C***********************************************************************
C
C FONCTION  : RETOURNE L'INDICE DE COLONNE DU PREMIER CARACTERE CAR
C             DE LA LIGNE (MEME APRES UN / DE COMMENTAIRE).
C             RETOURNE LA LONGUEUR MAXIMALE DE LA LIGNE SI LE CARACTERE
C             N'EST PAS TROUVE.
C
C             CETTE FONCTION EST UTILISEE POUR TROUVER LA FIN D'UNE
C             CHAINE DE CARACTERES . CETTE CHAINE PEUT CONTENIR DES
C             SIGNES '/', C'EST POURQUOI ON N'UTILISE PAS PREVAL
C             QUI SAUTE LES ZONES DE COMMENTAIRES.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !  ICOL          ! -->! POSITION COURANTE DU POINTEUR DANS LA LIGNE  !
C !  LIGNE         ! -->! LIGNE EN COURS DE DECODAGE                   !
C !  CAR1,CAR2,CAR3! -->! CARACTERES RECHERCHES DANS LA LIGNE          !
C !                !    !                                              !
C !________________!____!______________________________________________!
C !                !    !                                              !
C !   /COMMON/     !    !                                              !
C !                !    !                                              !
C !    DCMLIG      !    !                                              !
C !  . NLIGN       ! -->! NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU!
C !  . LONGLI      ! -->! LONGUEUR DES LIGNES                          !
C !________________!____!______________________________________________!
C
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C     - PORTABILITE : IBM,CRAY,HP,SUN
C
C     - APPELE PAR :  AIDELU,CARLU,IGNORE,INFLU,LOGLU
C
C     - REMARQUE :    OPTIMISATION EN ENVOYANT CAR1, CAR2, CAR3 DANS
C                     L'ORDRE LE PLUS PROBABLE
C
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER       ICOL
      CHARACTER*1   CAR1,CAR2,CAR3
      CHARACTER*(*) LIGNE
C
      INTEGER       LONGLI,NLIGN
C
C-----------------------------------------------------------------------
C
      INTEGER       K
C
C-----------------------------------------------------------------------
C
      COMMON / DCMLIG / NLIGN,LONGLI
C
C***********************************************************************
C                                    MARQUAGE RCS ET SCCS
C
C***********************************************************************
C
      PRECARMASC = LONGLI
C
      DO 100 K = ICOL,LONGLI
      IF (LIGNE(K:K).EQ.CAR1.OR.LIGNE(K:K).EQ.CAR2.OR.
     *    LIGNE(K:K).EQ.CAR3) THEN
        PRECARMASC = K
        GO TO 1000
      ENDIF
100   CONTINUE
C
      PRECARMASC=LONGLI+1
C
1000  CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
