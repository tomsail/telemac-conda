                        INTEGER FUNCTION INTLUMASC
C                       **************************
C
     *( ICOL , LIGNE )
C
C***********************************************************************
C DAMOCLES VERSION 5.0     30/09/93  J.M. HERVOUET    (LNH)  30 87 80 18
C                                      A. YESSAYAN
C                          15/12/93    O. QUIQUEMPOIX (LNH)  30 87 78 70
C
C COPYRIGHT EDF 1994
C
C***********************************************************************
C
C FONCTION: DECODE UN ENTIER A PARTIR DE LA COLONNE ICOL+1 DE LA LIGNE.
C           AVANCE LE POINTEUR ICOL SUR LE DERNIER CARACTERE DECODE.
C           SI LA CHAINE N'EST PAS TERMINEE, RECHERCHE SUR LA LIGNE
C           SUIVANTE.
C           AVANCE LE POINTEUR ICOL SUR LE DERNIER CARACTERE DECODE.
C           OU A ICOL=0 SI ON A LU LA LIGNE SUIVANTE
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
C     - PORTABILITE          :  IBM,CRAY,HP,SUN
C
C     - PRECAUTIONS D'EMPLOI : SI LA VALEUR LUE N'EST PAS UN ENTIER, IL
C                              Y A UN RISQUE D'ERREUR NON CONTROLEE PAR
C                              LE PROGRAMME.
C
C     - APPELE PAR :           DAMOC
C
C     - FONCTIONS APPELEES :   NEXTMASC,PREVALMASC
C
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER          ICOL
      CHARACTER*(*)    LIGNE
C
      INTEGER          NEXTMASC,PREVALMASC
      EXTERNAL NEXTMASC,PREVALMASC
C
      INTEGER          LNG,LU
      INTEGER          NLIGN,LONGLI
      INTEGER          NFIC
      LOGICAL          ERREUR , RETOUR
C
C-----------------------------------------------------------------------
C
      INTRINSIC DLOG10,DBLE,INT,CHAR
C
      INTEGER          I1,I2,ILONG,ISIGNE,IVAL,JD1,I3
      LOGICAL          LUFIC,LISUIV
      CHARACTER*1      CDEB,TABUL
      CHARACTER*3      LLONG
      CHARACTER*72     LIGNE2,FORMA
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR , RETOUR
      COMMON / DCMLIG / NLIGN , LONGLI
      COMMON / DCCHIE / NFIC
C
C***********************************************************************
C
      LUFIC = .FALSE.
      LISUIV = .FALSE.
      TABUL = CHAR(9)
C
      I1     = NEXTMASC( ICOL+1 , LIGNE )
C
C        //// DECODAGE EVENTUEL DU SIGNE ////
C
      IF ( LIGNE(I1:I1).EQ.'-' ) THEN
           ISIGNE = -1
           I1     =   NEXTMASC ( I1+1      , LIGNE )
      ELSE IF ( LIGNE(I1:I1).EQ.'+' ) THEN
           ISIGNE = +1
           I1     =   NEXTMASC ( I1+1      , LIGNE )
      ELSE
           ISIGNE = +1
      ENDIF
C
C        //// RECHERCHE DU PREMIER BLANC APRES LE NOMBRE  ////
C                       OU D'UN SEPARATEUR ;
C
      I2 = PREVALMASC (  I1  , LIGNE ,  ' ' , ';' , TABUL)
C
C     CAS OU L'ENTIER NE SE TERMINE PAS SUR LA LIGNE
C
      IF (I2.GT.LONGLI) THEN
         LUFIC=.TRUE.
         READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
         CDEB = LIGNE2(1:1)
         IF (CDEB.EQ.'0'.OR.CDEB.EQ.'1'.OR.CDEB.EQ.'2'.OR.
     *       CDEB.EQ.'3'.OR.CDEB.EQ.'4'.OR.CDEB.EQ.'5'.OR.
     *       CDEB.EQ.'6'.OR.CDEB.EQ.'7'.OR.CDEB.EQ.'8'.OR.
     *       CDEB.EQ.'9'.OR.CDEB.EQ.'.') THEN
            LISUIV = .TRUE.
            I3=1
            I3=PREVALMASC(I3,LIGNE2 , ' ' , ';', TABUL)
            IF (I1.LE.LONGLI) THEN
              LIGNE = LIGNE(I1:LONGLI)//LIGNE2(1:I3)
            ELSE
              LIGNE =LIGNE2(1:I3)
            ENDIF
            I2 = LONGLI-I1+1+I3
            I1 = 1
         ENDIF
       ENDIF
       GOTO 910
C
 900  CONTINUE
      RETOUR = .TRUE.
 910  CONTINUE
C     ON ACCEPTE LE CAS OU UN UTILISATEUR ECRIT UN ENTIER SOUS
C     FORME REELLE AVEC UN POINT A LA FIN.
      IF(LIGNE(I2-1:I2-1).EQ.'.') THEN
        LIGNE(I2-1:I2-1)=' '
        I2 = I2 - 1
      ENDIF
C
C     ILONG : LONGUEUR DE L'ENTIER
      ILONG  = I2 - I1
C
C        //// FORMAT DE DECODAGE ////
C
      JD1 = 3 - INT(DLOG10(DBLE(ILONG)))
      WRITE ( LLONG , '(I3)' ) ILONG
C
      IF(I1.EQ.1) THEN
         WRITE (FORMA , 1101 )  LLONG(JD1:3)
      ELSE
         WRITE (FORMA , 1100 )  I1-1 , LLONG(JD1:3)
      ENDIF
C
C        ////  DECODAGE ////
C
      READ  ( LIGNE , FORMA , ERR=995 ) IVAL
      INTLUMASC = ISIGNE * IVAL
C
C        //// MISE A JOUR DU POINTEUR ////
C
      IF (LUFIC) THEN
        NLIGN = NLIGN + 1
        LIGNE = LIGNE2
        IF (LISUIV) THEN
          ICOL = I3-1
        ELSE
          ICOL = 0
        ENDIF
      ELSE
        ICOL = I2 - 1
      ENDIF
C
1100  FORMAT('(',I3,'X,I',A,')')
1101  FORMAT('(I',A,')')
C
C-----------------------------------------------------------------------
C
      RETURN
C
C TRAITEMENT DES ERREURS DUES AU READ INTERNE POUR LA CONVERSION
C
995   CONTINUE
      IF(LNG.EQ.1) WRITE(6,996) NLIGN
      IF(LNG.EQ.2) WRITE(6,1996) NLIGN
      WRITE(6,*) LIGNE
996   FORMAT(1X,'ERREUR LIGNE ',1I6,', UN ENTIER EST ATTENDU : ',/)
1996  FORMAT(1X,'ERREUR LINE ',1I6,', INTEGER EXPECTED : ',/)
      ERREUR=.TRUE.
      RETURN
C
C TRAITEMENT DES ERREURS DE LECTURE DU FICHIER
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
