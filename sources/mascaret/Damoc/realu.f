                        DOUBLE PRECISION FUNCTION REALUMASC
C                       ***********************************
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
C FONCTION  : DECODE UN REEL A PARTIR DE LA COLONNE ICOL+1 DE LA LIGNE.
C             AVANCE LE POINTEUR ICOL SUR LE DERNIER CARACTERE DECODE.
C             ACCEPTE LE FORMAT F OU LE FORMAT E.
C             ACCEPTE LES REELS SANS VIRGULES, ET LES , POUR LES .
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
C     - PORTABILITE :          IBM,CRAY,HP,SUN
C
C     - PRECAUTIONS D'EMPLOI : SI LA VALEUR LUE N'EST PAS UN REEL, IL
C                              Y A UN RISQUE D'ERREUR NON CONTROLEE PAR
C                              LE PROGRAMME.
C
C     - APPELE PAR :           DAMOC
C
C     - FONCTIONS APPELEES :   NEXTMASC,PREVALMASC
C
C-----------------------------------------------------------------------
C
C
      IMPLICIT NONE
C
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
      INTEGER          I,I1,I2,ILONG,IPOINT,IFDECI,ILDECI,JD1,JD2,I3
      LOGICAL          FORMAE,LUFIC,LISUIV,VUPOIN
      CHARACTER*1      CODE,CDEB,CDEB2,TABUL
      CHARACTER*3      LLONG,LLDECI
      CHARACTER*72     FORMA,LIGNE2
      DOUBLE PRECISION RSIGNE , RVAL
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
      VUPOIN = .FALSE.
      TABUL = CHAR(9)
C
      I1     = NEXTMASC( ICOL+1 , LIGNE )
C
C   ////  DETERMINATION DU CODE DU FORMAT : F OU E  ////
C
      FORMAE = .FALSE.
C
C        //// DECODAGE EVENTUEL DU SIGNE             ////
C
      RSIGNE = +1.D0
      IF ( LIGNE(I1:I1).EQ.'-' ) THEN
           RSIGNE = -1.D0
           I1     =   NEXTMASC ( I1+1      , LIGNE )
      ELSE IF ( LIGNE(I1:I1).EQ.'+' ) THEN
           RSIGNE = +1.D0
           I1     =   NEXTMASC ( I1+1      , LIGNE )
      ENDIF
C
C        //// RECHERCHE DU PREMIER BLANC APRES LE NOMBRE  ////
C                       OU D'UN SEPARATEUR ";"
C
      I2     = PREVALMASC (  I1  , LIGNE ,  ' ' , ';' ,TABUL)
C
C     CAS OU L'ENTIER NE SE TERMINE PAS SUR LA LIGNE
C
      IF (I2.GT.LONGLI) THEN
         LUFIC=.TRUE.
         READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
         CDEB = LIGNE2(1:1)
         CDEB2 = LIGNE2(2:2)
C
         IF ((CDEB.EQ.'0'.OR.CDEB.EQ.'1'.OR.CDEB.EQ.'2'.OR.
     *        CDEB.EQ.'3'.OR.CDEB.EQ.'4'.OR.CDEB.EQ.'5'.OR.
     *        CDEB.EQ.'6'.OR.CDEB.EQ.'7'.OR.CDEB.EQ.'8'.OR.
     *        CDEB.EQ.'9'.OR.CDEB.EQ.'.'.OR.CDEB.EQ.'+'.OR.
     *        CDEB.EQ.'-'.OR.CDEB.EQ.',')
C
     *      .OR.
C
C CAS OU CELA DEPEND DU DEUXIEME CARACTERE DE LA LIGNE SUIVANTE
C
     *      ((CDEB.EQ.'E'.OR.CDEB.EQ.'E'.OR.CDEB.EQ.'D'.OR.
     *        CDEB.EQ.'D')
     *      .AND.
     *      ( CDEB2.EQ.'0'.OR.CDEB2.EQ.'1'.OR.CDEB2.EQ.'2'.OR.
     *        CDEB2.EQ.'3'.OR.CDEB2.EQ.'4'.OR.CDEB2.EQ.'5'.OR.
     *        CDEB2.EQ.'6'.OR.CDEB2.EQ.'7'.OR.CDEB2.EQ.'8'.OR.
     *        CDEB2.EQ.'9'.OR.CDEB2.EQ.'+'.OR.CDEB2.EQ.'-'    )))
C
     *      THEN
C
            LISUIV = .TRUE.
            I3=1
            I3=PREVALMASC(I3,LIGNE2 , ' ' , ';' ,TABUL)
            IF (I1.LE.LONGLI) THEN
              LIGNE = LIGNE(I1:LONGLI)//LIGNE2(1:I3)
            ELSE
              LIGNE = LIGNE2(1:I3)
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
C
C     ILONG : LONGUEUR DU REEL
      ILONG  = I2 - I1
      IPOINT = I2 - 1
      IFDECI = I2 - 1
      DO 100 I = I1 , I2-1
C          ON ACCEPTE LES POINTS, LES VIRGULES
           IF ( LIGNE(I:I).EQ.'.' ) THEN
                IPOINT = I
                VUPOIN=.TRUE.
           ELSEIF ( LIGNE(I:I).EQ.',' ) THEN
                LIGNE(I:I)='.'
                IPOINT = I
                VUPOIN=.TRUE.
           ELSE IF (LIGNE(I:I).EQ.'E'.OR.LIGNE(I:I).EQ.'E' ) THEN
C          ON ACCEPTE LES FORMATS E , D
                FORMAE = .TRUE.
                IFDECI = I-1
           ELSE IF ( LIGNE(I:I).EQ.'D'.OR.LIGNE(I:I).EQ.'D') THEN
                LIGNE(I:I)='E'
                FORMAE = .TRUE.
                IFDECI = I-1
           ENDIF
  100 CONTINUE
C
C        //// LONGUEUR DE LA PARTIE FRACTIONNAIRE ///
C
      IF (VUPOIN) THEN
        ILDECI = IFDECI - IPOINT
      ELSE
        ILDECI = 0
      ENDIF
C
C        //// FORMAT DE DECODAGE ////
C
      CODE = 'F'
      IF ( FORMAE ) CODE = 'E'
      JD1 = 3 - INT(DLOG10(DBLE(ILONG)))
      WRITE (LLONG,'(I3)') ILONG
      JD2 = 3
      IF ( ILDECI.GT.0 ) JD2 = 3-INT(DLOG10(DBLE(ILDECI)))
      WRITE (LLDECI,'(I3)') ILDECI
      IF ( I1.GT.1 ) THEN
           WRITE ( FORMA , 1010 )  I1-1,CODE,LLONG(JD1:3),LLDECI(JD2:3)
      ELSE
           WRITE ( FORMA , 1020 )  CODE,LLONG(JD1:3),LLDECI(JD2:3)
      ENDIF
C
1010  FORMAT('(',I3,'X,',A1,A,'.',A,')' )
1020  FORMAT('(',A1,A,'.',A,')' )
C
C        ////  DECODAGE ////
C
      READ  ( LIGNE , FORMA , ERR=995 ) RVAL
      REALUMASC = RSIGNE * RVAL
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
996   FORMAT(1X,'ERREUR LIGNE ',1I6,', UN REEL EST ATTENDU : ',/)
1996  FORMAT(1X,'ERREUR LINE ',1I6,', REAL EXPECTED : ',/)
      ERREUR=.TRUE.
      RETURN
C
C TRAITEMENT DES ERREURS DE LECTURE DU FICHIER
C
998   CONTINUE
      IF(LNG.EQ.1) WRITE(6,999)  NFIC,NLIGN+1
      IF(LNG.EQ.2) WRITE(6,1999) NFIC,NLIGN+1
999   FORMAT(1X,'UNITE LOGIQUE ',1I2,'   ERREUR LIGNE ',1I6)
1999  FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR LINE ',1I6)
      RETOUR = .TRUE.
      RETURN
C
      END
