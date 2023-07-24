                        INTEGER FUNCTION PREVALMASC
C                       ***************************
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
C             VALIDE DE LA LIGNE (C'EST A DIRE HORS COMMENTAIRE, NON
C             BLANC ET NON TABULATION).
C             RETOURNE LONGLI + 1 SI LE CARACTERE N'EST PAS TROUVE
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
C     - PORTABILITE :  IBM,CRAY,HP,SUN
C
C     - APPELE PAR :   CMD,DAMOC,INTLU,REALU
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
      INTEGER       I,J
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
      PREVALMASC = LONGLI + 1
      I      = ICOL -1
C
100   CONTINUE
      I = I + 1
      IF (LIGNE(I:I).NE.CAR1.AND.LIGNE(I:I).NE.CAR2.AND.
     *    LIGNE(I:I).NE.CAR3) THEN
C-----------------------------------------------------------------------
C          ELIMINATION DES COMMENTAIRES :
C
           IF ( I.GE.LONGLI ) GO TO 1000
           IF ( LIGNE(I:I).EQ.'/' ) THEN
                DO 110 J = I+1 , LONGLI
                     IF ( LIGNE(J:J).EQ.'/' ) THEN
                          I = J
                          GO TO 100
                     ENDIF
  110           CONTINUE
                GO TO 1000
C-----------------------------------------------------------------------
           ELSE
                GO TO 100
           ENDIF
      ELSE
           PREVALMASC = I
      ENDIF
C
1000  CONTINUE
C
      RETURN
      END
