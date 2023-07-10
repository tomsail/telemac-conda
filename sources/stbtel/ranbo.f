!                       ****************
                        SUBROUTINE RANBO
!                       ****************
!
     &(NBOR,KP1BOR,IFABOR,IKLE,NCOLOR,TRAV1,NPTFR,X,Y,NCOLFR,
     & NDP,NPOIN,NELEM,NELMAX,MESH)
!
!***********************************************************************
!  PROGICIEL : STBTEL V5.2   10/02/93    J.M. JANIN   (LNH)
!***********************************************************************
!
!brief  Building the table of boundary segments
!

!history  S.E.BOURBAN (HRW)
!+        21/03/2017
!+        V7P3
!+   Replacement of the DATA declarations by the PARAMETER associates
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    NBOR        |<-- | TABLEAU DES POINTS DE BORD                   |
! |    IFABOR      | -->| TABLEAU DES VOISINS DES FACES.               |
! |    IKLE        | -->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT |
! |    NCOLOR      | -->| TABLEAU DES COULEURS DES POINTS              |
! |    NCOLFR      |<-- | TABLEAU DES COULEURS DES POINTS DE BORD      |
! |    TRAV1       |<-->| TABLEAU DE TRAVAIL                           |
! |    NPTFR       |<-- | NOMBRE DE POINTS DE BORD
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       | -->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       | -->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |________________|____|______________________________________________|
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : STBTEL
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NDP,NELMAX,MESH,NELEM,NPOIN
      INTEGER, INTENT(INOUT) :: NPTFR
      INTEGER, INTENT(INOUT) :: NBOR(*),KP1BOR(*),NCOLFR(*)
      INTEGER, INTENT(INOUT) :: TRAV1(NPOIN,2)
      INTEGER, INTENT(IN)    :: IFABOR(NELMAX,*),IKLE(NELMAX,NDP)
      INTEGER, INTENT(IN)    :: NCOLOR(*)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IILE,NILE,I,ISUIV,IELEM,IFACE,NOEUD1,NOEUD2
      INTEGER IERROR, I1, I2
!
      DOUBLE PRECISION SOM1,SOM2,Y2
!
      LOGICAL SWAP
!
      INTEGER :: SOMSUI(4) = (/ 2 , 3 , 4 , 0 /)
      DOUBLE PRECISION, PARAMETER :: EPSILO = 1.D-6
!
!=======================================================================
! INITIALISATION
!=======================================================================
!
      SOMSUI(NDP) = 1
      IF (MESH.NE.2.AND.MESH.NE.3) THEN
        WRITE(LU,4000) MESH
4000    FORMAT(/,1X,'RANBO : MESH NOT ALLOWED , MESH = ',I4,/)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!=======================================================================
! RECHERCHE DES ARETES DE BORD,NUMEROTEES DE 1 A NPTFR
!=======================================================================
!
      NPTFR = 0
      DO IELEM=1,NELEM
        DO IFACE=1,NDP
          IF(IFABOR(IELEM,IFACE).LE.0) THEN
            NPTFR = NPTFR + 1
            TRAV1(NPTFR,1) = IKLE(IELEM,       IFACE )
            TRAV1(NPTFR,2) = IKLE(IELEM,SOMSUI(IFACE))
          ENDIF
        ENDDO
      ENDDO
!
!=======================================================================
! ON VERIFIE QUE CHAQUE POINT N'APPARAIT QUE DEUX FOIS
! ( UNE FOIS COMME NOEUD 1 , UNE FOIS COMME NOEUD 2 )
!=======================================================================
!
      IERROR = 0
      DO I=1,NPTFR
        I1 = 1
        I2 = 1
        DO ISUIV=1,NPTFR
          IF (TRAV1(I,1).EQ.TRAV1(ISUIV,2)) I1 = I1 + 1
          IF (TRAV1(I,2).EQ.TRAV1(ISUIV,1)) I2 = I2 + 1
        ENDDO
        IF (I1.NE.2) THEN
          IERROR = IERROR + 1
          WRITE(LU,1020) X(TRAV1(I,1)),Y(TRAV1(I,1)),I1
        ENDIF
        IF (I2.NE.2) THEN
          IERROR = IERROR + 1
          WRITE(LU,1020) X(TRAV1(I,2)),Y(TRAV1(I,2)),I2
        ENDIF
      ENDDO
!
1020  FORMAT(1X,'ERROR ON BOUNDARY NODE :',/,
     &       1X,'X=',F13.3,'  Y=',F13.3,/,
     &       1X,'IT BELONGS TO',I2,' BOUNDARY SEGMENT(S)')
!
      IF (IERROR.GT.0) THEN
        CALL PLANTE(1)
        STOP
      ENDIF
!
!=======================================================================
! RANGEMENT DES ARETES DE BORD BOUT A BOUT.
! ON COMMENCE ARBITRAIREMENT PAR LE POINT LE PLUS SUD-OUEST
! ( PUIS LE PLUS SUD SI CONFLIT ) AFIN D'ETRE SUR DE COMMENCER
! SUR LE CONTOUR ET NON SUR UNE ILE |||
!=======================================================================
!
      SOM2 = X(1) + Y(1)
      Y2   = Y(1)
!
      DO I=1,NPTFR
!
        SOM1 = X(TRAV1(I,1)) + Y(TRAV1(I,1))
        IF (ABS(SOM1-SOM2).LE.ABS(EPSILO*SOM1)) THEN
          IF (Y(TRAV1(I,1)).LE.Y2) THEN
            Y2    = Y(TRAV1(I,1))
            SOM2  = SOM1
            ISUIV = I
          ENDIF
        ELSEIF (SOM1.LE.SOM2) THEN
          Y2    = Y(TRAV1(I,1))
          SOM2  = SOM1
          ISUIV = I
        ENDIF
!
      ENDDO
!
      NOEUD1 = TRAV1(ISUIV,1)
      NOEUD2 = TRAV1(ISUIV,2)
      TRAV1(ISUIV,1) = TRAV1(1,1)
      TRAV1(ISUIV,2) = TRAV1(1,2)
      TRAV1(1,1) = NOEUD1
      TRAV1(1,2) = NOEUD2
!
      IILE = 0
      NILE = 1
!
      DO I=2,NPTFR
        SWAP = .FALSE.
!
!=======================================================================
! RECHERCHE DE L'ARETE DONT LE PREMIER NOEUD EST IDENTIQUE AU SECOND
! DE L'ARETE PRECEDENTE
!=======================================================================
!
        DO ISUIV=I,NPTFR
!
          IF (TRAV1(ISUIV,1).EQ.TRAV1(I-1,2)) THEN
!
!=======================================================================
! PERMUTATION DES ARETES DE NUMEROS I+1 ET ISUIV
!=======================================================================
!
            NOEUD1 = TRAV1(ISUIV,1)
            NOEUD2 = TRAV1(ISUIV,2)
            TRAV1(ISUIV,1) = TRAV1(I,1)
            TRAV1(ISUIV,2) = TRAV1(I,2)
            TRAV1(I,1) = NOEUD1
            TRAV1(I,2) = NOEUD2
            KP1BOR(I+NPTFR) = I-1
            KP1BOR(I-1) = I
            SWAP = .TRUE.
            EXIT
!
          ENDIF
!
        ENDDO
        IF(SWAP) CYCLE
!
!=======================================================================
! SI ON NE TROUVE PAS DE POINT SUIVANT : ON VERIFIE QUE LE DERNIER POINT
! TROUVE EST IDENTIQUE AU PREMIER , DANS CE CAS ON EST EN PRESENCE D'UNE
! ILE ET ON ITERE LE PROCESSUS GLOBAL
!=======================================================================
!
        IF (TRAV1(NILE,1).NE.TRAV1(I-1,2)) THEN
!
!=======================================================================
! SINON IL Y A ERREUR
!=======================================================================
!
          WRITE(LU,4500) TRAV1(I-1,2)
4500      FORMAT(1X,'ERROR IN STORING THE EDGE SEGMENTS',/,
     &           1X,'FOR THE NODE ',I5)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        KP1BOR(NILE+NPTFR) = I-1
        KP1BOR(I-1) = NILE
        IILE = IILE+1
        NILE = I
!
      ENDDO! ISUIV
!
!=======================================================================
! ON VERIFIE QUE LA DERNIERE ILE EST FERMEE
!=======================================================================
!
      IF (TRAV1(NILE,1).NE.TRAV1(NPTFR,2)) THEN
        WRITE(LU,5000) TRAV1(NILE,1),TRAV1(NPTFR,2)
5000    FORMAT(1X,'ERROR, THE BOUNDARY IS NOT CLOSED :',/,
     &         1X,'FIRST POINT :',I5,2X,'LAST POINT : ',I5)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      KP1BOR(NILE+NPTFR) = NPTFR
      KP1BOR(NPTFR) = NILE
!
      WRITE(LU,5500) NPTFR
      WRITE(LU,5600) IILE
 5500 FORMAT(1X,'NUMBER OF BOUNDARY POINTS      : ',I5)
 5600 FORMAT(1X,'NUMBER OF ISLANDS              : ',I5)
!
!=======================================================================
! REMPLISSAGE DU TABLEAU NBOR ET STOCKAGE DE LA COULEUR DES POINTS DE
! BORD DANS LE TABLEAU NCOLFR
!=======================================================================
!
      DO I=1,NPTFR
        NBOR(I      ) = TRAV1(I,1)
        NBOR(I+NPTFR) = TRAV1(I,2)
        NCOLFR(I) = NCOLOR(TRAV1(I,1))
      ENDDO
!
      RETURN
      END
