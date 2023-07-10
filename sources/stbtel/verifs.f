!                       *****************
                        SUBROUTINE VERIFS
!                       *****************
!
     &(IFABOR,IKLE,TRAV1,NPTFR,NUMPB,NBPB)
!
!***********************************************************************
!  PROGICIEL : STBTEL V5.2        10/02/93    J.M. JANIN   (LNH)
!                                 25/02/99    P. LANG      (SOGREAH)
!***********************************************************************
!
!brief    REPERAGE DES POINTS APPARTENANT A PLUS DE TROIS
!               SEGMENTS FRONTIERES APRES ELIMINATION DES ELEMENTS SECS
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
! |    TRAV1       |<-->| TABLEAU DE TRAVAIL                           |
! |    NPTFR       |<-- | NOMBRE DE POINTS DE BORD                     |
! |    X,Y         |--> | COORDONNEES DES POINTS DU MAILLAGE           |
! |    NUMPB       |<-- | NUMEROS DES POINTS POSANT PROBLEME           |
! |    NBPB        |<-- | NOMBRE DE POINTS POSANT PROBLEME             |
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       | -->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       | -->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
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
      USE DECLARATIONS_STBTEL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: IFABOR(NELMAX,*) , IKLE(NELMAX,4)
      INTEGER, INTENT(INOUT) :: TRAV1(NPOIN,2)
      INTEGER, INTENT(INOUT) :: NPTFR
      INTEGER, INTENT(INOUT) :: NUMPB(100), NBPB
!
      INTEGER I, J
      INTEGER ISUIV , IELEM , IFACE
      INTEGER I1 , I2
      LOGICAL EXIST
!
!      DATA SOMSUI / 2 , 3 , 4 , 0 /
      INTEGER :: SOMSUI(4) = (/ 2 , 3 , 4 , 0 /)
!
!=======================================================================
! INITIALISATION
!=======================================================================
!
      WRITE(LU,1020)
      NBPB = 0
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
          IF (IFABOR(IELEM,IFACE).LE.0) THEN
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
      DO I=1,NPTFR
        I1 = 1
        I2 = 1
        DO ISUIV=1,NPTFR
          IF (TRAV1(I,1).EQ.TRAV1(ISUIV,2)) I1 = I1 + 1
          IF (TRAV1(I,2).EQ.TRAV1(ISUIV,1)) I2 = I2 + 1
        ENDDO
        IF (I1.NE.2) THEN
          IF (NBPB.EQ.0) THEN
            NBPB = 1
            NUMPB(NBPB) = TRAV1(I,1)
          ELSE
            EXIST = .FALSE.
            DO J=1,NBPB
              IF (NUMPB(J).EQ.TRAV1(I,1)) EXIST = .TRUE.
            ENDDO
            IF (.NOT.EXIST) THEN
              NBPB = NBPB + 1
              IF (NBPB.GT.100) THEN
                WRITE(LU,9001)
                CALL PLANTE(1)
                STOP
              ENDIF
              NUMPB(NBPB) = TRAV1(I,1)
            ENDIF
          ENDIF
        ENDIF
        IF (I2.NE.2) THEN
          IF (NBPB.EQ.0) THEN
            NBPB = 1
            NUMPB(NBPB) = TRAV1(I,2)
          ELSE
            EXIST = .FALSE.
            DO J=1,NBPB
              IF (NUMPB(J).EQ.TRAV1(I,2)) EXIST = .TRUE.
            ENDDO
            IF (.NOT.EXIST) THEN
              NBPB = NBPB + 1
              IF (NBPB.GT.100) THEN
                WRITE(LU,9001)
                CALL PLANTE(1)
                STOP
              ENDIF
              NUMPB(NBPB) = TRAV1(I,2)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
!
      RETURN
!
! -------------------------FORMATS------------------------------------------
 1020 FORMAT (//,1X,'SEARCHING ABOUT CONNECTED ISLANDS',/,
     &          1X,'---------------------------------')
 9001 FORMAT (1X,'*****************************************',/,
     &        1X,'ERROR - ROUTINE VERIFS',/,
     &        1X,'NB OF CONNECTION POINTS GREATHER THAN 100',/,
     &        1X,'*****************************************')
!
      END
