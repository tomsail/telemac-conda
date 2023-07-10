!                       *****************
                        SUBROUTINE SURCON
!                       *****************
!
     &(X,Y,IKLE,IPO,NBOR,NPTFR,NCOLOR,IFABOR,COLOR)
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2                   19/04/91  J-C GALLAND  (LNH)
!                                           19/02/93  J-M JANIN    (LNH)
!***********************************************************************
!
! FONCTION : DECOUPAGE DES TRIANGLES SURCONTRAINTS
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
! |   IKLE         |<-->| NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
! |   TRAV1        |<-->| TABLEAU DE TRAVAIL
! |   NBOR         | -->| TABLEAU DES POINTS DE BORD
! |   NPTFR        | -->| NOMBRE DE POINT FRONTIERE
! |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS
! |   IFABOR       |<-->| TABLEAU DES VOISINS DES ELEMENTS
! |    COLOR       |<-->| STOCKAGE COULEURS DES NOEUDS SUR FICHIER GEO
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       |<-->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       |<-->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |                |    |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : STBTEL
! APPEL DE : DECOUP
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,NPOIN,NELMAX,NPMAX
      USE INTERFACE_STBTEL, EX_SURCON => SURCON
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(INOUT) :: X(*) , Y(*)
      INTEGER, INTENT(INOUT) :: NBOR(*) , IKLE(NELMAX,4) , NCOLOR(*)
      INTEGER, INTENT(INOUT) :: IFABOR(NELMAX,*) , IPO(*)
      LOGICAL, INTENT(INOUT) :: COLOR
      INTEGER, INTENT(IN) :: NPTFR
!
      INTEGER NPOIN2 , NELEM2
      INTEGER ITEST , IELEM , KELEM , ISWAP , KSWAP
      INTEGER I , J , K
      INTEGER :: IP(3) , KP , ISUI(3)
!
      PARAMETER ( ISUI = (/ 2 , 3 , 1 /) )
!
!=======================================================================
!
      WRITE(LU,4050)
!
      NPOIN2 = NPOIN
      NELEM2 = NELEM
      ITEST  = 0
!
!=======================================================================
! RECHERCHE DES TRIANGLES SURCONTRAINTS
!=======================================================================
!
      DO K = 1 , NPMAX
        IPO(K) = 0
      ENDDO
!
      DO K = 1 , NPTFR
        IPO(NBOR(K)) = 1
      ENDDO

      DO K = 1 , NELEM
        IF (IPO(IKLE(K,1))+IPO(IKLE(K,2))+IPO(IKLE(K,3)).EQ.3) THEN
!
! LE TRIANGLE EST SURCONTRAINT, ON LE DECOUPE EN TROIS
!
          ITEST  = ITEST + 1
          IF (ITEST.GT.INT(0.1*NELMAX)) THEN
            WRITE(LU,4000) INT(0.1*NELMAX)
            CALL PLANTE(1)
            STOP
          ENDIF
!
          WRITE(LU,4070) X(IKLE(K,1)),Y(IKLE(K,1)),
     &       X(IKLE(K,2)),Y(IKLE(K,2)),X(IKLE(K,3)),Y(IKLE(K,3))
!
          CALL DECOUP (K,X,Y,IKLE,NCOLOR,IFABOR,
     &                 NELEM2,NPOIN2,COLOR)
!
        ENDIF
      ENDDO !K
!
!=======================================================================
! REMISE A JOUR DE NPOIN ET NELEM
!=======================================================================
!
      NPOIN = NPOIN2
      NELEM = NELEM2
!
!=======================================================================
! MAINTENANT ON SWAPE LES ARETES CONTENANT 2 POINTS DE BORD ET QUI NE
! SONT PAS DES ARETES DE BORD
!
! REMARQUE : LES IFABOR NE SONT PAS MODIFIES DANS CE QUI SUIT CAR
!            ILS NE SERVENT PLUS APRES POUR LE LOGICIEL
!=======================================================================
!
      ISWAP = 0
      KSWAP = 0
      DO IELEM = 1,NELEM
        IP(1) = IKLE(IELEM,1)
        IP(2) = IKLE(IELEM,2)
        IP(3) = IKLE(IELEM,3)
        DO I = 1,3
          J = ISUI(I)
          IF(IFABOR(IELEM,I).GT.0.AND.IPO(IP(I))+IPO(IP(J)).EQ.2) THEN
            KELEM = IFABOR(IELEM,I)
!
! COMPTE TENU DU PREMIER DECOUPAGE, ON EST SUR QUE TOUT ELEMENT CONTIENT
! AU MOINS UN POINT INTERIEUR, ET DANS CE CAS UN SEUL.
! K EST SUR D'ETRE REMPLI.
!
            IF (IPO(IKLE(KELEM,1)).EQ.0) K=1
            IF (IPO(IKLE(KELEM,2)).EQ.0) K=2
            IF (IPO(IKLE(KELEM,3)).EQ.0) K=3
            KP = IKLE(KELEM,K)
!
            IF ((X(KP)-X(IP(I)))*(Y(IP(ISUI(J)))-Y(IP(I))).GT.
     &          (Y(KP)-Y(IP(I)))*(X(IP(ISUI(J)))-X(IP(I))).AND.
     &          (X(KP)-X(IP(J)))*(Y(IP(ISUI(J)))-Y(IP(J))).LT.
     &          (Y(KP)-Y(IP(J)))*(X(IP(ISUI(J)))-X(IP(J)))) THEN
!
              ISWAP = ISWAP + 1
              IKLE(IELEM,I) = KP
              IKLE(KELEM,ISUI(K)) = IP(ISUI(J))
!
              WRITE(LU,4080) X(IP(I)),Y(IP(I)),
     &                       X(IP(J)),Y(IP(J))
!
            ELSE
!
              KSWAP = KSWAP + 1
!
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!=======================================================================
!
      WRITE(LU,4100) ITEST,NPOIN,NELEM,ISWAP
!
      IF (KSWAP.NE.0) THEN
        WRITE(LU,4200) KSWAP
      ENDIF
!
 4000 FORMAT(//,1X,'**************************************************',
     &        /,1X,'THE MAXIMUM NUMBER OF OVERSTRESSED TRIANGLES   ',
     &        /,1X,'IS',I5,' : IT IS GREATER IN YOUR CASE ||',
     &        /,1X,'**************************************************')
 4050 FORMAT(//,1X,'OVERSTRESSED ELEMENTS ARE CANCELLED',/,
     &          1X,'-----------------------------------',/)
 4070 FORMAT   (1X,'ADDITIONAL NODE AT CENTRE OF TRIANGLE :',/,
     &          1X,'(',D9.3,',',D9.3,'),(',D9.3,',',D9.3,'),',
     &             '(',D9.3,',',D9.3,')')
 4080 FORMAT   (1X,'SWAP OF FACE :',/,
     &          1X,'(',D9.3,',',D9.3,'),(',D9.3,',',D9.3,')')
 4100 FORMAT (/,1X,'NUMBER OF CANCELLED ELEMENTS : ',I5,/,
     &          1X,'AFTER BEING CANCELLED :',/,
     &          1X,'          NUMBER OF POINTS      : ',I5,/,
     &          1X,'          NUMBER OF ELEMENTS    : ',I5,//,
     &          1X,'MOREOVER,',I4,' TRIANGLES HAVE BEEN SWAPPED',//)
 4200 FORMAT   (1X,'      BUT',I4,' COULD NOT BE',//)
!
      RETURN
      END
