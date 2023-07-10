!                       *****************
                        SUBROUTINE DYNAMI
!                       *****************
!
     &(NPTFR,NBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR,NCOLFR,MAILLE,NLIM)
!
!***********************************************************************
! STBTEL VERSION        6.0                J-C GALLAND (LNH) 30 87 78 13
!                                          J-M JANIN   (LNH) 30 87 72 84
!                                          P LANG      (LHF)
! ORIGINE   : TELEMAC
!***********************************************************************
!
!  FONCTION : ECRITURE DU FICHIER DYNAM DE TELEMAC
!             POUR TOUTE MODIFICATION DES CL VOIR DANS LE SPGM STBTEL
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! I      NOM       IMODEI                   ROLE
! I________________I____I______________________________________________
! |   NPTFR        | -->| NOMBRE DE POINTS FRONTIERE
! |   NBOR         | -->| TABLEAU DES POINTS DE BORD
! |   NCOLFR       | -->| TABLEAU DES COULEURS DES POINTS FRONTIERES
! |   MAILLE       | -->| NOM DU MAILLEUR
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NELEM       | -->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : STBTEL
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_HERMES
      USE DECLARATIONS_STBTEL, ONLY: OUT_FORMAT, TYP_BND_ELEM
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPTFR, NLIM
      INTEGER, INTENT(IN) :: NBOR(*) , NCOLFR(*)
      INTEGER, INTENT(INOUT) :: LIHBOR(*) , LIUBOR(*)
      INTEGER, INTENT(INOUT) :: LIVBOR(*) ,LITBOR(*)
      CHARACTER(LEN=9), INTENT(IN) :: MAILLE
!
      INTEGER ILOG , IADH , IENT , IENTU , IINC , ISORT
!
      INTEGER I,J,IERR
!
      DOUBLE PRECISION, ALLOCATABLE :: ZEROS(:)
      INTEGER, ALLOCATABLE :: NCOLOR(:)
!
!
!***********************************************************************
!
      ILOG = 2
      IADH = 0
      IENT = 5
      IENTU= 6
      ISORT= 4
      IINC = 1
!
      REWIND NLIM
!
      DO J =1,NPTFR
!
! PAR DEFAUT, ON SUPPOSE QUE LE POINT EST UN POINT FRONTIERE SOLIDE
! SANS FROTTEMENT. LA COULEUR 11, STANDARD POUR SUPERTAB, DONNE CE
! TYPE DE CARACTERISTIQUE.
!
        LIHBOR(J)=ILOG
        LIUBOR(J)=ILOG
        LIVBOR(J)=ILOG
        LITBOR(J)=ILOG
!
        IF(NCOLFR(J).EQ.1) THEN
!
! H IMPOSEE , U ET V LIBRES
!
          LIHBOR(J)=IENT
          LIUBOR(J)=ISORT
          LIVBOR(J)=ISORT
          LITBOR(J)=ISORT
!
        ELSE IF (NCOLFR(J).EQ.2) THEN
!
!  H  IMPOSEE , DEBIT IMPOSE
!
          LIHBOR(J)=IENT
          LIUBOR(J)=IENT
          LIVBOR(J)=IENT
          LITBOR(J)=IENT
!
        ELSE IF (NCOLFR(J).EQ.3) THEN
!
!  H , U ET V IMPOSEES
!
          LIHBOR(J)=IENT
          LIUBOR(J)=IENTU
          LIVBOR(J)=IENTU
          LITBOR(J)=IENT
!
        ELSE IF (NCOLFR(J).EQ.4) THEN
!
! H IMPOSEE , U LIBRE , V NULLE
!
          LIHBOR(J)=IENT
          LIUBOR(J)=ISORT
          LIVBOR(J)=IADH
          LITBOR(J)=ISORT
!
        ELSE IF (NCOLFR(J).EQ.5) THEN
!
!  CONDITION D'ONDE INCIDENTE
!
          LIHBOR(J)=IINC
          LIUBOR(J)=IINC
          LIVBOR(J)=IINC
          LITBOR(J)=ISORT
!
        ELSE IF (NCOLFR(J).EQ.7) THEN
!
! H IMPOSEE , U NULLE , V LIBRE
!
          LIHBOR(J)=IENT
          LIUBOR(J)=IADH
          LIVBOR(J)=ISORT
          LITBOR(J)=ISORT
!
        ELSE IF (NCOLFR(J).EQ.8) THEN
!
! H LIBRE , U ET V IMPOSEES
!
          LIHBOR(J)=ISORT
          LIUBOR(J)=IENT
          LIVBOR(J)=IENT
          LITBOR(J)=IENT
!
        ELSE IF (NCOLFR(J).EQ.9) THEN
!
!  H LIBRE , U ET V IMPOSEES
!
          LIHBOR(J)=ISORT
          LIUBOR(J)=IENTU
          LIVBOR(J)=IENTU
          LITBOR(J)=IENT
!
        ELSE IF (NCOLFR(J).EQ.12) THEN
!
! H LIBRE , U IMPOSEE , V NULLE
!
          LIHBOR(J)=ISORT
          LIUBOR(J)=IENT
          LIVBOR(J)=IADH
          LITBOR(J)=IENT
!
        ELSE IF (NCOLFR(J).EQ.13) THEN
!
! FRONTIERE SOLIDE AVEC V NULLE
!
          LIHBOR(J)=ILOG
          LIUBOR(J)=ILOG
          LIVBOR(J)=IADH
          LITBOR(J)=ILOG
!
        ELSE IF (NCOLFR(J).EQ.14) THEN
!
! FRONTIERE SOLIDE AVEC U NULLE
!
          LIHBOR(J)=ILOG
          LIUBOR(J)=IADH
          LIVBOR(J)=ILOG
          LITBOR(J)=ILOG
!
        ELSE IF (NCOLFR(J).EQ.15) THEN
!
! H LIBRE , U NULLE , V IMPOSEE
!
          LIHBOR(J)=ISORT
          LIUBOR(J)=IADH
          LIVBOR(J)=IENT
          LITBOR(J)=IENT
!
        ENDIF
!
      ENDDO
!
      ALLOCATE(NCOLOR(NPTFR))
      ALLOCATE(ZEROS(NPTFR))
      DO I=1,NPTFR
        NCOLOR(I) = I
      ENDDO
      ZEROS = 0.D0
!
      CALL SET_BND(OUT_FORMAT, NLIM, TYP_BND_ELEM, NPTFR, 1, NBOR,NPTFR,
     &             LIHBOR, LIUBOR, LIVBOR, ZEROS, ZEROS, ZEROS, ZEROS,
     &             LITBOR, ZEROS, ZEROS, ZEROS, NCOLOR, IERR)
      CALL CHECK_CALL(IERR, 'DYNAMI:SET_BND')

      DEALLOCATE(NCOLOR)
      DEALLOCATE(ZEROS)
!
      IF (MAILLE(1:8).NE.'SUPERTAB'.AND.
     &    MAILLE(1:7).NE.'TRIGRID') WRITE(LU,3040) MAILLE
!
!-----------------------------------------------------------------------
!
 3040 FORMAT(/,
     & ' **************************************************',/,
     & ' BEWARE: THE UNIVERSAL FILE FORMAT IS ',A8,/,
     & '         BOUNDARY CONDITIONS WILL HAVE TO BE',/,
     & '         CHECKED IN THE BOUNDARY CONDITIONS FILE',/,
     & ' **************************************************',/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
