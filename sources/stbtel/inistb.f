!                       *****************
                        SUBROUTINE INISTB
!                       *****************
!
     &(NPOIN1,TYPELE,MAILLE,PRECIS,NGEO,NSEC2,NSEC11,NSEC12)
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2            09/08/89    J.C. GALLAND
!***********************************************************************
!
!   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
!               MAILLAGE DANS LE FICHIER UNIVERSEL DE SUPERTAB
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | NPOIN1         |<-- | NOMBRE REEL DE POINTS DU MAILLAGE
! |                |    | (NPOIN REPRESENTE L'INDICE MAX DES NOEUDS CAR
! |                |    | SUPERTAB LAISSE DES TROUS DANS LA NUMEROTATION
! | TYPELE         |<-- | TYPE D'ELEMENTS
! | MAILLE         |<-- | NOM DU MAILLEUR
! | PRECIS         |<-- | FORMAT DE LECTURE DES COORDONNEES DES NOEUDS
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        |<-- | TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       |<-- | NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       |<-- | NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |  FICH:         |    |
! |    NRES        |--> | NUMERO DU CANAL DU FICHIER DE SERAFIN
! |    NGEO       |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
! |    NLIM      |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
! |    NFO1      |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
! |  SECT:         |    |
! |    NSEC11      |--> | INDICATEUR DU SECTEUR CONTENANT LES NOEUDS
! |                |    | (LECTURE EN SIMPLE PRECISION)
! |    NSEC12      |--> | INDICATEUR DU SECTEUR CONTENANT LES NOEUDS
! |                |    | (LECTURE EN DOUBLE PRECISION)
! |    NSEC2       |--> | INDICATEUR DU SECTEUR CONTENANT LES ELEMENTS
! |    NSEC3       |--> | INDICATEUR DU SECTEUR CONTENANT LE TITRE
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : HOMERE
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,MESH,NDP,NPOIN
      IMPLICIT NONE
!
      INTEGER, INTENT(INOUT) :: NPOIN1
      CHARACTER(LEN=11), INTENT(INOUT) :: TYPELE
      CHARACTER(LEN=9), INTENT(IN) ::  MAILLE
      CHARACTER(LEN=6), INTENT(INOUT) ::  PRECIS
      INTEGER, INTENT(IN) :: NSEC11 , NSEC12 , NGEO, NSEC2
!
      DOUBLE PRECISION X1
      INTEGER NPOIN2
      INTEGER N1, NSEC
      INTEGER N2
      INTEGER INDI11 , INDI12 , INDIC2
!
      CHARACTER(LEN=2)  MOINS1
      CHARACTER(LEN=4)  BLANC*4
!
! COMMON
!
!
!=======================================================================
! INITIALISATION
!=======================================================================
!
      REWIND NGEO
      NPOIN1  = 0
      NPOIN2  = 0
      NELEM   = 0
      INDI11  = 0
      INDI12  = 0
      INDIC2  = 0
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER ET RECHERCHE DES INDICATEURS
! NSEC1 ET NSEC2
!=======================================================================
!
 10   READ(NGEO,1000,ERR=110,END=120) BLANC,MOINS1
      IF (MOINS1.NE.'-1'.OR.BLANC.NE.'    ') GOTO 10
 1000 FORMAT(A4,A2)
!
 20   READ(NGEO,2000,ERR=110,END=120) NSEC
      IF (NSEC.EQ.-1) THEN
        GOTO 20
!
!=======================================================================
! NOMBRE DE POINTS
!=======================================================================
!
! LECTURE EN SIMPLE PRECISION
!
      ELSE IF (NSEC.EQ.NSEC11) THEN
        INDI11 = 1
!
 30     CONTINUE
        READ(NGEO,2000,ERR=110,END=120) NSEC
!
        IF (NSEC.NE.-1) THEN
          NPOIN1 = NPOIN1+1
          NPOIN2 = MAX0(NSEC,NPOIN1)
          GOTO 30
        ELSE
          GOTO 50
        ENDIF
!
! LECTURE EN DOUBLE PRECISION
!
      ELSE IF (NSEC.EQ.NSEC12) THEN
        INDI12 = 1
!
 31     CONTINUE
        READ(NGEO,2000,ERR=110,END=120) NSEC
!
        IF (NSEC.NE.-1) THEN
          NPOIN1 = NPOIN1+1
          NPOIN2 = MAX0(NSEC,NPOIN1)
        ELSE
          GOTO 50
        ENDIF
!
        READ(NGEO,4000,ERR=110,END=120) X1
!
        GOTO 31
!
!=======================================================================
! NOMBRE ET TYPE D'ELEMENTS
!=======================================================================
!
      ELSE IF (NSEC.EQ.NSEC2) THEN
        INDIC2 = 1
        IF (MAILLE.EQ.'SUPERTAB4') THEN
!  LECTURE AU FORMAT SUPERTAB VERSION 4
          READ(NGEO,3000,ERR=110,END=120) N1,N2,MESH
        ELSE
!  LECTURE AU FORMAT SUPERTAB VERSION 6
          READ(NGEO,3000,ERR=110,END=120) N1,MESH,N2
        ENDIF
        NELEM = 1
 40     READ(NGEO,2000,ERR=110,END=120) NSEC
        IF (NSEC.NE.-1) THEN
          NELEM = NELEM+1
          GOTO 40
        ELSE
          GOTO 50
        ENDIF
      ENDIF
!
 50   IF ((INDI11.EQ.1.OR.INDI12.EQ.1).AND.INDIC2.EQ.1) THEN
        GOTO 60
      ELSE
        GOTO 10
      ENDIF
!
 110  CONTINUE
      WRITE(LU,4100)
      CALL PLANTE(1)
      STOP
!
 120  CONTINUE
      IF (INDI11.NE.1.AND.INDI12.NE.1) WRITE(LU,4200)
      IF (INDI12.NE.1) WRITE(LU,4300)
      STOP
!
 60   CONTINUE
!
!=======================================================================
! AFFECTATION DES VALEURS LUES AUX VARIABLES CONCERNEES
!=======================================================================
!
      IF (INDI11.EQ.1) PRECIS='SIMPLE'
      IF (INDI12.EQ.1) PRECIS='DOUBLE'
!
      NELEM =NELEM / 2
!
      NPOIN = NPOIN2
!
!=======================================================================
! MISE DES VALEURS DE MESH AU STANDARD TELEMAC
!=======================================================================
!
      IF (MESH.EQ.94) THEN
        MESH = 2
        NDP  = 4
        TYPELE = 'QUADRANGLES'
      ELSEIF (MESH.EQ.91) THEN
        MESH = 3
        NDP  = 3
        TYPELE = 'TRIANGLES  '
      ELSE
        WRITE(LU,3140) MESH
 3140   FORMAT(' INISTB : TYPE OF MESH NOT AVAILABLE IN TELEMAC,
     &           MESH = ',I4)
      ENDIF
!
!=======================================================================
! IMPRESSION DES RESULTATS
!=======================================================================
!
 4100 FORMAT(//,'****************************************',/,
     &          'ERROR IN READING UNIVERSAL FILE (INISTB)',/,
     &          '****************************************')
 4200 FORMAT(//,'********************************************',/,
     &          'END OF THE UNIVERSAL FILE : NO NODE (INISTB)',/,
     &          '********************************************')
 4300 FORMAT(//,'***********************************************',/,
     &          'END OF THE UNIVERSAL FILE : NO ELEMENT (INISTB)',/,
     &          '***********************************************')
 2000 FORMAT(I10)
 3000 FORMAT(3I10)
 4000 FORMAT(D25.16)
!
      RETURN
      END
