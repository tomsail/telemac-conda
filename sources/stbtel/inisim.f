!                       *****************
                        SUBROUTINE INISIM
!                       *****************
!
     &(NPOIN1,TYPELE,INOP5,NGEO)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2            25/02/92    J.C. GALLAND
!***********************************************************************
!
!   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
!               MAILLAGE, AINSI QUE LA LONGUEUR DU TABLEAU NOP5 DANS LE
!               FICHIER GEOMETRIE DE SIMAIL
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | NPOIN1         |<-- | NOMBRE REEL DE POINTS DU MAILLAGE
! | TYPELE         |<-- | TYPE D'ELEMENTS
! | IA             |--> | TABLEAU DE TRAVAIL POUR LA LECTURE DE LA SD
! | INOP5          |<-- | DIMENSION DU TABLEAU NOP5 (CONTENANT LES IKLE)
! |                |    | DU FICHIER GEOMETRIE DE SIMAIL
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        |<-- | TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         |<-- | NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       |<-- | NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       |<-- | NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |  FICH:         |    |
! |    NRES        |--> | NUMERO DU CANAL DU FICHIER DE SERAFIN
! |    NGEO        |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
! |    NDYNAM      |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
! |    NFO1        |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : HOMERE
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,MESH,NDP,NPOIN
!
      IMPLICIT NONE
      !
      INTEGER, INTENT(INOUT) :: NPOIN1,INOP5
      INTEGER, INTENT(IN) :: NGEO
      CHARACTER(LEN=11), INTENT(INOUT) :: TYPELE
!
      INTEGER IA(32),LONG,I,J,NTASD,MESHT,MESHQ
!
!=======================================================================
! INITIALISATION
!=======================================================================
!
      REWIND NGEO
      NPOIN  = 0
      NPOIN1 = 0
      NELEM  = 0
!
!=======================================================================
! LECTURE PARTIELLE DU 1ER ENREGISTREMENT DE LA SD
!=======================================================================
!
      READ(NGEO,ERR=110,END=120) LONG,(IA(I),I=1,MIN(LONG,32))
!
!=======================================================================
! LECTURE PARTIELLE DU TABLEAU NOP0
! RECHERCHE DU NOMBRE DE TABLEAUX ASSOCIES, NTASD
!=======================================================================
!
      READ(NGEO,ERR=110,END=120) LONG,(IA(I),I=1,32)
      NTASD = IA(32)
!
!=======================================================================
! LECTURE DU TABLEAU NOP1 ET DES TABLEAUX ASSOCIES
!=======================================================================
!
      IF (NTASD.GT.0) THEN
        READ(NGEO,ERR=110,END=120) LONG,(IA(I),I=1,MIN(LONG,32))
        DO I=1,NTASD
          READ(NGEO,ERR=110,END=120) LONG,(IA(J),J=1,MIN(LONG,32))
        ENDDO
      ENDIF
!
!=======================================================================
! LECTURE DU TABLEAU NOP2
! LECTURE DU NOMBRE DE POINTS, DU NOMBRE D'ELEMENTS, DU TYPE D'ELEMENT
! ET DE LA LONGUEUR DU TABLEAU NOP5 (TABLEAU DES IKLE)
! AFFECTATION DES VALEURS LUES AUX VARIABLES CONCERNEES
!=======================================================================
!
      READ(NGEO,ERR=110,END=120) LONG,(IA(I),I=1,MIN(LONG,32))
      NPOIN1 = IA(15)
      NELEM  = IA(5)
      MESHT  = IA(8)
      MESHQ  = IA(9)
      INOP5  = IA(26)
!
      NPOIN = NPOIN1
!
!=======================================================================
! MISE DES VALEURS DE MESH AU STANDARD TELEMAC
!=======================================================================
!
      IF (MESHQ.NE.0) THEN
        MESH = 2
        NDP  = 4
        TYPELE = 'QUADRANGLES'
      ELSEIF (MESHT.NE.0) THEN
        MESH = 3
        NDP  = 3
        TYPELE = 'TRIANGLES  '
      ELSE
        WRITE(LU,3100)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      GOTO 20
!
 110  CONTINUE
      WRITE(LU,4100)
 120  CONTINUE
      WRITE(LU,4200)
!
20    CONTINUE
!
!=======================================================================
! IMPRESSION DES RESULTATS
!=======================================================================
!
 3100 FORMAT(/,'*************************************************'
     &      ,/,'INISIM : TELEMAC DOESN''T WORK WITH MESHES MIXING '
     &      ,/,'         TRIANGLES AND QUADRILATERALS',
     &      /,'**************************************************')
 4100 FORMAT(//,'*************************************',/,
     &          'INISIM : ERROR IN READING FILE SIMAIL',/,
     &          '*************************************',//)
 4200 FORMAT(//,'*************************************************',/,
     &          'INISIM : ATTEMPT TO READ AFTER END OF FILE SIMAIL',/,
     &          '*************************************************',//)
!
      RETURN
      END
