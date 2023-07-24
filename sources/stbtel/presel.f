!                       *****************
                        SUBROUTINE PRESEL
!                       *****************
!
     &(IKLE,TRAV1,NELEM,NELMAX,NDP,TEXTE,NBFOND,SORLEO,COLOR,
     & NSFOND,NVARIN,NVAROU,MAILLE)
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2    07/12/88    J-M HERVOUET (LNH) 30 87 80 18
!                            19/02/93    J-M JANIN    (LNH) 30 87 72 84
!                                        A   WATRIN
!***********************************************************************
!
!  FONCTION  :  PREPARATION DE DONNEES AVANT L'APPEL DE FMTSEL
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! APPELE PAR : PREDON
! APPEL DE : -
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NDP,NELEM,NELMAX,NBFOND,NVARIN
      INTEGER, INTENT(INOUT) :: NSFOND,NVAROU
      INTEGER, INTENT(INOUT) :: TRAV1(NELEM,NDP)
      INTEGER, INTENT(IN) :: IKLE(NELMAX,NDP)
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(26)
      CHARACTER(LEN=9), INTENT(IN) :: MAILLE
      LOGICAL, INTENT(INOUT) :: SORLEO(26),COLOR
!
      INTEGER I,IDP,IELEM
!
!-----------------------------------------------------------------------
!
!  IKLE EST REFAIT EN FONCTION DU NOMBRE DEFINITIF D'ELEMENTS
!  LE RESULTAT EST MIS DANS TRAV1.
!
      DO IELEM = 1 , NELEM
        DO IDP = 1 , NDP
          TRAV1(IELEM,IDP) = IKLE(IELEM,IDP)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!  NOMS DES VARIABLES QUI SERONT DANS LE FICHIER DE GEOMETRIE
!  TABLEAUX INDIQUANT SI ELLES SERONT ECRITES.
!
      DO I = 1 , 26
        SORLEO(I) = .FALSE.
      ENDDO
!
      NVAROU = NVARIN
      IF (NVAROU.GT.0) THEN
        DO I = 1 , NVAROU
          SORLEO(I) = .TRUE.
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!  RAJOUT DU FOND PUIS DE LA COULEUR DES NOEUDS PUIS D'UNE VARIABLE
!  BIDON SI NECESSAIRE DANS LES VARIABLES DE SORTIE
!
      IF (NBFOND.GT.0.AND.NSFOND.EQ.0.AND.NVAROU.LT.26) THEN
        NVAROU = NVAROU + 1
        SORLEO(NVAROU) = .TRUE.
        IF (LNG.EQ.LNG_FR)
     &    TEXTE(NVAROU)='FOND                            '
        IF (LNG.EQ.LNG_EN)
     &    TEXTE(NVAROU)='BOTTOM                          '
        NSFOND = NVAROU
      ELSEIF (NBFOND.EQ.0) THEN
        NSFOND = 0
      ENDIF
!
      IF (COLOR) THEN
        IF (NVAROU.LT.26) THEN
          NVAROU = NVAROU + 1
          SORLEO(NVAROU) = .TRUE.
          TEXTE(NVAROU) = 'COULEUR                         '
        ELSE
          COLOR = .FALSE.
        ENDIF
      ENDIF
!
      IF(NVAROU.EQ.0) THEN
        SORLEO(1) = .TRUE.
        IF(MAILLE.NE.'ADCIRC') THEN
          TEXTE(1) = 'MAILLAGE                        '
        ELSE
          IF (LNG.EQ.LNG_FR) TEXTE(1)='FOND                            '
          IF (LNG.EQ.LNG_EN) TEXTE(1)='BOTTOM                          '
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
