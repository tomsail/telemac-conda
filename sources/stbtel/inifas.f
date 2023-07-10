!                       *****************
                        SUBROUTINE INIFAS
!                       *****************
!
     &(TYPELE,NGEO)
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2      09.07.1996  P. CHAILLET  (LHF) - FASTTABS
!***********************************************************************
!
!     FONCTION  : INITIALISATION DES INFORMATIONS DANS LE CAS DE FASTTAB
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! !      NOM       |MODE!                   ROLE
! !________________|____!______________________________________________
! ! TYPELE         !<-- ! TYPE D'ELEMENT DU MAILLAGE (ICI TRIANGLES)
! ! NGEO           ! -->! NUMERO DU CANAL DU FICHIER MAILLEUR
! !________________!____!______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : HOMERE
! APPEL DE :
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,MESH,NDP,NPOIN
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) ::       NGEO
      CHARACTER(LEN=*), INTENT(INOUT) :: TYPELE
!
      CHARACTER(LEN=80)  LIGNE
      INTEGER       IE,J
      INTEGER       ELMLOC(8)
!
! - INITIALISATION
!
      REWIND (NGEO)
      NPOIN = 0
      NELEM = 0
!
!  - BOUCLE DE LECTURE DU FICHIER DE MAILLAGE
!  - TANT QUE NON FIN DE FICHIER
!
 1    CONTINUE
        READ (NGEO, '(A80)', END=9000, ERR=8000) LIGNE
!
! - LA LIGNE COMMENCE PAR "GNN" - DEINITION D'UN POINT
!
        IF (LIGNE(1:3).EQ.'GNN') THEN
          NPOIN = NPOIN + 1
        ENDIF
!
! - LA LIGNE COMMENCE PAR "GE" - DEINITION D'UN ELEMENT
!
        IF (LIGNE(1:2).EQ.'GE') THEN
          NELEM = NELEM + 1
          READ(LIGNE(4:80),*,ERR=8000,END=9000) IE,
     &     (ELMLOC(J),J=1,8)
          IF (ELMLOC(8).NE.0.OR.
     &         (ELMLOC(4).NE.0.AND.ELMLOC(6).EQ.0) ) THEN
!
! - ON RAJOUTERA DES ELEMENTS
!
            NELEM = NELEM + 1
          ENDIF
        ENDIF
      GOTO 1
!
 9000 CONTINUE
      TYPELE = 'TRIANGLES  '
      NDP = 3
      MESH = 3
!
      RETURN
!
! - TRAITEMENT ERREUR DE FICHIER
!
 8000 CONTINUE
      WRITE (LU,4001)
 4001 FORMAT (//,1X,'****************************'
     &        ,/,1X,'SUBROUTINE INIFAS :'
     &        ,/,1X,'ERROR READING FASTTABS FILE.'
     &        ,/,1X,'****************************')
      CALL PLANTE(1)
      STOP
      END
