!                       *****************
                        SUBROUTINE IMPRIM
!                       *****************
!
     &(NPOIN1,NPOIN,TYPELE,NELEM,TITRE,MAILLE,PRECIS)
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2        31/08/89    J-C GALLAND
!
!     FONCTION  : IMPRESSION DANS LE LISTING D'INFORMATIONS GENERALES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | NPOIN1         | -->| NOMBRE DE POINT DONNES PAR LE MAILLEUR
! | NPOIN          | -->| NOMBRE REEL DE POINTS
! | TYPELE         | -->| TYPE DES ELEMENTS
! | NELEM          | -->| NOMBRE D'ELEMENTS
! | TITRE          | -->| TITRE DU MAILLAGE
! | MAILLE         | -->| NOM DU MAILLEUR UTILISE
! | PRECIS         | -->| FORMAT DE LECTURE DES COORDONNEES DES NOEUDS
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : HOMERE
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOIN1, NPOIN, NELEM
!
      CHARACTER(LEN=11), INTENT(IN) :: TYPELE
      CHARACTER(LEN=80), INTENT(IN) :: TITRE
      CHARACTER(LEN=9), INTENT(IN) ::  MAILLE
      CHARACTER(LEN=6), INTENT(IN) ::  PRECIS
!
!-----------------------------------------------------------------------
!
      WRITE(LU,3010) MAILLE
!
 3010 FORMAT(/,1X,'MESH GENERATOR : ',A9,/,
     &         1X,'--------------')
!
      IF (MAILLE(1:8).EQ.'SUPERTAB') WRITE(LU,3020) PRECIS
!
 3020 FORMAT(1X,'(COORDINATES OF NODES ARE READ IN ',A6,' PRECISION)',/)
!
      WRITE(LU,3030) TITRE,NPOIN1,NPOIN,NELEM,TYPELE
!
 3030 FORMAT(//,1X,'DATAS READ IN THE UNIVERSAL FILE',
     &        /,1X,'---------------------------------',/
     &        /,1X,'TITLE OF THE MESH                 : ',A72,
     &        /,1X,'REAL NUMBER OF POINTS             : ',I11,
     &        /,1X,'MAX. NUMBER GIVEN BY THE MESH GENERATOR: ',I11,
     &        /,1X,'TOTAL NUMBER OF ELEMENTS           : ',I11,
     &        /,1X,'TYPE OF ELEMENTS                   :      ',A11)
!
      RETURN
      END
