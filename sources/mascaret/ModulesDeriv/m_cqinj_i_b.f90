MODULE M_CQINJ_I_B
  IMPLICIT NONE

  INTERFACE 
      SUBROUTINE CQINJ_B(qinjec, qinjecb, x, z, zb, apport, deversoir, &
&                        qdeverse, erreur)
!
! **********************************************************************
!   FONCTION : CALCUL DU TABLEAU QINJEC DES APPORTS (DEBITS + DEVRESOIRS)
!   --------
!
! ----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :     - UL_LST : Sortie listing
!   ----------------------      
!   SOUS PROGRAMMES APPELANTS :  - REZO, SARAP
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    - INTERPOLATION_S
!   -------------------------
!=========================================================================
!=========================== Declarations ================================
! Type DOUBLE
        USE M_PRECISION
! Parametres de calcul
        USE M_PARAMETRE_C
! Liste des messages d'erreur
        USE M_MESSAGE_C
! Numero du canal UL_LST
        USE M_FICHIER_T
! Definition du type DEVERSOIR_T
        USE M_DEVERSOIR_T
! Definition du type APPORT_T
        USE M_APPORT_T
! Definition du type ERREUR_T
        USE M_ERREUR_T
! Interpolation
        USE M_INTERPOLATION_S
        USE M_INTERPOLATION_S_B
! Traitement des erreurs
        USE M_TRAITER_ERREUR_I
        IMPLICIT NONE
!.. Arguments .. 
        DOUBLE PRECISION, DIMENSION(:) :: qinjec
        DOUBLE PRECISION, DIMENSION(:) :: qinjecb
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: z
        DOUBLE PRECISION, DIMENSION(:) :: zb
        TYPE(APPORT_T), DIMENSION(:), INTENT(IN) :: apport
        TYPE(DEVERSOIR_T), DIMENSION(:), INTENT(IN) :: deversoir
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qdeverse
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
      END SUBROUTINE CQINJ_B
  END INTERFACE

END MODULE M_CQINJ_I_B

