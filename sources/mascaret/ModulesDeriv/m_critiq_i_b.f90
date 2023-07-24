MODULE M_CRITIQ_I_B
  IMPLICIT NONE

  INTERFACE 
      SUBROUTINE CRITIQ_B(zcrit, zcritb, section, zref, q, qb, cf1, cf1b&
&       , cf2, cf2b, idt, xdt, profil, profil_plan, modelelit, &
&       loifrottement, unitelisting, erreur)
!
! **********************************************************************
!
!   FONCTION :
!   --------
!
!   CALCUL DE LA COTE CRITIQUE DANS LA SECTION Section
!
! ----------------------------------------------------------------------
! ARGUMENTS
! .__________________.____._______________________________________________
! !    NOM      !TYPE!MODE!                   ROLE
! !_____________!____!____!______________________________________________
! ! ZCRIT       ! R  !<-- ! COTE CRITIQUE
! ! Section           ! I  ! -->! SECTION DU CALCUL DE LA COTE CRITIQUE
! ! ZREF        ! R  ! -->! COTE DU FOND
! ! Q           ! R  ! -->! DEBIT GLOBAL
! ! CF1         ! R  ! -->! COEF. DE FROTTEMENT , LIT MINEUR
! ! CF2         ! R  ! -->! COEF. DE FROTTEMENT , LIT MAJEUR
! ! IDT         ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
! ! XDT         ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
! ! Profil      ! T  ! -->! Caracteristiques du planimetrage d'un profil
! ! Profil_plan ! T  ! -->! Variables de profil planimetrees
! ! ModeleLit   ! I  ! -->! Modele du lit
! ! Erreur      ! T  !<-->! Erreur
! !_____________!____!____!______________________________________________
!
! VARIABLES LOCALES
! .___________.________________________________________________________
! !           !    !    !
! !___________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   -----------------------
!
!   SOUS PROGRAMME APPELANT :  PERMAT
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   RHSBP_S : CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE SECTION
!
!   REPAR : CALCUL DE LA REPARTITION DES DEBITS ENTRE LE LIT MINEUR
!           ET LE LIT MAJEUR ACTIF
!
!   COMMENTAIRES :
!   ------------
!
! ----------------------------------------------------------------------
!============================= Declarations ===========================
!.. Modules importes ..
!----------------------
        USE M_PRECISION
! Constantes nommees
        USE M_PARAMETRE_C
! Types derives
! Type PROFIL_T
        USE M_PROFIL_T
! Type PROFIL_PLAN_T
        USE M_PROFIL_PLAN_T
! Type ERREUR_T
        USE M_ERREUR_T
! Procedures-module
! Sous-programme RHSBP_S
        USE M_RHSBP_S_B
! Traitement des erreurs
        USE M_TRAITER_ERREUR_I
! Interfaces
        USE M_REPAR_I_B
        IMPLICIT NONE
!.. Arguments ..
! --------------
        DOUBLE PRECISION :: zcrit
        DOUBLE PRECISION :: zcritb
        INTEGER, INTENT(IN) :: section
! TABLEAUX DIMENSIONNES A NMSCAL
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
        DOUBLE PRECISION, INTENT(IN) :: q
        DOUBLE PRECISION :: qb
        DOUBLE PRECISION, INTENT(IN) :: cf1
        DOUBLE PRECISION :: cf1b
        DOUBLE PRECISION, INTENT(IN) :: cf2
        DOUBLE PRECISION :: cf2b
        INTEGER, DIMENSION(:), INTENT(IN) :: idt
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
        TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
        TYPE(PROFIL_PLAN_T), INTENT(IN) :: profil_plan
        INTEGER, INTENT(IN) :: modelelit
        INTEGER, INTENT(IN) :: loifrottement
        INTEGER, INTENT(IN) :: unitelisting
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
      END SUBROUTINE CRITIQ_B
  END INTERFACE

END MODULE M_CRITIQ_I_B

