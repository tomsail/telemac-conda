MODULE M_PERMAT_I_D
  IMPLICIT NONE
  INTERFACE
      SUBROUTINE PERMAT_D(z, zd, q, qd, zinit, zinitd, x, zref, cf1, &
&       cf1d, cf2, cf2d, pcsing, pcsingd, idt, xdt, profil, profilplan, &
&       f1, connect, numbief, nbsect, singularite, &
&       modelelit, impression, unitelisting, temps, loifrottement, &
&       cqmv, decentrement, erreur)
!
!============================= Declarations ===========================
!.. Modules importes ..
!----------------------
        USE M_PRECISION
! Constantes nommees
! GPES
        USE M_PARAMETRE_C
! Messages d'erreur
        USE M_MESSAGE_C
! Types derives
! Type PROFIL_T
        USE M_PROFIL_T
! Type PROFIL_PLAN_T
        USE M_PROFIL_PLAN_T
! Type SINGULARITE_T
        USE M_SINGULARITE_T
! Type EREUR_T
        USE M_ERREUR_T
! Type CONNECT_T
        USE M_CONNECT_T
! Procedures-module
! Sous-programme RHSBP_S
        USE M_RHSBP_S
        USE M_RHSBP_S_D
! Sous programme RHSBP1
        USE M_RHSB1_S
! Numero de bief d'une section
        USE M_NUM_BIEF_S
! Calcul du nombre de Froude
        USE M_FROUDE_S
! Interface generique de traitement des erreurs
        USE M_TRAITER_ERREUR_I
! Interfaces
        USE M_REPAR_I
        USE M_REPAR_I_D
        USE M_PSING_I_D
        USE M_CRITIQ_I_D
        IMPLICIT NONE
!.. Arguments ..
!---------------
! TABLEAU  DIMENSIONNE  A NbSect
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: z
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: zd
! TABLEAU  DIMENSIONNE  A NbSect
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: q
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: qd
        DOUBLE PRECISION, INTENT(IN) :: zinit
        DOUBLE PRECISION, INTENT(IN) :: zinitd
! TABLEAUX DIMENSIONNES A NMSCAL
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1d
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2d
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: pcsing
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: pcsingd
        INTEGER, DIMENSION(:), INTENT(IN) :: idt
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
        TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
        TYPE(PROFIL_PLAN_T), INTENT(IN) :: profilplan
        DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: f1
        INTEGER, INTENT(IN) :: numbief
        TYPE(CONNECT_T), INTENT(IN) :: connect
        TYPE(SINGULARITE_T), DIMENSION(:), INTENT(IN) :: singularite
        INTEGER, INTENT(IN) :: modelelit
        LOGICAL, INTENT(IN) :: impression
        INTEGER, INTENT(IN) :: unitelisting
        DOUBLE PRECISION, INTENT(IN) :: temps
        INTEGER, INTENT(IN) :: loifrottement
        INTEGER, INTENT(IN) :: nbsect
        INTEGER, INTENT(IN) :: cqmv
        LOGICAL, INTENT(IN) :: decentrement
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
      END SUBROUTINE PERMAT_D
  END INTERFACE
END MODULE M_PERMAT_I_D

