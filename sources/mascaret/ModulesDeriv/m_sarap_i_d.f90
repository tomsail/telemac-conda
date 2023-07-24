MODULE M_SARAP_I_D
  INTERFACE
      SUBROUTINE SARAP_D(z, zd, q1, q2, p1, p1d, p2, p2d, b1, b1d, b2, &
&       b2d, bs, rh1, rh1d, rh2, rh2d, s1, s1d, s2, s2d, beta, betad, &
&       froude, extremite, apport, qinjec, qdeverse, temps, profil, &
&       profilplan, f1, x, cf1, cf1d, cf2, cf2d, zref, xdt, idt, connect&
&       , singularite, pcsing, pcsingd, deversoir, modelelit, confluent, &
&       abaque, algorithme, impression, unitelisting, loifrottement, &
&       pertechargeconfluent, cqmv, decentrement, erreur)

        USE M_PRECISION
! Constantes nommees
        USE M_CONSTANTES_CALCUL_C
        USE M_MESSAGE_C
        USE M_PARAMETRE_C
! Types derives
        USE M_APPORT_T
! Type confluent
        USE M_CONFLUENT_T
        USE M_CONNECT_T
        USE M_EXTREMITE_T
        USE M_SINGULARITE_T
        USE M_DEVERSOIR_T
        USE M_PROFIL_T
        USE M_PROFIL_PLAN_T
        USE M_ERREUR_T
! Procedures-module
! Sous-programme RHSBP_GENERIQUE_S
        USE M_RHSBP_S
! Interface generique d'appel aux
        USE M_TRAITER_ERREUR_I
! procedures de traitement des erreurs
! Calcul du numero du bief d'une section
        USE M_NUM_BIEF_S
! Calcul de Froude
        USE M_FROUDE_S
! Interfaces
        USE M_CQINJ_I
        USE M_PERSAR_I_D
        USE M_REPAR_I
        IMPLICIT NONE
!.. Donnees/Resultats ..
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z, q1, q2
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zd
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: p1, p2
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: p1d, p2d
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: b1, b2, bs
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: b1d, b2d
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: rh1, rh2
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: rh1d, rh2d
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: s1, s2
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: s1d, s2d
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: beta
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: betad
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: froude
! Conditions aux limites des extremites libres
        TYPE(EXTREMITE_T), DIMENSION(:), INTENT(IN) :: extremite
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
! Maillage
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x, cf1, cf2
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1d, cf2d
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
        INTEGER, DIMENSION(:), INTENT(IN) :: idt
! Variables planimetrees
        TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
        TYPE(PROFIL_PLAN_T), INTENT(IN) :: profilplan
        DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: f1
! Debits d apports
        TYPE(APPORT_T), DIMENSION(:), INTENT(IN) :: apport
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qdeverse
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qinjec
! Pertes de charge singulieres
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsing
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsingd
! Table du connectivite du reseau
        TYPE(CONNECT_T), INTENT(IN) :: connect
! Algorithme de resolution
        INTEGER, DIMENSION(:), INTENT(IN) :: algorithme
! Singularites
        TYPE(SINGULARITE_T), DIMENSION(:), INTENT(IN) :: singularite
! Modelisation du lit
        INTEGER, INTENT(IN) :: modelelit
! Confluences
        TYPE(CONFLUENT_T), DIMENSION(:), INTENT(IN) :: confluent
        DOUBLE PRECISION, DIMENSION(6, 6, 5), INTENT(IN) :: abaque
! Parametres
        LOGICAL, INTENT(IN) :: impression
        INTEGER, INTENT(IN) :: unitelisting
        INTEGER, INTENT(IN) :: loifrottement
        INTEGER, INTENT(IN) :: cqmv
        LOGICAL, INTENT(IN) :: pertechargeconfluent
        LOGICAL, INTENT(IN) :: decentrement
! Temps
        DOUBLE PRECISION, INTENT(IN) :: temps
! Deversoirs
        TYPE(DEVERSOIR_T), DIMENSION(:), INTENT(IN) :: deversoir
      END SUBROUTINE SARAP_D
  END INTERFACE
END MODULE M_SARAP_I_D

