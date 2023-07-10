MODULE M_SARAP_I_B
  IMPLICIT NONE

  INTERFACE
      SUBROUTINE SARAP_B(z, zb, q1, q2, p1, p1b, p2, p2b, b1, b1b, b2, &
&       b2b, bs, rh1, rh1b, rh2, rh2b, s1, s1b, s2, s2b, beta, betab, &
&       froude, extremite, apport, qinjec, qinjecb, qdeverse, temps, &
&       profil, profilplan, f1, x, cf1, cf1b, cf2, cf2b, zref, xdt, idt&
&       , connect, singularite, pcsing, pcsingb, deversoir&
&       , modelelit, confluent, abaque, algorithme, &
&       impression, unitelisting, loifrottement, pertechargeconfluent, &
&       cqmv, decentrement, erreur)
! .....................................................................
! .          CALCUL EN REGIME PERMANENT A L'AIDE DU CODE SARA         .
! .          ADAPTE A LA RESOLUTION D'UN RESEAU RAMIFIE               .
! .....................................................................
!
!
!
! .....................................................................
! . ADAPTE A LA RESOLUTION D'UN RESEAU RAMIFIE                        .
! . LIT MINEUR / MAJEUR                                               .
! .                                                                   .
! . LE SENS DE PARCOURS DU RESEAU EST DETERMINE DANS LE SOUS-PROGRAMME.
! . ALGOP, APPELE LORS DE LA PHASE DE LECTURE DES DONNEES             .
! .....................................................................
!
!
! .....................................................................
! . FICHIERS EN SORTIE                                                .
! .....................................................................
! . NUMERO . SUPPORT PHYSIQUE . ROLE                                  .
! .....................................................................
! . UniteListing   . FICHIER LISTING  . IMPRESSION DES PRINCIPAUX RESULTATS   .
! .....................................................................
!============================= Declarations ===========================
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
        USE M_CQINJ_I_B
        USE M_PERSAR_I
        USE M_PERSAR_I_B
        USE M_REPAR_I
        IMPLICIT NONE
!.. Donnees/Resultats ..
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z, q1, q2
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zb
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: p1, p2
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: p1b, p2b
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: b1, b2, bs
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: b1b, b2b
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: rh1, rh2
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: rh1b, rh2b
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: s1, s2
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: s1b, s2b
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: beta
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: betab
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: froude
! Conditions aux limites des extremites libres
        TYPE(EXTREMITE_T), DIMENSION(:), INTENT(IN) :: extremite
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
! Maillage
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x, cf1, cf2
        DOUBLE PRECISION, DIMENSION(:) :: cf1b, cf2b
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
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qinjecb
! Pertes de charge singulieres
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsing
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsingb
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
      END SUBROUTINE SARAP_B
  END INTERFACE

END MODULE M_SARAP_I_B

