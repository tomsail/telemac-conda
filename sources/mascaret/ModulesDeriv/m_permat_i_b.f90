MODULE M_PERMAT_I_B
  IMPLICIT NONE

  INTERFACE
      SUBROUTINE PERMAT_B(z, zb, q, qb, zinit, zinitb, x, zref, cf1, &
&       cf1b, cf2, cf2b, pcsing, pcsingb, idt, xdt, profil, profilplan, &
&       f1, connect, numbief, nbsect, singularite, &
&       modelelit, impression, unitelisting, temps, loifrottement, &
&       cqmv, decentrement, erreur)
! **********************************************************************
!   FONCTION :
!   --------
!
!   CALCUL DE LIGNE D'EAU DANS UN BIEF EN REGIME PERMANENT
!   MODELISATION UNIDIMENSIONNELLE
!   LITS MINEUR ET MAJEUR - HYPOTHESES DEBORD OU PRADO
!   SINGULARITES
!
! ----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   -----------------------
!
!   - UniteListing   : IMPRESSION DES RESULTATS GLOBAUX
!
!   SOUS PROGRAMMES APPELANTS :  PERSAR
!   ---------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   CRITIQ : CALCUL DE LA COTE CRITIQUE
!   FROUDE : CALCUL DU NOMBRE DE FROUDE
!   PSING  : TRAITEMENT DES SINGULARITES EN PERMANENT
!   REPAR  : CALCUL DE LA REPARTITION DES DEBITS ENTRE LE LIT MINEUR
!            ET LE LIT MAJEUR ACTIF, HYPOTHESES 'DEBORD'
!   RHSBP_S: CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE SECTION
!
!   COMMENTAIRES :
!   ------------
!
!   LE TERME REPRESENTANT LES PERTES DE CHARGE REGULIERES EST PRIS
!   SELON LA MOYENNE HARMONIQUE DES
!   COEFFICIENTS CALCULES DANS CHACUNE DES SECTIONS AMONT ET AVAL:
!                  2      1     1
!                ----- = --- + ---
!                JAVAM   JAV   JAM
!
!   LES PERTES DE CHARGE SINGULIERES SONT :
!      - RALENTISSEMENT : JS=CPCS*(BETAAM*VAM-BETAAV*VAV)**2/2./G
!      - OBSTACLE  EN A : JS=PCSing(A)*BETAAM*VAM**2/2./G
!
!   LE CALCUL EST SUPPOSE SE FAIRE EN REGIME FLUVIAL.
!   TOUTEFOIS SI AU COURS DES ITERATIONS ON DETECTE UN REGIME FLUVIAL
!   A L'AVAL ET TORRENTIEL A L'AMONT , ON IMPOSE ALORS Z=ZCRITIQUE A
!   LA SECTION AMONT , CECI TANT QU'ON N'A PAS RETROUVE UN ECOULEMENT
!   FLUVIAL. DE PLUS UN TEST EST REALISE POUR VERIFIER QUE CE PASSAGE
!   FLUVIAL AMONT - CRITIQUE AVAL N'EST PAS TROP BRUTAL
!
!   DOCUMENTATION EXTERNE :
!   ---------------------
!
!   NOTE D'UTILISATION       : RAPPORT HE-43/92.19
!   NOTE DE PRINCIPE         : RAPPORT HE-43/92.64
!
! ----------------------------------------------------------------------
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
        USE M_RHSBP_S_B
! Numero de bief d'une section
        USE M_NUM_BIEF_S
! Calcul du nombre de Froude
        USE M_FROUDE_S
! Interface generique de traitement des erreurs
        USE M_TRAITER_ERREUR_I
! Interfaces
        USE M_REPAR_I
        USE M_REPAR_I_B
        USE M_PSING_I_B
        USE M_CRITIQ_I_B
        USE M_CRITIQ_I
        IMPLICIT NONE
!.. Arguments ..
!---------------
! TABLEAU  DIMENSIONNE  A NbSect
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: z
        DOUBLE PRECISION, DIMENSION(:) :: zb
! TABLEAU  DIMENSIONNE  A NbSect
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: q
        DOUBLE PRECISION, DIMENSION(:) :: qb
        DOUBLE PRECISION, INTENT(IN) :: zinit
        DOUBLE PRECISION :: zinitb
! TABLEAUX DIMENSIONNES A NMSCAL
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
        DOUBLE PRECISION, DIMENSION(:) :: cf1b
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
        DOUBLE PRECISION, DIMENSION(:) :: cf2b
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: pcsing
        DOUBLE PRECISION, DIMENSION(:) :: pcsingb
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
      END SUBROUTINE PERMAT_B
  END INTERFACE

END MODULE M_PERMAT_I_B

