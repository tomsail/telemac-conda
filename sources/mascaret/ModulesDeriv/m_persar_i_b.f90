MODULE M_PERSAR_I_B
  IMPLICIT NONE

  INTERFACE
      SUBROUTINE PERSAR_B(z, zb, q, qb, x, zref, cf1, cf1b, cf2, cf2b, &
&       pcsing, pcsingb, idt, xdt, profil, profilplan, f1, qinjec, &
&       qinjecb, connect, singularite, extremite, &
&       modelelit, confluent, abaque, impression, unitelisting, temps, &
&       algorithme, loifrottement, pertechargeconfluent, cqmv, &
&       decentrement, erreur)
! **********************************************************************
!
!   FONCTION :
!   ----------
!
!   CALCUL DE LA LIGNE D'EAU
!
!-----------------------------------------------------------------------
! ARGUMENTS
! .________________.____.____._______________________________________________
! !    NOM         !TYPE!MODE!                   ROLE
! !________________!____!____!_______________________________________________
! ! Z              ! R  !<-- ! COTE DE LA SURFACE LIBRE
! ! Q              ! R  !<-- ! DEBITS
! ! X              ! R  ! -->! ABSCISSE DES SECTIONS DE CALCUL(SUR LE BIEF)
! ! ZREF           ! R  ! -->! COTE DU FOND
! ! CF1,CF2        ! R  ! -->! COEF. DE FROTTEMENT ,  1= MINEUR   2= MAJEUR
! ! PCSing         ! R  ! -->! PERTE DE CHARGE SINGULIERE
! ! IDT            ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
! ! XDT            ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
! ! Profil         ! R  ! -->! Caracteristiques du planimetrage d'un profil
! ! ProfilPlan     ! R  ! -->! SECTION MOUILLEE  ZONE DE STOCKAGE
! ! QInjec         ! R  ! -->! DEBIT APPORTE OU SOUTIRE
! ! Connect        ! T  ! -->! Structure contenant la table de connectivite
! ! Singularite    ! T  ! -->! Structure decrivant la singularite
! ! Extremite      ! T  ! -->! Structure decrivant les extremites
! ! ModeleLit      ! I  ! -->! Modele du lit
! ! Temps          ! R  ! -->! Temps
! ! Algorithme     ! I  ! -->! TABLEAUX DES APPELS AUX S.P. RESOLVANT LE
! !                !    !    ! CALCUL DE LIGNES D'EAU :
! !                !    !    !  Algorithme = ISUB + I
! !                !    !    !   AVEC  ISUB = 100  ==>       QBIEF
! !                !    !    !         ISUB = 200  ==>  CALL PERMAT
! !                !    !    !         ISUB = 300  ==>  CALL QNODE (1ER PAS)
! !                !    !    !         ISUB = 400  ==>  CALL QNODE (2EM PAS)
! !                !    !    !         ISUB = 500  ==>  CALL QREPAR(1ER PAS)
! !                !    !    !         ISUB = 600  ==>  CALL QREPAR(2EM PAS)
! !                !    !    !     ET   I = LE NUMERO DU NOEUD OU DU BIEF
! ! Erreur         ! T  !<-->! Erreur
! !________________!____!____!______________________________________________
!
! VARIABLES LOCALES
! .______________________________________________________________________
! !   IA        ! I  ! -- ! COMPTEUR DU TABLEAU DES APPELS
! !   ICOMPT    ! I  ! -- ! COMPTEUR LIMITANT LE NOMBRE D'ITERATION
! !   NumPassage! I  ! -- ! INDICATEUR POUR LES APPELS A QNODE ET QREPAR
! !   noeud_bief! I  ! -- ! NUMERO DU NOEUD OU DU BIEF CONSIDERE
! !SommeDebitance R  ! -->! SOMME DES DEBITANCES DES BRANCHES AVAL
! !   ZAval     ! R  ! -->! TABLEAU DE COTE (INTERNE A QREPAR)
! !_____________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ------------------------
!   SOUS PROGRAMME APPELANT :  SARAP
!   -------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   - PERMAT :  SOUS-PROGRAMME DU CALCUL DE LA LIGNE D'EAU
!               DANS UN BIEF
!   - QNODE  :  REPARTITION DES DEBITS A UN NOEUD AVEC
!               EGALITE DES COTES ( UNE SEULE BRANCHE AVAL)
!   - QREPAR :  REPARTITION DES DEBITS A UN NOEUD AVEC
!               EGALITE DES COTES (PLUSIEURS BRANCHES AVAL)
!
!   COMMENTAIRES :
!   --------------
! **********************************************************************
!============================= Declarations ===========================
!
!.. Modules importes ..
!----------------------
        USE M_PRECISION
! Messages d'erreur
        USE M_MESSAGE_C
! Types derives
! Type confluent
        USE M_CONFLUENT_T
! Definition du Type CONNECT_T
        USE M_CONNECT_T
! Definition du Type EXTREMITE_T
        USE M_EXTREMITE_T
! Definition du Type PROFIL_T
        USE M_PROFIL_T
! Definition du Type PROFIL_PLAN_T
        USE M_PROFIL_PLAN_T
! Definition du type ERREUR_T
        USE M_ERREUR_T
! Definition du Type SINGULARITE_T
        USE M_SINGULARITE_T
! Interfaces
! Calcul des pertes de charge auto aux confluents
        USE M_CALC_PC_CONFLU_I_B
        USE M_PERMAT_I
        USE M_PERMAT_I_B
        USE M_QNODE_I_B
        USE M_QREPAR_I_B
! Traitement de l'erreur
        USE M_TRAITER_ERREUR_I
        IMPLICIT NONE
!.. Arguments ..
! --------------
! TABLEAUX DIMENSIONNES A NMSCAL
        DOUBLE PRECISION, DIMENSION(:) :: z
        DOUBLE PRECISION, DIMENSION(:) :: zb
        DOUBLE PRECISION, DIMENSION(:) :: q
        DOUBLE PRECISION, DIMENSION(:) :: qb
! TABLEAUX DIMENSIONNES A NMSCAL
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
        DOUBLE PRECISION, DIMENSION(:) :: cf1b
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
        DOUBLE PRECISION, DIMENSION(:) :: cf2b
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsing
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: pcsingb
        INTEGER, DIMENSION(:), INTENT(IN) :: idt
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
        TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
        TYPE(PROFIL_PLAN_T), INTENT(IN) :: profilplan
        DOUBLE PRECISION, DIMENSION(:, :), INTENT(IN) :: f1
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: qinjec
        DOUBLE PRECISION, DIMENSION(:) :: qinjecb
        TYPE(CONNECT_T), INTENT(IN) :: connect
        TYPE(SINGULARITE_T), DIMENSION(:), INTENT(IN) :: singularite
        TYPE(EXTREMITE_T), DIMENSION(:), INTENT(IN) :: extremite
        INTEGER, INTENT(IN) :: modelelit
        TYPE(CONFLUENT_T), DIMENSION(:), INTENT(IN) :: confluent
        DOUBLE PRECISION, DIMENSION(:, :, :), INTENT(IN) :: abaque
        LOGICAL, INTENT(IN) :: impression
        INTEGER, INTENT(IN) :: unitelisting
        DOUBLE PRECISION, INTENT(IN) :: temps
        INTEGER, DIMENSION(:), INTENT(IN) :: algorithme
        INTEGER, INTENT(IN) :: loifrottement
        INTEGER, INTENT(IN) :: cqmv
        LOGICAL, INTENT(IN) :: pertechargeconfluent
        LOGICAL, INTENT(IN) :: decentrement
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
      END SUBROUTINE PERSAR_B
  END INTERFACE

END MODULE M_PERSAR_I_B

