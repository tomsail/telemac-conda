MODULE M_QREPAR_I_B
  IMPLICIT NONE

  INTERFACE 
      SUBROUTINE QREPAR_B(sommedebitance, sommedebitanceb, zaval, zavalb&
&       , numpassage, q, qb, z, zb, zref, x, cf1, cf1b, cf2, cf2b, idt, &
&       xdt, profil, profil_plan, numconfluence, connect, modelelit, &
&       epsil, dzprev, unitelisting, loifrottement, erreur)
!
! **********************************************************************
!
!   FONCTION :
!   --------
!
!   REPARTITION DES DEBITS AU NOEUD NumConfluence
!   CAS DE PLUSIEURS BRANCHES AVAL
!
! ----------------------------------------------------------------------
! ARGUMENTS
! ._______________.____.____._______________________________________________
! ! NOM           !TYPE!MODE! ROLE
! !_______________!____!____!______________________________________________
! ! SommeDebitance! R  !<-- ! SOMME DES DEBITANCES DES BRANCHES AVAL
! ! ZAval         ! R  !<-- ! TABLEAU DE COTE (INTERNE A QREPAR)
! ! NumPassage    ! I  !<-->! VARIABLE INDICATRICE :
! !               !    !    !  = 1  CALCUL DU DEBIT DANS LA BRANCHE AVAL
! !               !    !    !  = 2  CALCUL DES COTES DANS LES BRANCHES AMONT
! ! Q             ! R  !<-->! TABLEAU DES DEBITS
! ! Z             ! R  ! -->! TABLEAU DES COTES DE LA SURFACE LIBRE
! ! ZREF          ! R  ! -->! COTES DU FOND DU LIT
! ! X             ! R  ! -->! ABSCISSES DES SECTIONS DE CALCUL
! ! CF1           ! R  ! -->! COEF. DE FROTTEMENT , LIT MINEUR
! ! CF2           ! R  ! -->! COEF. DE FROTTEMENT , LIT MAJEUR
! ! IDT           ! I  ! -->!  )PERMET DE DETERMINER LA SECTION DE CALCUL A
! ! XDT           ! R  ! -->!  )PARTIR DES SECTIONS DE DONNEES
! ! Profil        ! T  ! -->! Caracteristiques du planimetrage d'un profil
! ! Profil_plan   ! R  ! -->! Variables de profil planimetrees
! ! NumConfluence ! I  ! -->! NUMERO DU NOEUD A TRAITER
! ! Connect       ! T  ! -->! Structure contenant la table de connectivite
! ! ModeleLit     ! I  ! -->! Modele du lit
! ! Erreur        ! T  !<-->! Erreur
! !_______________!____!____!______________________________________________
!
! VARIABLES LOCALES
! ._______________.____.____.______________________________________________
! !               !    !    !
! !_______________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS PROGRAMME APPELANT :  PERSAR
!   -------------------------
!   SOUS PROGRAMMES APPELES :  
!   -------------------------
!
!   INTERPOLATION_S : INTERPOLATION DE LAGRANGE D'ODRE N
!
!   REPAR           : CALCUL DE LA REPARTITION DES DEBITS ENTRE
!                     LE LIT MINEUR ET LE LIT MAJEUR
!
!   RHSBP_S :  CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE
!              SECTION
!
!   COMMENTAIRES :
!   ------------
!
! ----------------------------------------------------------------------
!
!============================= Declarations ===========================
!.. Modules importes ..
!----------------------
        USE M_PRECISION
! Types derives
! Type CONNECT_T
        USE M_CONNECT_T
! Type PROFIL_T
        USE M_PROFIL_T
! Type PROFIL_PLAN_T
        USE M_PROFIL_PLAN_T
! Type ERREUR_T
        USE M_ERREUR_T
! Procedures-module
! Sous-programme INTERPOLATION_S
        USE M_INTERPOLATION_S_B
! Sous-programme RHSBP_GENERIQUE_S
        USE M_RHSBP_S_B
! Traitement de l'erreur 
        USE M_TRAITER_ERREUR_I
! Interfaces
        USE M_REPAR_I_B
        IMPLICIT NONE
!.. Arguments ..
! --------------
! TABLEAUX DIMENSIONNES A NMPLAN
        DOUBLE PRECISION, DIMENSION(:) :: sommedebitance
        DOUBLE PRECISION, DIMENSION(:) :: sommedebitanceb
        DOUBLE PRECISION, DIMENSION(:) :: zaval
        DOUBLE PRECISION, DIMENSION(:) :: zavalb
        INTEGER, INTENT(INOUT) :: numpassage
! TABLEAU  DIMENSIONNE  A NbSect
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: q
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qb
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: zb
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: zref
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf1
        DOUBLE PRECISION, DIMENSION(:) :: cf1b
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: cf2
        DOUBLE PRECISION, DIMENSION(:) :: cf2b
        INTEGER, DIMENSION(:), INTENT(IN) :: idt
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: xdt
        TYPE(PROFIL_T), DIMENSION(:), INTENT(IN) :: profil
        TYPE(PROFIL_PLAN_T), INTENT(IN) :: profil_plan
        INTEGER, INTENT(IN) :: numconfluence
        TYPE(CONNECT_T), INTENT(IN) :: connect
        INTEGER, INTENT(IN) :: modelelit
        DOUBLE PRECISION, INTENT(OUT) :: epsil
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: dzprev
        INTEGER, INTENT(IN) :: unitelisting
        INTEGER, INTENT(IN) :: loifrottement
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
      END SUBROUTINE QREPAR_B
  END INTERFACE

END MODULE M_QREPAR_I_B

