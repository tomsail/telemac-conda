MODULE M_REPAR_I_B
  IMPLICIT NONE

  INTERFACE 
      SUBROUTINE REPAR_B(deb, debb, vmoy, vmoyb, beta, betab, q1, q2, s1&
&       , s1b, s2, s2b, rh1, rh1b, rh2, rh2b, p1, p1b, p2, p2b, q, qb, &
&       cf1, cf1b, cf2, cf2b, modelelit, loifrottement, erreur)
! **********************************************************************
!   FONCTION :
!   --------
!   CALCUL DE LA REPARTITION DE DEBIT
!   MODELE DEBORD : REPARTITION LIT MINEUR/LIT MAJEUR , UTILISANT LE
!                   PARAMETRE DE MODIFICATION DES DEBITANCES A
!   MODELE CRUGOS : APPLICATION DE LA FORMULE DE COMPOSITION DES
!   ou Fond Berge   RUGOSITES D'EINSTEIN -  LE LIT EST ALORS CONSIDERE
!                   COMME UNIQUE, AVEC UN COEFFICIENT DE RUGOSITE
!                   FONCTION DES COEFFICIENTS INITIAUX, ET DES
!                   PARAMETRES GEOMETRIQUES
!   SI AUNCUN MODELE N'A ETE RETENU, LE MODELE DEBORD EST APPLIQUE EN
!   PRENANT LE PARAMETRE A EGAL A 1
!
! ----------------------------------------------------------------------
! ARGUMENTS
! .________________.____._______________________________________________
! !    NOM    !TYPE!MODE!                   ROLE
! !___________!____!____!______________________________________________
! !  DEB      ! R  !<-- ! DEBITANCE
! !  VMOY     ! R  !<-- ! VITESSE MOYENNE
! !  BETA     ! R  !<-- ! COEFFICIENT DE REPARTITION DE VITESSES MIN/MAJ
! !  Q1,Q2    ! R  !<-- ! DEBIT
! !  S1,S2    ! R  !<-->! SECTION MOUILLEE    ) INDICE 1 LIT MINEUR
! !  RH1,RH2  ! R  !<-->! RAYON HYDRAULIQUE   )        2 LIT MAJEUR
! !  Q        ! R  ! -->! DEBIT GLOBAL
! !  P1,P2    ! R  ! -->! PERIMETRE MOUILLE   )
! !  CF1,CF2  ! R  ! -->! COEF. DE FROTTEMENT )
! !  ModeleLit! I  ! -->! Type du modele du lit
! !  Erreur   ! T  ! -->! Erreur
! !___________!____!____!______________________________________________
!  VARIABLES LOCALES
! .___________.____.____.______________________________________________
! !  RH       ! R  !<-- ! RAYON HYDRAULIQUE
! !   A       ! R  ! -- ! PARAMETRE DU MODELE DEBORD
! !   PUT     ! R  ! -- ! VALEUR SEUIL POUR LE CALCUL DE A
! !___________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
! ----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   -------------------------
!   SOUS PROGRAMMES APPELANTS :  REZO, CRITIQ, PERMAT, QREPAR, SARAP
!   ---------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!   COMMENTAIRES :
!   --------------
!   POUR TRAITER UN LIT COMPOSE EN LIT UNIQUE, IL SUFFIT DE DEMANDER
!   LE MODELE CRUGOS, AVEC DES COEFFICIENTS DE RUGOSITE EGAUX
! ----------------------------------------------------------------------
!============================ Declarations ==============================
        USE M_PRECISION
        USE M_PARAMETRE_C
! MODELE_LIT
        USE M_CONSTANTES_CALCUL_C
! type ERREUR_T
        USE M_ERREUR_T
! Traitement de l'erreur
        USE M_TRAITER_ERREUR_I
        IMPLICIT NONE
!.. Formal Arguments .. 
        DOUBLE PRECISION :: deb
        DOUBLE PRECISION :: debb
        DOUBLE PRECISION :: vmoy
        DOUBLE PRECISION :: vmoyb
        DOUBLE PRECISION :: beta
        DOUBLE PRECISION :: betab
        DOUBLE PRECISION, INTENT(OUT) :: q1
        DOUBLE PRECISION, INTENT(OUT) :: q2
        DOUBLE PRECISION, INTENT(INOUT) :: s1
        DOUBLE PRECISION :: s1b
        DOUBLE PRECISION, INTENT(INOUT) :: s2
        DOUBLE PRECISION :: s2b
        DOUBLE PRECISION, INTENT(INOUT) :: rh1
        DOUBLE PRECISION :: rh1b
        DOUBLE PRECISION, INTENT(INOUT) :: rh2
        DOUBLE PRECISION :: rh2b
        DOUBLE PRECISION, INTENT(IN) :: q
        DOUBLE PRECISION :: qb
        DOUBLE PRECISION, INTENT(IN) :: p1
        DOUBLE PRECISION :: p1b
        DOUBLE PRECISION, INTENT(IN) :: p2
        DOUBLE PRECISION :: p2b
        DOUBLE PRECISION, INTENT(IN) :: cf1
        DOUBLE PRECISION :: cf1b
        DOUBLE PRECISION, INTENT(IN) :: cf2
        DOUBLE PRECISION :: cf2b
        INTEGER, INTENT(IN) :: modelelit
        INTEGER, INTENT(IN) :: loifrottement
        TYPE(ERREUR_T), INTENT(INOUT) :: erreur
      END SUBROUTINE REPAR_B
  END INTERFACE

END MODULE M_REPAR_I_B

