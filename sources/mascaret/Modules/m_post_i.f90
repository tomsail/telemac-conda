!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

module M_POST_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface
   subroutine POST( TAUF                      , &
                    Y           , HMOY        , &
                    Q2G         , Q2D         , &
                    VOL         , VOLS        , &
                    CHARG                     , &
                    SS                        , &
                    V1          , V2          , &
                    ZMAX        , TZMAX       , &
                    VZMAX                     , &
                    ZMIN        , TZMIN       , &
                    V1MIN       , V1MAX       , &
                    BMAX                      , &
                    TOND                      , &
                    QMAX        , TQMAX       , &
                    EMAX                      , &
! Donnees
                    Z                         , &
                    Q1          , Q2          , &
                    S1          , S2          , &
                    B1          , B2          , &
                    BS                        , &
                    P1          , P2          , &
                    RH1         , RH2         , &
                    BETA                      , &
                    PROF                      , &
                    PROFIL_PLAN               , &
                    Temps, TempsInitial       , &
                    Iteration                 , &
! Modele
                    X                         , &
                    ZREF                      , &
                    IDT         , XDT         , &
                    CF1                       , &
! Etats
                    ZInitial                  , &
! Parametres
                    Noyau                     , &
                    PresenceZoneStockage      , &
                    DebProgressifLM           , &
                    DZarriveeFront            , &
                    PhaseSimulation           , &
                    VarCalc                   , &
                    LoiFrottement             , &
                    RepriseCalcul             , &
                    FichierRepriseLecture     , &
! Erreur
                    Erreur                      &
                                            )

   ! .....................................................................
   !  FONCTION :
   !  --------
   !
   !             CONSTRUCTION DES VARIABLES HYDRAULIQUES
   !  choisies par l'utilisateur a partir des variables de sortie HYDRAU
   !  et des variables planimetrees fournies par la fonctionnalite PLANIM
   !
   !----------------------------------------------------------------------------
   !                             ARGUMENTS
   ! .___________________.____._________________________________________________
   ! !    NOM            !TYPE!MODE!                   ROLE
   ! !___________________!____!____!____________________________________________
   ! ! TAUF              ! R  !<-->! Contrainte au fond
   ! ! Y                 ! R  !<-->! Hauteur d'eau maximale
   ! ! HMOY              ! R  !<-->! Hauteur d'eau moyenne
   ! ! Q2G               ! R  !<-->! Debit majeur gauche
   ! ! Q2D               ! R  !<-->! Debit majeur droit
   ! ! VOL               ! R  !<-->! Volume du lit actif
   ! ! VOLS              ! R  !<-->! Volume de stockage
   ! ! CHARG             ! R  !<-->! Charge
   ! ! SS                ! R  !<-->! Section mouillee des zones de stockage
   ! ! V1                ! R  !<-->! Vitesse mineure
   ! ! V2                ! R  !<-->! Vitesse majeure
   ! ! ZMAX              ! R  !<-->! Cote maximale atteinte
   ! ! TZMAX             ! R  !<-->! Instant de cote maximale atteinte
   ! ! ZMIN              ! R  !<-->! Cote minimale atteinte
   ! ! TZMIN             ! R  !<-->! Instant de cote minimale atteinte
   ! ! V1MIN             ! R  !<-->! Vitesse mineure minimale
   ! ! V1MAX             ! R  !<-->! Vitesse majeure minimale
   ! ! BMAX              ! R  !<-->! Largeur au miroir maximale
   ! ! VZMAX             ! R  !<-->! Vitesse a la cote maximale
   ! ! TOND              ! R  !<-->! Instant d'arrivee d'onde
   ! ! QMAX              ! R  !<-->! Debit maximal
   ! ! TQMAX             ! R  !<-->! Instant de debit maximal
   ! ! EMAX              ! R  !<-->! Energie maximale
   ! ! X                 ! R  ! -->! Abscisse des sections
   ! ! ZREF              ! R  ! -->! Cote du fond
   ! ! CF1               ! R  ! -->! Coefficient de frottement mineur
   ! ! Z                 ! R  ! -->! Cote d'eau a une section de calcul
   ! ! Q1                ! R  ! -->! Debit mineur
   ! ! Q2                ! R  ! -->! Debit majeur
   ! ! S1                ! R  ! -->! Section mouillee mineure
   ! ! S2                ! R  ! -->! Section mouillee majeure
   ! ! B1                ! R  !<-->! Largeur au miroir mineure
   ! ! B2                ! R  !<-->! Largeur au miroir majeure
   ! ! BS                ! R  !<-->! Largeur au miroir des zones de stockage
   ! ! P1                ! R  !<-->! Perimetre mouille mineur
   ! ! P2                ! R  !<-->! Perimetre mouille majeur
   ! ! RH1               ! R  !<-->! Rayon hydraulique mineur
   ! ! RH2               ! R  !<-->! Rayon hydraulique majeur
   ! ! BETA              ! R  ! -->! Coefficient beta du modele DEBORD
   ! ! IDT               ! I  ! -->! Indices  des sections de donnees
   ! ! XDT               ! R  ! -->! Position des sections de donnees
   ! ! PROF              ! T  ! -->! Structure des profils de donnees
   ! ! PROFIL_PLAN       ! T  ! -->! Structure des variables des profils
   ! ! Noyau             ! I  ! -->! Noyau de calcul
   ! ! TEMPS             ! R  ! -->! Temps
   ! ! Iteration         ! I  ! -->! Iteration
   ! ! TempsInitial      ! R  ! -->! Temps de debut de la simulation
   ! ! ZInitial          ! R  !<-->! Valeurs initiales des cotes
   ! ! PresenceZoneStockage! L! -->! Existence ou non d'une zone de stockage
   ! ! DebProgressifLM   ! L  ! -->! Presence d'un debordement progress du lit majeur
   ! ! PhaseSimulation   ! I  ! -->! Phase de la simulation
   ! ! ModeleCalcul      ! I  ! -->! Modele de calcul utilise
   ! ! VarCalc           ! L  ! -->! Liste des variables a calculer
   ! ! LoiFrottement     ! L  ! -->! Loi de frottement
   ! ! Erreur            ! T  !<-->! Erreur
   ! !___________________!____!____!____________________________________________
   !
   !  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
   !               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
   !----------------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :        Neant
   !   ----------------------
   !
   !   SOUS-PROGRAMME(S) APPELANT(S) :
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S)   :
   !   ---------------------------     - TAUFOND : Calcul de la contrainte au fond
   !                                   - VOLUME : calcul du volume entre deux sections
   !                                   - RHSBP_GENERIQUE : interpolation sur les profils
   !
   !   COMMENTAIRES : L'ordre de calcul des variables est a respecter
   !   ------------
   !
   !
   !***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PROFIL_T             ! Definition du type PROFIL
   use M_PROFIL_PLAN_T        ! Definition du type PROFIL_PLAN_T
   use M_RHSBP_S              ! Sous-programme de module RHSBP_GENERIQUE
   use M_CONSTANTES_CALCUL_C  ! Constantes pour les phases et modeles de calcul
   use M_INDEX_VARIABLE_C     ! Constantes servant a reperer les variables
   use M_PARAMETRE_C          ! EPS_ONDE, W23, GPES
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_MESSAGE_C            ! liste des messages d'erreurs
   use M_TRAITER_ERREUR_I     ! Traitement des erreurs
   use M_FICHIER_T            ! definition du type fichier 

   !.. Declaration Explicite .. 
   !---------------------------
   implicit none

   !.. Arguments .. 
   !---------------
   ! Resultats
   type(FICHIER_T)              , intent(in) :: FichierRepriseLecture
   real(DOUBLE)   , dimension(:), pointer :: TAUF
   real(DOUBLE)   , dimension(:), pointer :: Y
   real(DOUBLE)   , dimension(:), pointer :: HMOY
   real(DOUBLE)   , dimension(:), pointer :: Q2G
   real(DOUBLE)   , dimension(:), pointer :: Q2D
   real(DOUBLE)   , dimension(:), pointer :: VOL
   real(DOUBLE)   , dimension(:), pointer :: VOLS
   real(DOUBLE)   , dimension(:), pointer :: CHARG
   real(DOUBLE)   , dimension(:), pointer :: SS
   real(DOUBLE)   , dimension(:), pointer :: V1
   real(DOUBLE)   , dimension(:), pointer :: V2
   real(DOUBLE)   , dimension(:), pointer :: ZMAX
   real(DOUBLE)   , dimension(:), pointer :: TZMAX
   real(DOUBLE)   , dimension(:), pointer :: ZMIN
   real(DOUBLE)   , dimension(:), pointer :: TZMIN
   real(DOUBLE)   , dimension(:), pointer :: V1MIN
   real(DOUBLE)   , dimension(:), pointer :: V1MAX
   real(DOUBLE)   , dimension(:), pointer :: BMAX
   real(DOUBLE)   , dimension(:), pointer :: VZMAX
   real(DOUBLE)   , dimension(:), pointer :: TOND
   real(DOUBLE)   , dimension(:), pointer :: QMAX
   real(DOUBLE)   , dimension(:), pointer :: TQMAX
   real(DOUBLE)   , dimension(:), pointer :: EMAX
   ! Donnees
   real(DOUBLE)   , dimension(:), pointer :: Z
   real(DOUBLE)   , dimension(:), pointer :: Q1
   real(DOUBLE)   , dimension(:), pointer :: Q2
   real(DOUBLE)   , dimension(:), pointer :: S1
   real(DOUBLE)   , dimension(:), pointer :: S2
   real(DOUBLE)   , dimension(:), pointer :: B1
   real(DOUBLE)   , dimension(:), pointer :: B2
   real(DOUBLE)   , dimension(:), pointer :: BS
   real(DOUBLE)   , dimension(:), pointer :: P1
   real(DOUBLE)   , dimension(:), pointer :: P2
   real(DOUBLE)   , dimension(:), pointer :: RH1
   real(DOUBLE)   , dimension(:), pointer :: RH2
   real(DOUBLE)   , dimension(:), pointer :: BETA
   type (PROFIL_T), dimension(:), intent(in   ) :: PROF
   type (PROFIL_PLAN_T)         , intent(in   ) :: PROFIL_PLAN
   real(DOUBLE)                 , intent(in   ) :: Temps
   real(DOUBLE)                 , intent(in   ) :: TempsInitial
   integer                      , intent(in   ) :: Iteration
   ! Modele
   real(DOUBLE)   , dimension(:), pointer       :: X
   real(DOUBLE)   , dimension(:), pointer       :: ZREF
   real(DOUBLE)   , dimension(:), pointer       :: CF1
   integer        , dimension(:), intent(in   ) :: IDT
   real(DOUBLE)   , dimension(:), intent(in   ) :: XDT
   ! Etat
   real(DOUBLE)   , dimension(:), pointer       :: ZInitial
   ! Parametres
   integer                      , intent(in   ) :: Noyau
   logical                      , intent(in   ) :: PresenceZoneStockage
   logical                      , intent(in   ) :: DebProgressifLM
   real (DOUBLE)                , intent(in   ) :: DZArriveeFront
   integer                      , intent(in   ) :: PhaseSimulation
   logical        , dimension(:), intent(in   ) :: VarCalc
   integer                      , intent(in   ) :: LoiFrottement
   logical                      , intent(in   ) :: RepriseCalcul
   type(ERREUR_T)               , intent(inout) :: Erreur

   end subroutine POST

   end interface

end module M_POST_I
