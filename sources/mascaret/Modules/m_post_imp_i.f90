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

module M_POST_IMP_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine POST_IMP( &
! Donnees
      X, ZREF         , & ! Maillage et cotes de reference
      RGC, RDC        , & ! Rives gauche et droite
      CF1, CF2        , & ! Coeff de frottement mineur et majeur
   Z, QT, Q1, Q2       , & ! Cote debits mineur et majeur
      DebitFlux       , & ! Flux de masse
      S1, S2          , & ! Sections mineur et majeur
      B1, B2, BS      , & ! Largeurs au miroir mineur, majeur et de stockage
      P1, P2          , & ! Perimetres mouillees mineur et majeur
      RH1, RH2        , & ! Rayons hydrauliques mineur et majeur
      FR, BETA        , & ! Froude et BETA
      TAUF            , & ! Contrainte au fond
      Y, HMOY         , & ! Hauteur d'eau et hauteur d'eau moyenne
      Q2G, Q2D        , & ! Debits majeur droit et gauche
      VOL, VOLS       , & ! Volumes lit actif et zone de stockage
      CHARG, SS       , & ! Charge et Section de stockage
      V1, V2          , & ! Vitesse mineur et majeur
      ZMAX, TZMAX     , & ! Cote max et temps associe
      VZMAX           , & ! Vitesse a la cote max
      ZMIN, TZMIN     , & ! Cote min et temps associe
      V1MIN, V1MAX    , & ! Vitesse mineur min et max
      BMAX            , & ! Largeur au miroir max
      TOND            , & ! Temps d'arrivee de l'onde
      QMAX, TQMAX     , & ! Debit max et temps associe
      EMAX            , & ! Energie maximale
      YVRAI , QVRAI   , & ! solutions analytiques
      Qdeverse        , & ! Debit deverse
      Temps           , & ! Temps courant
      Apport          , & ! Debits d'Apports
      PasTempsOptimal , & ! Pas de temps optimal
! Modele
      Connect         , & ! Connectivite du reseau
      ModeleLit       , & ! Modele du lit (Debord/Crugos)
! Parametres
      Iteration       , & ! Numero du pas de temps
      Noyau           , & ! Noyau de calcul utilise
      PhaseSimulation , & ! Phase Initialisation/Calcul
     ImpressionCalcul , & ! ImpressionCalcul
      Regime          , & ! Regime Permanent / Non Permanent
      VarImp          , & ! Variables a imprimer
      UniteListing    , & ! Unite logique ficheier listing
! Etats
  TempsPrecedent      , & ! Temps precedent
  VolBiefActif, VolBiefStockage, & ! Volumes actifs et de stockage
  QAmontPrec, QAvalPrec        , & ! Debits amont et aval des biefs
  Erreur                         & ! Erreur
                                 )

   ! .....................................................................
   !  FONCTION :   IMPRESSION DES VALEURS DES VARIABLES SUR LISTING
   !-----------------------------------------------------------------------------
   !
   !  FICHIERS ENTREE/SORTIE : Listing (unite logique : UniteListing)                  
   !  ----------------------
   !
   !
   !  SOUS-PROGRAMME(S) APPELANT(S) : - SUPERVISEUR
   !  -----------------------------
   !  SOUS-PROGRAMME(S) APPELE(S)   : - CONVERSION_TEMPS  (interne)
   !  ---------------------------     - INIT_VAR_SORTIE_S (module)
   !
   !  COMMENTAIRES :
   !  ------------
   !
   !  DOCUMENTATION EXTERNE :
   !  ---------------------
   !
   !***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C ! Constantes
   use M_INDEX_VARIABLE_C    ! Constantes servant a reperer les variables
   use M_PARAMETRE_C         ! GPES, TETA
   use M_MESSAGE_C           ! Messages d'erreur
   use M_APPORT_T            ! Definition du type APPORT_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_INIT_VAR_SORTIE_S   ! Initialisation des structures des variables a sortir
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   !.. Declarations Explicites .. 
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! Donnees
   real(DOUBLE)   , dimension(:), pointer   :: X
   real(DOUBLE)   , dimension(:), pointer   :: ZREF
   real(DOUBLE)   , dimension(:), pointer   :: RGC
   real(DOUBLE)   , dimension(:), pointer   :: RDC
   real(DOUBLE)   , dimension(:), pointer   :: CF1
   real(DOUBLE)   , dimension(:), pointer   :: CF2
   real(DOUBLE)   , dimension(:), pointer   :: Z
   real(DOUBLE)   , dimension(:), pointer   :: QT
   real(DOUBLE)   , dimension(:), pointer   :: Q1
   real(DOUBLE)   , dimension(:), pointer   :: Q2
   real(DOUBLE)   , dimension(:), pointer   :: DebitFlux
   real(DOUBLE)   , dimension(:), pointer   :: S1
   real(DOUBLE)   , dimension(:), pointer   :: S2
   real(DOUBLE)   , dimension(:), pointer   :: B1
   real(DOUBLE)   , dimension(:), pointer   :: B2
   real(DOUBLE)   , dimension(:), pointer   :: BS
   real(DOUBLE)   , dimension(:), pointer   :: P1
   real(DOUBLE)   , dimension(:), pointer   :: P2
   real(DOUBLE)   , dimension(:), pointer   :: RH1
   real(DOUBLE)   , dimension(:), pointer   :: RH2
   real(DOUBLE)   , dimension(:), pointer   :: FR
   real(DOUBLE)   , dimension(:), pointer   :: BETA
   real(DOUBLE)   , dimension(:), pointer   :: TAUF
   real(DOUBLE)   , dimension(:), pointer   :: Y
   real(DOUBLE)   , dimension(:), pointer   :: HMOY
   real(DOUBLE)   , dimension(:), pointer   :: Q2G
   real(DOUBLE)   , dimension(:), pointer   :: Q2D
   real(DOUBLE)   , dimension(:), pointer   :: VOL
   real(DOUBLE)   , dimension(:), pointer   :: VOLS
   real(DOUBLE)   , dimension(:), pointer   :: CHARG
   real(DOUBLE)   , dimension(:), pointer   :: SS
   real(DOUBLE)   , dimension(:), pointer   :: V1
   real(DOUBLE)   , dimension(:), pointer   :: V2
   real(DOUBLE)   , dimension(:), pointer   :: ZMAX
   real(DOUBLE)   , dimension(:), pointer   :: TZMAX
   real(DOUBLE)   , dimension(:), pointer   :: VZMAX
   real(DOUBLE)   , dimension(:), pointer   :: ZMIN
   real(DOUBLE)   , dimension(:), pointer   :: TZMIN
   real(DOUBLE)   , dimension(:), pointer   :: V1MIN
   real(DOUBLE)   , dimension(:), pointer   :: V1MAX
   real(DOUBLE)   , dimension(:), pointer   :: BMAX
   real(DOUBLE)   , dimension(:), pointer   :: TOND
   real(DOUBLE)   , dimension(:), pointer   :: QMAX
   real(DOUBLE)   , dimension(:), pointer   :: TQMAX
   real(DOUBLE)   , dimension(:), pointer  :: EMAX
   real(DOUBLE)   , dimension(:) , pointer  :: YVRAI
   real(DOUBLE)   , dimension(:) , pointer  :: QVRAI
   real(DOUBLE)   , dimension(:) , pointer  :: Qdeverse
   real(DOUBLE)                 , intent(in   ) :: Temps
   type(APPORT_T) , dimension(:), intent(in   ) :: Apport
   real(DOUBLE)                 , intent(in   ) :: PasTempsOptimal
   Logical                      , intent(in   ) :: ImpressionCalcul
! Modele
   type(CONNECT_T)              , intent(in   ) :: Connect
   integer                      , intent(in   ) :: ModeleLit
! Parametres
   integer                      , intent(in   ) :: Iteration
   integer                      , intent(in   ) :: Noyau
   integer                      , intent(in   ) :: PhaseSimulation
   integer                      , intent(in   ) :: Regime
   logical        , dimension(:), intent(in   ) :: VarImp
   integer                      , intent(in   ) :: UniteListing
! Etats
   real(DOUBLE)                         , intent(inout) :: TempsPrecedent
   real(DOUBLE)   , dimension(:)        , pointer       :: VolBiefActif
   real(DOUBLE)   , dimension(:)        , pointer       :: VolBiefStockage
   real(DOUBLE)   , dimension(:)        , pointer       :: QAmontPrec
   real(DOUBLE)   , dimension(:)        , pointer       :: QAvalPrec
   type(ERREUR_T)                       , intent(inout) :: Erreur

   end subroutine POST_IMP

   end interface

end module M_POST_IMP_I
