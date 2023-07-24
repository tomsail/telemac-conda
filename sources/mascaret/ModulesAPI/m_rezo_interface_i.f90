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

module M_REZO_INTERFACE_I
!***********************************************************************
! PROGICIEL : MASCARET      A. LEBOSSE
!                           S. PERON
!                           E. BEN SLAMA
!                           S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  REZO_INTERFACE    ( &
! Donnees / Resultats
    Z               , & ! Cote de la surface libre
    Q1, Q2          , & ! Debits mineur et majeur
    P1, P2          , & ! Perimetres mouilles mineur et majeur
    B1, B2, BS      , & ! Largeurs au miroir mineur, majeur et de stockage
    RH1, RH2        , & ! Rayons hydrauliques mineur et majeur
    S1, S2          , & ! Sections mouillee mineur et majeur
    DTLEVY          , & ! Pas de temps optimal
    Beta            , & ! Coefficient Beta de repartition des lits
    Froude          , & ! Nombre de Froude
    Extremite       , & ! Extremites libres
    Apport          , & ! Debits d'apport
    Qinjec          , & ! Debit injecte
    Qdeverse        , & ! debit total deverse par un deversoir lateral ponctuel ou lineique
    Temps           , & ! Temps
    PhaseSimulation , & ! Phase de la simulation
! Profils
    Profil          , & ! Profils geometriques
    ProfilPlan      , & ! Profils planimetrees
! Modele
    X                , & ! Maillage
    CF1, CF2         , & ! Coefficients de frottement mineur et majeur
    ZREF             , & ! Cote de reference
    XDT              , & ! Position section/profil amont
    IDT              , & ! Numero du profil amont d'une section
    Connect          , & ! Table de connectivite
    Singularite      , & ! Singularites
    PCSing           , & ! Pertes de charges singulieres
    Deversoir        , & ! Deversoirs
    ModeleLit        , & ! Modele du lit
    Confluent        , & ! Caracteristiques des confluences
    Abaque           , & ! Abaques des pertes de  charges aux confluences
! Parametres
    DTImpression     , & ! Pas de temps d'impression
    Impression       , & ! Flag d'autorisation d'impression
    UniteListing     , & ! Unite logique du fichier listing
    LoiFrottement    , & ! Loi de frottement
    PerteChargeConfluent,& ! Perte de charge automatique aux confluents
! Etat
    TempsPrecedent   , & ! Temps precedent
    Tinitial         , & ! Temps de debut de simulation
    NumeroPas        , & ! Numero du pas de temps
    DPDZ1, DPDZ2     , & ! Derivee de P1 et de P2 / Z
    OptionCasier     , & ! Flag de presence de casiers
    Liaison          , & ! Caracteristiques des liaisons RIVIERE-CASIER et CASIER-CASIER
    Casier           , & ! Caracteristiques des casiers
    ApportPluie      , & ! Apport de pluie des casiers
    DT               , & ! DT REZO
    Matrice          , &
    NoConvection     , & ! Attenuation de la convection
    CQMV             , & ! qmv debits d'apport
    Erreur             & ! Erreur
                       )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE      S. PERON
!                             E. BEN SLAMA    S. MANDELKERN
!                             F. ZAOUI
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
! FONCTION :
!
! TRAITEMENT D'UN PAS DE TEMPS A L'AIDE DU CODE REZO
!
! .....................................................................
! DANS LES COMMENTAIRES :
! DQ DESIGNE LA VARIATION DE DEBIT D'UN PAS DE TEMPS AU SUIVANT
! DANS UNE SECTION DE CALCUL,
! ET DE MEME DZ DESIGNE LA VARIATION DE COTE
! COPIE DE LA SUBROUTINE REZO AVEC SUPPRESSION DES VARIABLES REMANENTES
! DT ET MATRICE EN ARGUMENT INOUT
! .....................................................................
!
! .....................................................................
! . FICHIERS EN SORTIE                                                .
! .....................................................................
! . NUMERO       . SUPPORT PHYSIQUE . ROLE
! .....................................................................
! . UniteListing . FICHIER LISTING  . IMPRESSION DES PRINCIPAUX RESULTATS
! .....................................................................
! .....................................................................
! . PROGRAMME APPELANT                : CALCUL_MASCARET
! .....................................................................

   !============================= Declarations ===========================
   use M_PRECISION           ! Type DOUBLE
   use M_PARAMETRE_C         ! Parametres de calcul
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes parametres du calcul
   use M_APPORT_T            ! Definition du type APPORT_T
   use M_CONFLUENT_T         ! Type confluent
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_DEVERSOIR_T         ! Definition du type DEVERSOIR_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_EXTREMITE_T         ! Definition du type EXTREMITE_T
   use M_PROFIL_T            ! Definition du type PROFIL_T
   use M_PROFIL_PLAN_T       ! Definition du type PROFIL_PLAN_T
   use M_SINGULARITE_T       ! Definition du type SINGULARITE_T
   use M_REZOMAT_T           ! Definition du type REZOMAT_T
   use M_INTERPOLATION_S     ! Interpolation
   use M_RHSBP_S             ! Calcul de RH, S, B et P
   use M_NUM_BIEF_S          ! Calcul du numero du bief d'une section
   use M_FROUDE_S            ! Calcul et controle du nombre de Froude
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_CALCUL_I            ! Calcul
   use M_CHAINAGE_REZO_I     ! Chainage de la matrice
   use M_SINGULARITE_REZO_I  ! Lien direct section -> no. singularite
   use M_CONTQ_I             ! Controle de la conservation du debit aux noeuds
   use M_CCL_I               ! Transformation pour calcul des conditions aux limites
   use M_CQINJ_I             ! Transformation pour calcul des de bits d'apport
   use M_KSING_I             ! Traitement des singularites
   use M_CALC_PC_CONFLU_I    ! Calcul des pertes de charge aux conf
   use M_DPDZ_I              ! Calcul du derive du perimetre mouille / cote
   use M_REPAR_I             ! Calcul de la repartition lit mineur / majeur
   use M_CONSTANTES_CASIER_C ! Module pour l'implicitation des casiers
   use M_CASIER_T            ! Les casiers
   use M_LIAISON_T
   use M_APPORT_PLUIE_T
   use M_KLIAISON_I
   use M_APIMASCARET_I       ! API

   !.. Implicit Declarations
   implicit none

   !.. Arguments ..
   real(DOUBLE), dimension(:), intent(inout) :: Z, Q1, Q2
   real(DOUBLE), dimension(:), intent(inout) :: P1, P2
   real(DOUBLE), dimension(:), intent(inout) :: B1, B2, BS
   real(DOUBLE), dimension(:), intent(inout) :: RH1, RH2
   real(DOUBLE), dimension(:), intent(inout) :: S1, S2
   real(DOUBLE)              , intent(  out) :: DTLEVY
   real(DOUBLE), dimension(:), intent(  out) :: Beta
   real(DOUBLE), dimension(:), intent(  out) :: Froude
   ! Maillage
   real(DOUBLE), dimension(:), intent(in   ) :: X, CF1, CF2
   real(DOUBLE), dimension(:), intent(in   ) :: ZREF
   real(DOUBLE), dimension(:), intent(in   ) :: XDT
   integer     , dimension(:), intent(in   ) :: IDT
   ! Conditions aux limites
   type(EXTREMITE_T)   , dimension(:), intent(inout) :: Extremite
   ! Variables planimetrees
   type (PROFIL_T)     , dimension(:), intent(in   ) :: Profil
   type (PROFIL_PLAN_T)              , intent(in   ) :: ProfilPlan
   ! Debits d apports
   type(APPORT_T)      , dimension(:), intent(in   ) :: Apport
   ! Temps
   real(DOUBLE)                      , intent(in   ) :: Temps
   integer                           , intent(in   ) :: PhaseSimulation
   real(DOUBLE)                      , intent(in   ) :: DTImpression
   ! Pertes de charge singulieres
   real(DOUBLE)        , dimension(:), intent(inout) :: PCSing
   ! Confluences
   type(CONFLUENT_T)   , dimension(:) , intent(in   ) :: Confluent
   real(DOUBLE)    , dimension(:,:,:) , intent(in   ) :: Abaque
   ! Table de connectivite du reseau
   type(CONNECT_T)                   , intent(in   ) :: Connect
   ! Singularites
   type (SINGULARITE_T), dimension(:), pointer, intent(inout) :: Singularite
   ! Parametres
   integer                           , intent(in   ) :: ModeleLit
   logical                           , intent(in   ) :: Impression
   logical                           , intent(in   ) :: NoConvection
   integer                           , intent(in   ) :: UniteListing
   integer                           , intent(in   ) :: LoiFrottement
   logical                           , intent(in   ) :: PerteChargeConfluent
   ! Deversoirs
   type (DEVERSOIR_T)  , dimension(:), intent(in   ) :: Deversoir
   real(DOUBLE)        , dimension(:), intent(inout) :: Qdeverse
   ! Etats
   real(DOUBLE)                      , intent(inout) :: Tinitial
   real(DOUBLE)                      , intent(inout) :: TempsPrecedent
   integer                           , intent(inout) :: NumeroPas
   real(DOUBLE)        , dimension(:), pointer       :: DPDZ1, DPDZ2
   ! Casier
   logical                           , intent(in   ) :: OptionCasier
   type(CASIER_T)       , dimension(:)  , pointer, intent(inout) :: Casier
   type(LIAISON_T)      , dimension(:)  , pointer, intent(inout) :: Liaison
   type(APPORT_PLUIE_T ), dimension(:)  , pointer, intent(inout   ) :: ApportPluie
   ! Erreur
   type(ERREUR_T)                    , intent(inout) :: Erreur
   real(DOUBLE)                      , intent(inout) :: DT      ! Pas de temps REZO
   type(REZOMAT_T)                   , intent(inout) :: Matrice ! Matrice du reseau
   integer                           , intent(in   ) :: CQMV
   ! Tableaux de travail
   ! Debits d'apports generalise (avec deversoirs)
   real(DOUBLE), dimension(:) , intent(inout) :: QINJEC
   real(DOUBLE), dimension(size(X))           :: Q
   ! Coefficients de l'equation discretisee d'une singularites
   real(DOUBLE), dimension(:), allocatable :: ASING, BSING, CSING, DSING
   ! Debit passant sur cette singularite
   real(DOUBLE), dimension(:), allocatable :: QSING
   ! Coefficients de l'equation discretisee d'une cond a la limite
   ! RDLIM*DQ + SDLIM*DZ = TDLIM
   real(DOUBLE), dimension(size(Extremite))   :: RDLIM, SDLIM, TDLIM

   end subroutine REZO_INTERFACE

   end interface

end module M_REZO_INTERFACE_I
