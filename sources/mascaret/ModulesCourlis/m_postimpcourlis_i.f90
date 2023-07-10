Module M_PostImpCourlis_I

Interface

Subroutine  PostImpCourlis  ( &

  FicListingCourlis       , & ! Unite logique fichier listing
  PhasePostImpCourlis     , & !
  Temps                   , & ! Temps courant
  num_pas                 , & ! Numero du pas de temps
  DT                      , & ! Pas de temps
  NbProfil                , & ! Nombre de profils
  NbCouche                , & ! Nombre de couches
  Absc                    , & ! Abscisse des profils (=ProfilCourlis%Absc)
  Zref                    , & ! Point bas de l interface eau-sediment (=ProfilCourlis%Zref(1))
  Zsurf                   , & ! Cote de la surface libre
  Vitesse                 , & ! Vitesse moyenne par section
  Sm                      , & ! Surface mouillee
  CVase                   , & ! Concentration des vases en suspension
  CSable                  , & ! Concentration des sables en suspension
  DepotCumulCouche        , & ! depot cumule /profil et /couche (> 0 depot, < 0 erosion)
  DeltaSurfaceSed         , & ! Variation de la surface sedimentaire
  QVase                   , & ! Flux de depot des vases (> 0 depot, < 0 erosion)
  QSable                  , & ! Flux de depot des sables (> 0 depot, < 0 erosion)
  TauHMax                 , & ! Contrainte hydr. loc. max. ds section (depend du tirant d'eau local)
  TauHMoy                 , & ! Contrainte hydr. loc. moy. ds section (depend du tirant d'eau local)
  TauEMoy                 , & ! Contrainte hydr. eff. moy. ds section (depend du rayon hydr.)
  CeqMoy                  , & ! Conc. d'equilibre des sables moy. ds section
  FluxVase                , & ! Bilan sur les flux de vases
  FluxSable               , & ! Bilan sur les flux de sables
  MasseVase               , & ! Bilan sur les masses de vases
  MasseSable              , & ! Bilan sur les masses de sables
  VolSedDepot             , & ! Volume de sedimt depose depuis debut du calcul
  Erreur                  )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       05/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Ecriture du fichier listing de COURLIS
!  --------
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!=========================================================================

use M_PRECISION                 ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C       ! Constantes num, phys et info
use M_PARAMETRE_C               ! Definition des constante tq EPS*, W0, ...

use M_FICHIER_T                 ! Definition du type FICHIER_T
use M_BILAN_FLUX_T              ! Definition du type BILAN_FLUX_T
use M_BILAN_MASSE_T             ! Definition du type BILAN_MASS_T

use M_ERREUR_T                  ! Type ERREUR_T
use M_MESSAGE_C                 ! Messages d'erreur
use M_TRAITER_ERREUR_I          ! Traitement de l'erreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none


! Variables d'entree
!-------------------
  type(FICHIER_T)           , intent(in) :: FicListingCourlis
  integer                   , intent(in) :: PhasePostImpCourlis
  real(DOUBLE)              , intent(in) :: Temps
  real(DOUBLE)              , intent(in) :: Dt
  integer                   , intent(in) :: num_pas
  integer                   , intent(in) :: NbProfil
  integer                   , intent(in) :: NbCouche
  real(DOUBLE), dimension(:), intent(in) :: Absc
  real(DOUBLE), dimension(:), intent(in) :: Zref

  real(DOUBLE), dimension(:)  , pointer :: Zsurf
  real(DOUBLE), dimension(:)  , pointer :: Vitesse
  real(DOUBLE), dimension(:)  , pointer :: Sm
  real(DOUBLE), dimension(:)  , pointer :: CVase
  real(DOUBLE), dimension(:)  , pointer :: CSable
  real(DOUBLE), dimension(:,:), pointer :: DepotCumulCouche
  real(DOUBLE), dimension(:)  , pointer :: DeltaSurfaceSed
  real(DOUBLE), dimension(:)  , pointer :: QVase
  real(DOUBLE), dimension(:)  , pointer :: QSable
  real(DOUBLE), dimension(:)  , pointer :: TauHMax
  real(DOUBLE), dimension(:)  , pointer :: TauHMoy
  real(DOUBLE), dimension(:)  , pointer :: TauEMoy
  real(DOUBLE), dimension(:)  , pointer :: CeqMoy

  real(DOUBLE)             , intent(in) :: VolSedDepot

  type(BILAN_MASSE_T)      , intent(in) :: MasseVase
  type(BILAN_MASSE_T)      , intent(in) :: MasseSable

  type(BILAN_FLUX_T)       , intent(in) :: FluxVase
  type(BILAN_FLUX_T)       , intent(in) :: FluxSable


! Variables locales
!------------------
  integer      :: UniteList, i, k
  real(DOUBLE) :: MasseEauTotal
!  real(DOUBLE) :: MasseEntreeTotal  ! PU2017 : Mise en commentaire
!  real(DOUBLE) :: MasseSortieTotal  ! PU2017 : Mise en commentaire
!  real(DOUBLE) :: MasseApportTotal  ! PU2017 : Mise en commentaire
  real(DOUBLE) :: MasseErreurVase, MasseErreurSable
  real(DOUBLE) :: MasseDepVaseTotal, MasseDepSableTotal
!  real(DOUBLE) :: MasseTotal  ! PU2017 : Mise en commentaire
  real(DOUBLE) :: MasseErrRelVase, MasseErrRelSable

  real(DOUBLE), dimension(:), allocatable :: MasseDepotTotal
  real(DOUBLE), dimension(:), allocatable :: DepotCumul ! depot cumule par profil (> 0 depot, < 0 erosion)

!  character(72) :: Modele    ! Chaine de caractere temporaire  ! PU2017 : Mise en commentaire

! Traitement des erreurs
  integer        :: retour
!  character(132) :: arbredappel_old  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================

End Subroutine PostImpCourlis

End Interface

End Module M_PostImpCourlis_I
