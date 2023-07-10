Module M_LecParamSedim_I

Interface

Subroutine  LecParamSedim ( &

    UniteListing      , & ! Unite logique fichier listing
    FichierSedim      , & ! Fichier contenant les donnees sedim. rel. a ch. couche + donnees concernant la stabilite des berges
    ImpressionSedim   , &
    NbCouches         , & ! Nb de couches sedimentaires
    CoucheSed         , & ! variable contenant ttes les donnees rel. a ch. couche
    Talus             , & ! variable contenant ttes les donnees rel. aux talus
    LimiteSable       , & ! % de sable a part. dql la couche est traitee suivant les lois du sable
    CnuxV             , & ! Coefficient de diffusion vases
    CnuxS             , & ! Coefficient de diffusion sables
    ConsConv          , & ! parametres schema de convection
!    FracH             , & ! MS2018: nouvelle variable, clipping evolution pourcentage de la hauteur d eau
    MOTINT            , &
    MOTREA            , &
    MOTCAR            , &
    ADRESS            , &
    MOTLOG            , &
    Erreur            )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL M. Jodeau
!
!  VERSION : 5.1       08-2009  Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture des caracteristiques des sediments en place
!  --------
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele : - LecFichierSedim
!  ---------------------
!
!=========================================================================


!=========================================================================
! DECLARATIONS
!=========================================================================

use M_MY_GLOBAL_VAR_SED
use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
use M_FICHIER_T           ! Definition du type FICHIER_T

use M_COUCHE_T            ! Definition du type COUCHE_T
use M_TALUS_T             ! Definition du type TALUS_T
use M_CONSTANTES_TRACER_T ! parametres schema de convection
use M_LecFichierSedim_I   ! Interface de sous-programme

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_MESSAGE_TRACER_C    ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'erreur

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  integer , intent(in   ) :: UniteListing
  logical , intent(in   ) :: ImpressionSedim

  integer           , dimension(:)  , intent(in   ) :: MOTINT
  logical           , dimension(:)  , intent(in   ) :: MOTLOG
  real(DOUBLE)      , dimension(:)  , intent(in   ) :: MOTREA
  character(LEN=144), dimension(:)  , intent(in   ) :: MOTCAR
  integer           , dimension(:,:), intent(in   ) :: ADRESS

  type(FICHIER_T)     , intent(inout) :: FichierSedim

! Variables de sortie
  integer                     , intent(  out) :: NbCouches
  type(TALUS_T)               , intent(  out) :: Talus
  real(DOUBLE)                , intent(  out) :: LimiteSable
  real(DOUBLE)                , intent(  out) :: CnuxV
  real(DOUBLE)                , intent(  out) :: CnuxS
  type(COUCHE_T), dimension(:), pointer       :: CoucheSed
  type(CONSTANTES_TRACER_T)   , intent(  out) :: ConsConv

! MS2018: Nouvelles variables
!  real(DOUBLE)               , intent(  out) :: FracH

! Variables locales
  integer      :: ModeParamSedim ! Mode d'entree des parametres sedimentaires
                                 ! (par fichier ou par l'interface)
  integer      :: iCouche        ! Compteur de couche
  real(DOUBLE) :: ValSum         ! Sum of values for making tests
  integer      :: ib             ! PU2017 : Mise en commentaire de nbtrac


! Traitement des erreurs
  integer   :: retour   ! Code de retour de la fonction read
!  character(132) :: arbredappel_old ! Arbre d'appel initial  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

! VARIABLES POUR DAMOCLE
! ---------------------
!  integer, parameter :: NMAX = 10000               ! PU2017 : Mise en commentaire
!  integer, parameter :: LANGUE_FRANCAISE = 1       ! PU2017 : Mise en commentaire
!  integer, parameter :: langue = LANGUE_FRANCAISE  ! PU2017 : Mise en commentaire
!  logical, parameter :: impression_doc = .false.   ! PU2017 : Mise en commentaire

!  type(FICHIER_T)    :: fichier_listing_damoc = FICHIER_T(9,"listing_tracer.damoc")

!  character(LEN=72) , dimension(4,NMAX,2) :: MOTCLE  ! PU2017 : Mise en commentaire
!  character(LEN=144), dimension(NMAX)     :: MOTCAR
!  logical           , dimension(NMAX)     :: MOTLOG
!  integer           , dimension(NMAX)     :: MOTINT
!  integer           , dimension(4,NMAX)   :: ADRESS
!  integer           , dimension(4,NMAX)   :: DIMENS  ! PU2017 : Mise en commentaire
!  integer           , dimension(4,NMAX)   :: TROUVE  ! PU2017 : Mise en commentaire
!  real(DOUBLE)      , dimension(NMAX)     :: MOTREA

!.. External Calls ..

!MS2018 ==> ajout d'une variable warning pour les diametres
  integer    :: warning

  external DAMOC

!=========================================================================

End Subroutine LecParamSedim

End Interface

End Module M_LecParamSedim_I

