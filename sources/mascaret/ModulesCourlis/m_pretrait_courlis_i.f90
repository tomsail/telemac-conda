Module M_Pretrait_Courlis_I

Interface

Subroutine  PRETRAIT_COURLIS  ( &

    FicListingCourlis           , & ! Unite logique fichier listing
    FichierMotCle               , & ! Fichier des mots-cle
    FichierDico                 , & ! Fichier dictionnaire de Damocles
    Noyau                       , & ! Choix du noyau de calcul de l'hydraulique
    Type_maillage               , & ! Choix du type de maille longitudinal
    CritereArret                , & ! Critere d'arret des calculs
    NBief                       , & ! Nombre de biefs
    PasTempsVariable            , & ! Pas de temps dependant du nombre de courant
    TempsMaximum                , & ! Temps de fin de calcul
    OptionCasier                , & ! Choix d'un calcul avec CASIER
    OptionCourlis               , & ! Choix d'un calcul Courlis
!    CHARRIAGE                   , & ! Choix du mode de transport
    CalcSable                   , & ! Choix d'un calcul avec Sable
    Apport                      , & ! Apports hyrauliques
    Profil                      , & ! Profils geometriques de l'hydraulique
! Lecture des parametres de sediments
    FichierSedim                , & ! Fichier des parametres de sediments
    CoucheSed                   , & ! variable contenant ttes les donnees rel. a ch. couche
    Talus                       , & ! variable contenant ttes les donnees rel. aux talus
    LimiteSable                 , & ! % de sable a part. dql la couche est traitee
                                    ! suivant les lois de sable
    CnuxV                       , & ! Coefficient de diffusion vases
    CnuxS                       , & ! Coefficient de diffusion sables
    ConsConv                    , & ! parametres schema convection
! Lecture de la geometrie des rivieres
    FicGeomCourlis              , & ! Fichier des profils definissant les interfaces
    ProfilCourlis               , & ! Profils de la geometrie des rivieres, lus dans COURLIS
! Lecture des concentrations initiales
    FicCMESIni                  , & ! Fichier des concentrations initiales en sable et vase
    CVaseIni                    , & ! Concentration en vase  initialemt ds bief
    CSableIni                   , & ! Concentration en sable initialemt ds bief
! Lois de concentration
    FicLoiConc                  , & ! Fichier de l'evolution temporelle de conc en vase
    NbLoiConc                   , & ! Nombre de Lois de concentration
    LoiConc                     , & ! Structure de donnnees des lois de concentration
! Lecture des apports Courlis
    ApportVase                  , & ! Var. contenant les donnees sedim. des apports de vase
    ApportSable                 , & ! Var. contenant les donnees sedim. des apports de sable
    CL_Vase                     , & ! CL amont de la concentration en Vase
    CL_Sable                    , & ! CL amont de la concentration en Sable
! Impression des parametres et resultats
    FicStockPLongCourlis        , & ! Fichier graphique des res. selon profil en long du bief
    FicStockPTransCourlis       , & ! Fichier graphique des res. selon profil en travers du b.
    FicControleCourlis          , & ! Ficher de controle
    FicErreurCourlis            , & ! Fichier d'erreur
    FicResuGeom                 , & ! Fichier de la geometrie finale des couches
    FicResuMES                  , & ! Fichier des concentrations en MES a fin calcul a ch. x
    PasImpressionCourlis        , & ! Periodicite des enregistrements listing
    PasStockLongCourlis         , & ! Periodicite des enregistremts graph. profil en long
    PasStockTransCourlis        , & ! Periodicite des enregistremts graph. profil en travers
! Lecture des parametres de couplage
    NbIterHydro                 , & ! Nb d'iter. pour l'hydraulique entre 2 echanges
    NbIterSedim                 , & ! Nb d'iter. pour  la sedimento entre 2 echanges
! Traitement des erreurs
    Erreur                      )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL, M. Jodeau
!
!  VERSION : 5.1       08-2009    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================

!=========================================================================
!  Fonction : Pretraitement de COURLIS : lecture des donnees d'entree
!  --------   (fichier cas) et preparation des donnees pour le calcul
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele : - TRAITER_ERREUR
!  ---------------------   - DAMOCLES
!                          - LecImpResCourlis
!                          - LecParamSedim
!                          - LecGeomCourlis
!                          - LecConcIni
!                          - LecLoiConc
!                          - LecApportCourlis
!                          - LecCouplage
!
!======================================================================
!  DECLARATIONS
!======================================================================

use M_MY_GLOBAL_VAR_SED
use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info

use M_FICHIER_T           ! Definition du type FICHIER_T
use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS_T
use M_PROFIL_T            ! Definition du type PROFIL_T
use M_COUCHE_T            ! Definition du type COUCHE_T
use M_TALUS_T             ! Definition du type TALUS_T
use M_APPORT_T            ! Definition du type APPORT_T
use M_SOURCE_TRACER_T     ! Definition du type TRACEUR_T
use M_CL_COURLIS_T        ! Definition du type CL_COURLIS_T
use M_LOI_CONC_T          ! Definition du type LOI_CONC_T
use M_CONSTANTES_TRACER_T ! parametres schema convection

! Interface de sous-programmes
use M_LecParamSedim_I
use M_LecImpResCourlis_I
use M_LecGeomCourlis_I
use M_LecConcIni_I
use M_LecLoiConc_I
use M_LecCouplage_I
use M_LecApportCourlis_I

! Module de tratement des erreurs
use M_ERREUR_T         ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I ! Traitement de l'erreur

!======================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(FICHIER_T)     , intent(inout) :: FicListingCourlis
  type(FICHIER_T)     , intent(in   ) :: FichierMotCle
  integer             , intent(in   ) :: Noyau
  integer             , intent(in   ) :: Type_maillage
  integer             , intent(in   ) :: CritereArret
  integer             , intent(in   ) :: NBief
  logical             , intent(in   ) :: PasTempsVariable
!  logical             , intent(in   ) :: CHARRIAGE
  real(DOUBLE)        , intent(in   ) :: TempsMaximum
  logical             , intent(in   ) :: OptionCasier
  type(FICHIER_T)     , intent(inout) :: FichierSedim
  type(FICHIER_T)     , intent(inout) :: FicGeomCourlis
  type(FICHIER_T)     , intent(inout) :: FicCMESIni
  type(FICHIER_T)     , intent(inout) :: FicLoiConc

  type(APPORT_T) , dimension(:) , pointer  :: Apport
  type(PROFIL_T) , dimension(:) , pointer  :: Profil

! Variables de sortie
  logical       , intent(  out) :: OptionCourlis
  logical       , intent(  out) :: CalcSable

  ! Sous-programme LecParamSedim
  type(TALUS_T)      , intent(  out) :: Talus
  real(DOUBLE)       , intent(  out) :: LimiteSable
  real(DOUBLE)       , intent(  out) :: CnuxV
  real(DOUBLE)       , intent(  out) :: CnuxS

  type(COUCHE_T)     , dimension(:), pointer      :: CoucheSed
  type(CONSTANTES_TRACER_T)        , intent( out) :: ConsConv

  ! Sous-programme LecGeomCourlis
  type(PROFIL_COURLIS_T), dimension(:), pointer  :: ProfilCourlis

  ! Sous-programme LecConcIni
  real(DOUBLE)   , dimension(:), pointer  :: CVaseIni
  real(DOUBLE)   , dimension(:), pointer  :: CSableIni

  ! Sous-programme LecLoiConc
  integer                         , intent(  out) :: NbLoiConc
  type(LOI_CONC_T)  , dimension(:), pointer       :: LoiConc

  ! Sous-programme d'impression des parametres-resultats
  type(FICHIER_T)  , intent(inout) :: FicStockPLongCourlis
  type(FICHIER_T)  , intent(inout) :: FicStockPTransCourlis
  type(FICHIER_T)  , intent(inout) :: FicControleCourlis
  type(FICHIER_T)  , intent(inout) :: FicErreurCourlis
  type(FICHIER_T)  , intent(inout) :: FicResuGeom
  type(FICHIER_T)  , intent(inout) :: FicResuMES
  integer          , intent(  out) :: PasImpressionCourlis
  integer          , intent(  out) :: PasStockLongCourlis
  integer          , intent(  out) :: PasStockTransCourlis

  ! Sous-programme d'apport Courlis
  type(CL_COURLIS_T)                 , intent(  out) :: CL_Vase, CL_Sable
  type(SOURCE_TRACER_T), dimension(:), pointer       :: ApportVase
  type(SOURCE_TRACER_T), dimension(:), pointer       :: ApportSable

  ! Sous-programme des parametres de couplage
  integer,        intent(  out) :: NbIterHydro
  integer,        intent(  out) :: NbIterSedim

! Variables locales
  integer   :: UniteListing
  logical   :: ImpressionSedim
  logical   :: ImpressionGeom
  logical   :: ImpressionCouplage
  logical   :: ImpressionConcIni
  logical   :: ImpressionLoiConc
  logical   :: ImpressionApport
  integer   :: NbCouche   ! Nb de couches sedimentaires
  integer   :: NbInterface  ! Nb d'interfaces sedimentaires
  integer   :: NbProf   ! Nb de profils memorises dans Profil
  integer   :: NbProfCourlis ! Nb de profils memorises dans ProfilCourlis

  ! Concentration Initiale des traceurs
  !   (=> utilise pour les conc. de vase et sable)
  real(DOUBLE) , dimension(:,:), pointer :: Traceurs0
  real(DOUBLE) , dimension(:)  , pointer :: Abscisse
  integer          :: retour

! Traitement des erreurs
  type(ERREUR_T)             , intent(inout) :: Erreur
!  character(132)           :: arbredappel_old  ! PU2017 : Mise en commentaire

! VARIABLES POUR DAMOCLE
! ---------------------
  integer, parameter :: NMAX = 10000
  integer, parameter :: LANGUE_FRANCAISE = 1
  integer, parameter :: langue = LANGUE_FRANCAISE
  logical, parameter :: impression_doc = .false.
  type(FICHIER_T)    :: fichier_listing_damoc = FICHIER_T(9,"listing_courlis.damoc")

  character(LEN=72) , dimension(4,NMAX,2) :: MOTCLE
  character(LEN=144), dimension(NMAX)     :: MOTCAR
  logical           , dimension(NMAX)     :: MOTLOG
  integer           , dimension(NMAX)     :: MOTINT
  integer           , dimension(4,NMAX)   :: ADRESS
  integer           , dimension(4,NMAX)   :: DIMENS
  integer           , dimension(4,NMAX)   :: TROUVE
  real(DOUBLE)      , dimension(NMAX)     :: MOTREA

  type(FICHIER_T), intent(inout) :: FichierDico

!.. External Calls ..

  external DAMOC

!=========================================================================

End Subroutine PRETRAIT_COURLIS

End Interface

End Module M_Pretrait_Courlis_I
