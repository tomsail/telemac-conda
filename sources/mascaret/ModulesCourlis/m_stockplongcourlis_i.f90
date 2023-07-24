Module M_StockPLongCourlis_I

Interface

Subroutine  StockPLongCourlis  ( &

    FicStockPLongCourlis   , & ! Unite logique fichier listing
    PhaseStockPLongCourlis , & ! Phase de la simulation (init, calcul, ??)
    Temps                  , & ! Temps courant
    num_pas                , & ! Numero du pas de temps
    NbProfil               , & ! Nombre de profils
    NbCouche               , & ! Nombre de couches
    ProfilCourlis          , & ! Profils sedimentaires
    Zsurf                  , & ! Cote de la surface libre
    Vitesse                , & ! Vitesse moyenne par section
    SurfMouil              , & ! Surface mouillee
    CVase                  , & ! Concentration des vases  en suspension
    CSable                 , & ! Concentration des sables en suspension
    DepotCumulCouche       , & ! Depot cumule /profil et /couche (> 0 depot, < 0 erosion)
    DeltaSurfaceSed        , & ! Variation de la surface sedimentaire
    QVase                  , & ! Flux de depot des vases (> 0 depot, < 0 erosion)
    QSable                 , & ! Flux de depot des sables (> 0 depot, < 0 erosion)
    TauHMax                , & ! Contrainte hydr. loc. max. ds section (depend du tirant d'eau local)
    TauHMoy                , & ! Contrainte hydr. loc. moy. ds section (depend du tirant d'eau local)
    TauEMax                , & ! Contrainte hydr. eff. max. ds section (depend du rayon hydr.)
    TauEMoy                , & ! Contrainte hydr. eff. moy. ds section (depend du rayon hydr.)
    CeqMoy                 , & ! Conc. d'equilibre des sables moy. ds section
    Erreur                 )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       05/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Ecriture du fichier resultat de Courlis au format RUBENS
!  --------    (format LIDONP) - trace des profils en long
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info

use M_FICHIER_T           ! Definition du type FICHIER_T
use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'erreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none


! Variables d'entree
!-------------------
  integer                     , intent(in   ) :: NbCouche, NbProfil
  integer                     , intent(in   ) :: num_pas
  integer                     , intent(in   ) :: PhaseStockPLongCourlis
  real(DOUBLE)                                :: Temps
  type(FICHIER_T)             , intent(in   ) :: FicStockPLongCourlis

  real(DOUBLE), dimension(:)  , intent(in   ) :: Zsurf
  real(DOUBLE), dimension(:)  , intent(in   ) :: Vitesse
  real(DOUBLE), dimension(:)  , intent(in   ) :: SurfMouil
  real(DOUBLE), dimension(:)  , intent(in   ) :: CVase, CSable
  real(DOUBLE), dimension(:)  , intent(in   ) :: QVase, QSable
  real(DOUBLE), dimension(:,:), intent(in   ) :: DepotCumulCouche
  real(DOUBLE), dimension(:)  , intent(in   ) :: DeltaSurfaceSed
  real(DOUBLE), dimension(:)  , intent(in   ) :: TauHMax, TauHMoy
  real(DOUBLE), dimension(:)  , intent(in   ) :: TauEMax,TauEMoy, CeqMoy

  type(PROFIL_COURLIS_T), dimension(:), intent(in)  :: ProfilCourlis

! Variables locales
! -----------------

! NbInterface [ 1 : dependant de NbCouche, 2 : inutile ici !? ]
  integer :: NbInterface

  integer :: i, k
  integer :: Unite  ! PU2017 : Mise en commentaire de NbBief

!  integer      , dimension(2)   :: iTab  ! PU2017 : Mise en commentaire
  integer                       :: I2  ! PU2017 : Mise en commentaire de I1
!  real(DOUBLE) , dimension(2)   :: rTab
!  character(72), dimension(2)   :: Titre
!  character(72)                 :: TitCal

  ! Noms des variables pour le standard LIGRAF
!  character(4) :: C_I1              = 'I1  '
!  character(4) :: C_I2              = 'I2  '
!  character(4) :: C_Vitesse         = 'VIT '
!  character(4) :: C_SurfMouil       = 'SMOU'
!  character(4) :: C_DeltaSurfaceSed = 'DSUR'
!  character(4) :: C_CVase           = 'CVAS'
!  character(4) :: C_CSable          = 'CSAB'
!  character(4) :: C_Zsurf           = 'ZSUR'
!  character(4) :: C_X               = 'XABS'
!  character(4) :: C_Zdur            = 'ZDUR'
!  character(4) :: C_QVase           = 'FLUV'
!  character(4) :: C_QSable          = 'FLUS'
!  character(4) :: C_TauHMax         = 'THMA'
!  character(4) :: C_TauHMoy         = 'THMO'
!  character(4) :: C_TauEMax         = 'TEMA'
!  character(4) :: C_TauEMoy         = 'TEMO'
!  character(4) :: C_CeqMoy          = 'CEQM'
!  character(4) :: C_Fin             = 'FIN '

!  character(4), dimension(7) :: C_Zref
!  character(4), dimension(7) :: C_DepotCumul

! Traitement des erreurs
!-----------------------
  integer        :: retour
!  character(132) :: arbredappel_old  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================

End Subroutine StockPLongCourlis

End Interface

End Module M_StockPLongCourlis_I
