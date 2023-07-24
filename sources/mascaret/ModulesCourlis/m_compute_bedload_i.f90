Module M_compute_bedload_I

Interface

Subroutine compute_bedload  (  &

    CVase           ,  & ! Concentration des vases en suspension
    CSable          ,  & ! Concentration des sables en suspension
    QVaseCouche     ,  & ! Flux de depot des vases par couche  (> 0 depot, < 0 erosion)
    QSableCouche    ,  & ! Flux de depot des sables par couche (> 0 depot, < 0 erosion)
    QVase           ,  & ! Flux de depot des vases  (> 0 depot, < 0 erosion)
    QSable          ,  & ! Flux de depot des sables (> 0 depot, < 0 erosion)
    QApportVase     ,  & ! Flux de d'apport lineaires des vases (Qapp*Capp) en (kg/s/m)
    QApportSable    ,  & ! Flux de d'apport lineaires des sables (Qapp*Capp) en (kg/s/m)
    TauH            ,  & ! Contrainte hydraulique locale (depend du tirant d'eau local)
    TauHMoy         ,  & ! Contrainte hydraulique moyenne dans la section
    TauHMax         ,  & ! Contrainte hydraulique maximale dans la section
    TauE            ,  & ! Contrainte hydraulique effective (depend du rayon hydr.)
    TauEMoy         ,  & ! Contrainte hydraulique effective moyenne ds section
    TauEMax         ,  & ! Contrainte hydraulique effective maximale ds section
    Ceq             ,  & ! Concentration d'equilibre des sables locale
    CeqMoy          ,  & ! Concentration d'equilibre des sables moyenne dans la section
!    DeltaH          ,  & ! Variation de hauteur sedimentaire en chaque point des profils
    ProfilCourlis   ,  & ! Profils sedimentaires
    CL_Vase         ,  & ! CL amont de la concentration en Vase
    CL_Sable        ,  & ! CL amont de la concentration en Sable
    ApportVase      ,  & ! Apports en vase
    ApportSable     ,  & ! Apports en sable
    Apport          ,  & ! Apports hydrauliques
    LoiHydrau       ,  & ! Lois hydrauliques
    LoiConc         ,  & ! Lois de concentration
    TempsCourlis    ,  & ! Temps du calcul
    TempsInitial    ,  & ! Premier temps
    DtCourlis       ,  & ! Pas de temps
    Zsurf1          ,  & ! Cote de la surface libre a t+dt
    Sm0             ,  & ! Surface mouillee a t
    Sm1             ,  & ! Surface mouillee a t+dt
    Vit1            ,  & ! Vitesse moyenne par section a t+dt
    Pm1             ,  & ! Perimetre mouille a t+dt
    CnuxV           ,  & ! Coefficient de diffusion vases
    CnuxS           ,  & ! Coefficient de diffusion Sables
    ConsConv        ,  & ! Parametres pour les schema de convection
    CoucheSed       ,  & ! Parametres sedimentaires des differentes couches
    LimiteDepotG    ,  & ! numero du point au dela duquel il peut y a voir depot ou erosion (1er pt immerge)
    LimiteDepotD    ,  & ! numero du dernier pt pour lequel il peut y a voir depot ou erosion (dernier pt immerge)
    LimiteSable     ,  & ! % de sable a partir duquel la couche est traitee suivant les lois du sable
    CalcSable       ,  & ! choix du calcul avec sable
    Erreur          )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER M. Jodeau
!
!  VERSION : 5.1       08-2009    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Calcule les termes sources  et resout l'equation de
!  --------    transport (convection - diffusion) des sediments dans l'eau
!
!  Sous-programme appelant : Courlis
!  -----------------------
!
!  Sous-programme appele :  CalcApport
!  ---------------------    Convec
!                           Sedimento
!                           Diffu
!
!=========================================================================

use M_PRECISION                 ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C               ! Definition des constante tq EPS*, W0, ...

USE M_SHARE_VAR, ONLY: profil, profilplan

use M_MY_GLOBAL_VAR_SED         ! PU2017: Acces aux variables globales Vsed et Hsed
use M_CSUR_I                    ! PU2017: Ajout pour mise a jour du planimetrage
use M_CSURM1_I                  ! PU2017: Ajout pour mise a jour du planimetrage

use M_PROFIL_COURLIS_T          ! Definition du type PROFIL_COURLIS
use M_COUCHE_T                  ! Definition du type COUCHE_T
use M_APPORT_T                  ! Definition du type APPORT_T
use M_SOURCE_TRACER_T           ! Donnees des sources d'un traceur
use M_CL_COURLIS_T              ! Definition du type CL_COURLIS_T
use M_LOI_T                     ! Definition du type LOI_T
use M_LOI_CONC_T                ! Definition du type LOI_T
use M_CONSTANTES_TRACER_T       ! parametres schema convection

use M_ERREUR_T                  ! Type ERREUR_T
use M_MESSAGE_C                 ! Messages d'erreur
use M_TRAITER_ERREUR_I          ! Traitement de l'errreur

use M_CalcApport_I
use M_Convec_I
use M_Sedimento_I
use M_Diffu_I

use M_INTERPOLATION_S           ! Sous-programme INTERPOLATION_S

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE)             :: DtCourlis, TempsCourlis, TempsInitial
  real(DOUBLE), intent(in) :: CnuxV
  real(DOUBLE), intent(in) :: CnuxS
  logical                  :: CalcSable

  real(DOUBLE)                  , dimension(:)  , intent(in   ) :: Zsurf1
  real(DOUBLE)                  , dimension(:)  , intent(in   ) :: Sm0, Sm1, Pm1
  real(DOUBLE)                                  , intent(in   ) :: LimiteSable
  integer                       , dimension(:)  , intent(in   ) :: LimiteDepotG
  integer                       , dimension(:)  , intent(in   ) :: LimiteDepotD
  type(COUCHE_T)                , dimension(:)  , intent(in   ) :: CoucheSed
  type(LOI_T)                   , dimension(:)  , intent(in   ) :: LoiHydrau
  type(LOI_CONC_T)              , dimension(:)  , intent(in   ) :: LoiConc

! Variables de sortie
  type(PROFIL_COURLIS_T)        , dimension(:)  , intent(inout) :: ProfilCourlis
  type(CL_COURLIS_T)                            , intent(inout) :: CL_Vase, CL_Sable
  type(SOURCE_TRACER_T)         , dimension(:)  , intent(inout) :: ApportVase
  type(SOURCE_TRACER_T)         , dimension(:)  , intent(inout) :: ApportSable
  type(APPORT_T)                , dimension(:)  , intent(inout) :: Apport
  real(DOUBLE)                  , dimension(:)  , intent(inout) :: CVase, CSable
  real(DOUBLE)                  , dimension(:,:), intent(inout) :: QVaseCouche
  real(DOUBLE)                  , dimension(:,:), intent(inout) :: QSableCouche
  real(DOUBLE)                  , dimension(:)  , intent(  out) :: QVase, QSable
  real(DOUBLE)                  , dimension(:,:), intent(  out) :: TauH, TauE, Ceq
  real(DOUBLE)                  , dimension(:)  , intent(  out) :: QApportVase
  real(DOUBLE)                  , dimension(:)  , intent(  out) :: QApportSable
  real(DOUBLE)                  , dimension(:)  , intent(  out) :: TauHMoy, TauHMax
  real(DOUBLE)                  , dimension(:)  , intent(  out) :: TauEMoy, TauEMax
  real(DOUBLE)                  , dimension(:)  , intent(  out) :: CeqMoy
!  real(DOUBLE)                  , dimension(:,:), intent(inout) :: DeltaH
  type(CONSTANTES_TRACER_T)                     , intent(inout) :: ConsConv
  real(DOUBLE)                  , dimension(:)  , intent(inout) :: Vit1

! Variables locales
  integer :: i, k               ! Compteurs  !, iapp
  integer :: NbProfil           ! Nombre de profils
  integer :: NbCouche           ! Nombre de couches sedimentaires
  integer :: NbApport           ! Nombre d'apports

! AJOUT PU2016
  integer                                   :: j
  integer                                   :: NPt                                      ! Nombre de point de la section en travers i
  integer                                   :: NPtmax                                   ! Nombre de point max
  real(DOUBLE)                              :: NPtd                                     ! Nombre de points de la section en travers i (double)
  real(DOUBLE)                              :: Rho, RhoS                                ! constantes du calcul  !, W52, W14
  real(DOUBLE)                              :: V, Sm, Rh, Rh13                          ! variables hydrauliques du profil !H13,Rh43,H,Zsl
  real(DOUBLE)                              :: V2
  real(DOUBLE)                              :: Kp, Kt, d50                              ! parametres sedimentaires de la couche affleurante ! Cf,M,Ps,Tce,W
  real(DOUBLE)                              :: d50m                                     ! diametre de grain moyen
  real(DOUBLE)                              :: TaH, TauHMa, TauHMo !variables intermediaires de calcul
  real(DOUBLE)                              :: TaE, TauEMo, TauEMa !, Ce, CeqMo ! variables intermediaires de calcul
  real(DOUBLE)                              :: HDepo                                    ! variables intermediaires de calcul (flux, volume, hauteur de depot) !Depo,VolDep
  real(DOUBLE), dimension(:)  , allocatable :: dxj                                      ! pas d'abscisse angulaire du profil
  integer     , dimension(:)  , allocatable :: Couche                                   ! numero de la 1ere couche non vide en chaque point de profil
  real(DOUBLE), dimension(:)  , allocatable :: Yi,Yj                                    ! abscisse longitudinale
  real(DOUBLE)                              :: minZb1, minZb2                           ! Cote du point bas
  real(DOUBLE), dimension(:)  , allocatable :: Fr                                       ! Nombre de Froude
  real(DOUBLE), dimension(:)  , allocatable :: Az                                       ! Volume de sediment depose par profil
  real(DOUBLE), dimension(:)  , allocatable :: dAz                                      ! Variation de volume de sediment depose par profil
  real(DOUBLE), dimension(:)  , allocatable :: Qsed                                     ! Capacite de transport par section
  real(DOUBLE)                              :: R, RG, Zsl, Z2                           ! Variables temporaires
  real(DOUBLE), dimension(:)  , allocatable :: Hroe                                     ! Hauteur d'eau (Roe)
  real(DOUBLE), dimension(:)  , allocatable :: Uroe                                     ! Vitesse (Roe)
  real(DOUBLE)                              :: H1,H2                                    ! Hauteurs d'eau temporaires
  real(DOUBLE)                              :: Tiro                                     ! Variables planimetrees
  real(DOUBLE), dimension(:)  , allocatable :: DZP                                      ! Tableau pas de planimetrage
  real(DOUBLE)                              :: B1
  integer                                   :: nb_pas

  integer , parameter :: ORDRE_INTERPOLATION = 1
  integer             :: num_loi            ! Numero de la loi utilisee


! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  type(ERREUR_T)                :: Erreur1
  integer                       :: retour  ! Code de retour de la fonction read, allocate

! MS2018 gestion des fonds durs (erosion)
!variables a declarer NodeWidth, HEros, HErosPot, EroPot, SedWidth, LayerThick, VolAvail
  real(DOUBLE)                  :: NodeWidth, HEros, HErosPot, EroPot
  real(DOUBLE)                  :: SedWidth, LayerThick, VolAvail

!=========================================================================

End Subroutine compute_bedload

End Interface

End Module M_compute_bedload_I
