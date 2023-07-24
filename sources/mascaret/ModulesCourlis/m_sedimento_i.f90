Module M_Sedimento_I

Interface

Subroutine Sedimento  (  &

  QVaseCouche    ,  & ! Flux de depot des vases par couche (> 0 depot, < 0 erosion)
  QSableCouche   ,  & ! Flux de depot des sables par couche (> 0 depot, < 0 erosion)
  TauH           ,  & ! Contrainte hydraulique locale (depend du tirant d'eau local)
  TauHMoy        ,  & ! Contrainte hydraulique moyenne dans la section
  TauHMax        ,  & ! Contrainte hydraulique maximale dans la section
  TauE           ,  & ! Contrainte hydraulique effective (depend du rayon hydr.)
  TauEMoy        ,  & ! Contrainte hydraulique effective moyenne ds section
  TauEMax        ,  & ! Contrainte hydraulique effective maximale ds section
  Ceq            ,  & ! Concentration d'equilibre des sables locale
  CeqMoy         ,  & ! Concentration d'equilibre des sables moyenne dans la section
  DeltaH         ,  & ! Variation de hauteur sedimentaire en chaque point des profils
  ProfilCourlis  ,  & ! Profils sedimentaires
  NbProfil       ,  & ! Nombre de profils
  CoucheSed      ,  & ! Parametres sedimentaires des differentes couches
  NbCouche       ,  & ! Nombre de couches
  CVase          ,  & ! Concentration des vases en suspension
  CSable         ,  & ! Concentration des sables en suspension
  Dt             ,  & ! Pas de temps
  Zsurf          ,  & ! Cote de la surface libre
  Vitesse        ,  & ! Vitesse moyenne par section
  SurfMouil      ,  & ! Surface mouillee
  PerimMouil     ,  & ! Perimetre mouille
  LimiteDepotG   ,  & ! numero du point au dela duquel il peut y a voir depot ou erosion (1er pt immerge)
  LimiteDepotD   ,  & ! numero du dernier pt pour lequel il peut y a voir depot ou erosion (dernier pt immerge)
  LimiteSable    ,  & ! % de sable a partir duquel la couche est traitee suivant les lois du sable
  CalcSable      ,  & ! choix calcul avec sable
  Erreur         )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Calcul des flux sedimentaire a l'equilibre, resolution de
!  --------   l'equation d'Exner (avec un decentrement adaptatif selon
!             le regime de l'ecoulement amont-aval) et calcul d'evolution
!             des fonds
!
!  Sous-programme appelant :
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_PRECISION         ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C       ! Definition des constante tq EPS*, W0, ...
use M_PROFIL_COURLIS_T  ! Definition du type PROFIL_COURLIS
use M_COUCHE_T          ! Definition du type COUCHE_T

use M_ERREUR_T          ! Type ERREUR_T
use M_MESSAGE_C         ! Messages d'erreur
use M_TRAITER_ERREUR_I  ! Traitement de l'erreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE)        :: Dt
  integer, intent(in) :: NbProfil
  integer, intent(in) :: NbCouche
  logical             :: CalcSable

  type(COUCHE_T),         dimension(:) , intent(in   )  :: CoucheSed
  real(DOUBLE),           dimension(:) , intent(in   )  :: CVase, CSable
  real(DOUBLE),           dimension(:) , intent(in   )  :: Zsurf, Vitesse
  real(DOUBLE),           dimension(:) , intent(in   )  :: SurfMouil
  real(DOUBLE),           dimension(:) , intent(in   )  :: PerimMouil
  real(DOUBLE)                         , intent(in   )  :: LimiteSable
  integer,                dimension(:) , intent(in   )  :: LimiteDepotG
  integer,                dimension(:) , intent(in   )  :: LimiteDepotD
  type(PROFIL_COURLIS_T), dimension(:) , intent(inout)  :: ProfilCourlis

! Variables de sortie
  real(DOUBLE), dimension(:,:), intent(inout) :: QVaseCouche, QSableCouche
  real(DOUBLE), dimension(:,:), intent(  out) :: TauH, TauE, Ceq
  real(DOUBLE), dimension(:)  , intent(  out) :: TauHMoy, TauHMax
  real(DOUBLE), dimension(:)  , intent(  out) :: TauEMoy, TauEMax
  real(DOUBLE), dimension(:)  , intent(  out) :: CeqMoy
  real(DOUBLE), dimension(:,:), intent(  out) :: DeltaH

! Variables locales
  integer                                 :: i, j, k, kk
  integer                                 :: NPt                                      ! Nombre de point de la section en travers i
  real(DOUBLE)                            :: Rho, RhoS, W52, W14                      ! constantes du calcul
  real(DOUBLE)                            :: V, Sm, Rh, Rh43, Rh13, Zsl, H            ! variables hydrauliques du profil
  real(DOUBLE)                            :: Cv, Cs                                   ! concentrations en suspension au profil i
  real(DOUBLE)                            :: dxj                                      ! pas d'abscisse angulaire du profil
  real(DOUBLE)                            :: Kp, Kt, Tce, M, d50, Cf, W, Tcd, Ps      ! parametres sedimentaires de la couche affleurante
  real(DOUBLE)                            :: TaH, TauHMa, TauHMo                      ! variables intermediaires de calcul
  real(DOUBLE)                            :: TaE, TauEMo, TauEMa                      ! variables intermediaires de calcul
  real(DOUBLE)                            :: Qv, Ce, CeqMo                            ! variables intermediaires de calcul
  real(DOUBLE)                            :: Flux                                     ! variables intermediaires de calcul
  real(DOUBLE)                            :: Depo, HDepo, VolDep                      ! variables intermediaires de calcul (flux, volume, hauteur de depot)
  real(DOUBLE)                            :: Eros, HEros                              ! variables intermediaires de calcul (flux, hauteur d'erosion)
  real(DOUBLE)                            :: VolPot                                   ! Volume erodable potentiel (si la couche n'est pas limitee en epaisseur)
  real(DOUBLE)                            :: VolDis                                   ! Volume de sediment disponible pour l'erosion
  real(DOUBLE)                            :: z_ref                                    ! point bas des interfaces sedimentaires
  logical                                 :: Vaseux                                   ! a vrai si la couche suit les lois des vases
  real(DOUBLE), dimension(:), allocatable :: Xj, Fr                                   ! abscisse transversale du profil en cours et nombre de Froude
  integer     , dimension(:), allocatable :: Couche                                   ! numero de la 1ere couche non vide en chaque point de profil
  real(DOUBLE), dimension(:), allocatable :: ErosPot                                  ! flux d'erosion potentiel en chaque pt du profil

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  integer                       :: retour            ! Code de retour de la fonction read, allocate
  character(132)                :: arbredappel_old   ! Ancien arbre d'appel

!=========================================================================

End Subroutine Sedimento

End Interface

End Module M_Sedimento_I
