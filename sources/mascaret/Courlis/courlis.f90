Subroutine Courlis      (  &

  CVase           ,  & ! Concentration des vases en suspension
  CSable          ,  & ! Concentration des sables en suspension
  QVaseCouche     ,  & ! Flux de depot des vases par couche (> 0 depot, < 0 erosion)
  QSableCouche    ,  & ! Flux de depot des sables par couche (> 0 depot, < 0 erosion)
  QVase           ,  & ! Flux de depot des vases (> 0 depot, < 0 erosion)
  QSable          ,  & ! Flux de depot des sables (> 0 depot, < 0 erosion)
  QApportVase     ,  & ! Flux de d'apport lineaires des vases
  QApportSable    ,  & ! Flux de d'apport lineaires des sables
  TauH            ,  & ! Contrainte hydraulique locale (depend du tirant d'eau local)
  TauHMoy         ,  & ! Contrainte hydraulique moyenne dans la section
  TauHMax         ,  & ! Contrainte hydraulique maximale dans la section
  TauE            ,  & ! Contrainte hydraulique effective (depend du rayon hydr.)
  TauEMoy         ,  & ! Contrainte hydraulique effective moyenne ds section
  TauEMax         ,  & ! Contrainte hydraulique effective maximale ds section
  Ceq             ,  & ! Concentration d'equilibre des sables locale
  CeqMoy          ,  & ! Concentration d'equilibre des sables moyenne dans la section
!  DeltaH          ,  & ! Variation de hauteur sedimentaire en chaque point des profils
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
  Sm0             ,  & ! Surface mouillee a t
  Sm1             ,  & ! Surface mouillee a t+DtCourlis
  Vit1            ,  & ! Vitesse moyenne par section a t+DtCourlis
  Zsurf1          ,  & ! Cote de la surface libre a t+DtCourlis
  Pm1             ,  & ! Perimetre mouille a t+DtCourlis
  CnuxV           ,  & ! Coefficient de diffusion vases
  CnuxS           ,  & ! Coefficient de diffusion sables
  ConsConv        ,  & ! Parametres schema de convection
  CoucheSed       ,  & ! Parametres sedimentaires des differentes couches
  LimiteSable     ,  & ! % de sable a partir duquel la couche est traitee suivant les lois du sable
  Talus           ,  & ! Parametres relatifs aux talus
  Resini          ,  & ! Resistance initiale des blocs au mouvement
  SurPl           ,  & !
  FluxVase        ,  & ! Bilan sur les flux de vases
  FluxSable       ,  & ! Bilan sur les flux de sables
  MasseVase       ,  & ! Bilan sur les masses de vases
  MasseSable      ,  & ! Bilan sur les masses de sables
  DepotCumulCouche,  & ! Depot cumule /profil et /couche (> 0 depot, < 0 erosion)
  VolSedDepot     ,  & ! Volume de sedimt depose depuis debut du calcul
  DeltaSurfaceSed ,  & ! Variation de la surface sedimentaire
  phase_talus     ,  & ! calcul de la stabilite des berges
  CalcSable       ,  & ! choix du calcul avec sable
  Erreur          )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER - M. Jodeau
!
!  VERSION : 5.1       08-2009  Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction : Calcule les termes sources  et resout l'equation de
!  --------    transport (convection - diffusion) des sediments dans l'eau
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele : StabiliteTalus
!  ---------------------   LimiteDepot
!                          DansLo
!                          Bilan
!
!=========================================================================

use M_PRECISION                 ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C               ! Definition des constante tq EPS*, W0, ...
use M_CONSTANTES_CALCUL_C       ! Constantes num, phys et info

use M_PROFIL_COURLIS_T          ! Definition du type PROFIL_COURLIS
use M_COUCHE_T                  ! Definition du type COUCHE_T
use M_APPORT_T                  ! Definition du type APPORT_T
use M_SOURCE_TRACER_T           ! Donnees des sources d'un traceur
use M_CONSTANTES_TRACER_T       ! parametres schema de convection
use M_CL_COURLIS_T              ! Definition du type CL_COURLIS_T
use M_LOI_T                     ! Definition du type LOI_T
use M_LOI_CONC_T                ! Definition du type LOI_T
use M_TALUS_T                   ! Definition du type TALUS_T
use M_BILAN_FLUX_T              ! Definition du type BILAN_FLUX_T
use M_BILAN_MASSE_T             ! Definition du type BILAN_MASS_T

use M_ERREUR_T                  ! Type ERREUR_T
use M_MESSAGE_C                 ! Messages d'erreur
use M_TRAITER_ERREUR_I          ! Traitement de l'errreur

use M_StabiliteTalus_I
use M_LimiteDepot_I
use M_compute_bedload_I
use M_compute_suspension_I
use M_Bilan_Courlis_I

use M_MY_GLOBAL_VAR_SED

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE)                             :: DtCourlis, TempsCourlis, TempsInitial
  real(DOUBLE),              intent(in   ) :: CnuxV
  real(DOUBLE),              intent(in   ) :: CnuxS
  type(CONSTANTES_TRACER_T), intent(inout) :: ConsConv
  logical :: CalcSable

  integer                               , intent(in)    :: phase_talus

  real(DOUBLE)          , dimension(:)  , intent(in   ) :: Zsurf1
  real(DOUBLE)          , dimension(:)  , intent(in   ) :: Sm0, Sm1, Pm1
  real(DOUBLE)                          , intent(in   ) :: LimiteSable
  type(COUCHE_T)        , dimension(:)  , intent(in   ) :: CoucheSed
  type(LOI_T)           , dimension(:)  , intent(in   ) :: LoiHydrau
  type(LOI_CONC_T)      , dimension(:)  , intent(in   ) :: LoiConc
  type(TALUS_T)                         , intent(in   ) :: Talus
  real(DOUBLE)          , dimension(:,:), intent(in   ) :: Resini

! Variables de sortie
  type(PROFIL_COURLIS_T), dimension(:)  , intent(inout) :: ProfilCourlis
  type(CL_COURLIS_T)                    , intent(inout) :: CL_Vase, CL_Sable
  type(SOURCE_TRACER_T) , dimension(:)  , intent(inout) :: ApportVase
  type(SOURCE_TRACER_T) , dimension(:)  , intent(inout) :: ApportSable
  type(APPORT_T)        , dimension(:)  , intent(inout) :: Apport
  real(DOUBLE)          , dimension(:)  , intent(inout) :: CVase, CSable
  real(DOUBLE)          , dimension(:,:), intent(inout) :: QVaseCouche
  real(DOUBLE)          , dimension(:,:), intent(inout) :: QSableCouche
  real(DOUBLE)          , dimension(:)  , intent(  out) :: QVase, QSable
  real(DOUBLE)          , dimension(:)  , intent(  out) :: QApportVase
  real(DOUBLE)          , dimension(:)  , intent(  out) :: QApportSable
  real(DOUBLE)          , dimension(:,:), intent(  out) :: TauH, TauE, Ceq
  real(DOUBLE)          , dimension(:)  , intent(  out) :: TauHMoy, TauHMax
  real(DOUBLE)          , dimension(:)  , intent(  out) :: TauEMoy, TauEMax
  real(DOUBLE)          , dimension(:)  , intent(  out) :: CeqMoy
!  real(DOUBLE)          , dimension(:,:), intent(inout) :: DeltaH
  real(DOUBLE)          , dimension(:)  , intent(inout) :: Vit1

  integer               , dimension(:,:), intent(inout) :: SurPl

  type(BILAN_FLUX_T)                    , intent(  out) :: FluxVase, FluxSable
  type(BILAN_MASSE_T)                   , intent(inout) :: MasseVase, MasseSable
  real(DOUBLE)          , dimension(:)  , intent(inout) :: DeltaSurfaceSed
  real(DOUBLE)          , dimension(:,:), intent(inout) :: DepotCumulCouche
  real(DOUBLE)                          , intent(inout) :: VolSedDepot

! Variables locales
!  integer                                               :: i,j      ! Compteurs  ! PU2017 : Mise en commentaire
  integer                                               :: NbProfil  ! Nombre de profils

  integer               , dimension(:)  , pointer       :: LimiteDepotG, LimiteDepotD

! Traitement des erreurs
  type(ERREUR_T), intent(inout)                         :: Erreur
  integer                                               :: retour  ! Code de retour de la fonction read, allocate
!  character(132) :: arbredappel_old                              ! Ancien arbre d'appel  ! PU2017 : Mise en commentaire

!=========================================================================

  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>Courlis'

  If (DtCourlis == 0.0) Then
    return
  Endif

!=========================================================================
! Initialisation et allocation des tableaux locaux
!=========================================================================

  NbProfil = size(ProfilCourlis)

  Allocate(LimiteDepotG(NbProfil),STAT=retour)
  If (retour /= 0) then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'LimiteDepotG')
    return
  End if

  Allocate(LimiteDepotD(NbProfil),STAT=retour)
  If (retour /= 0) then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'LimiteDepotD')
    return
  End if

!=========================================================================
! Calcul de la stabilite des berges
!=========================================================================
  If (phase_talus == PHASE_CALCUL .AND. sediment_slide_option) Then

    call StabiliteTalus  (  &
      ProfilCourlis  ,  & ! Profils sedimentaires
      Talus          ,  & ! Parametres relatifs aux talus
      DtCourlis      ,  & ! Pas de temps
      Zsurf1         ,  & ! Cote de la surface libre
      CoucheSed      ,  & ! Parametres sedimentaires des differentes couches
      DeltaH         ,  & ! Variation de hauteur sedimentaire en chaque point des profils
      Resini         ,  & ! Resistance initiale des blocs au mouvement
      SurPl          ,  & !
      Erreur         )

    If (Erreur%Numero /= 0) Then
      return
    Endif

  Endif

!=========================================================================
! Calcul des points limites de la partie immergee
!=========================================================================
  call LimiteDepot         (  &
    LimiteDepotG       ,  & ! numero du point au dela duquel il peut y a voir depot ou erosion (1er pt immerge)
    LimiteDepotD       ,  & ! numero du dernier pt pour lequel il peut y a voir depot ou erosion (dernier pt immerge)
    Zsurf1             ,  & ! Cote de la surface libre
    ProfilCourlis      ,  & ! Profils sedimentaires
    NbProfil           ,  & ! Nombre de profils
    Erreur             )

  If (Erreur%Numero /= 0) Then
    return
  Endif

!=========================================================================
! Calcul des termes sources par apport, depot-erosion et de l'equation
!  de transport diffusion
!=========================================================================
  if(bedload_option) then
    call compute_bedload  (  &
      CVase           ,  & ! Concentration des vases en suspension
      CSable          ,  & ! Concentration des sables en suspension
      QVaseCouche     ,  & ! Flux de depot des vases par couche (> 0 depot, < 0 erosion)
      QSableCouche    ,  & ! Flux de depot des sables par couche (> 0 depot, < 0 erosion)
      QVase           ,  & ! Flux de depot des vases (> 0 depot, < 0 erosion)
      QSable          ,  & ! Flux de depot des sables (> 0 depot, < 0 erosion)
      QApportVase     ,  & ! Flux de d'apport lineaires des vases
      QApportSable    ,  & ! Flux de d'apport lineaires des sables
      TauH            ,  & ! Contrainte hydraulique locale (depend du tirant d'eau local)
      TauHMoy         ,  & ! Contrainte hydraulique moyenne dans la section
      TauHMax         ,  & ! Contrainte hydraulique maximale dans la section
      TauE            ,  & ! Contrainte hydraulique effective (depend du rayon hydr.)
      TauEMoy         ,  & ! Contrainte hydraulique effective moyenne ds section
      TauEMax         ,  & ! Contrainte hydraulique effective maximale ds section
      Ceq             ,  & ! Concentration d'equilibre des sables locale
      CeqMoy          ,  & ! Concentration d'equilibre des sables moyenne dans la section
!      DeltaH          ,  & ! Variation de hauteur sedimentaire en chaque point des profils
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
      Zsurf1          ,  & ! Cote de la surface libre
      Sm0             ,  & ! Surface mouillee a t
      Sm1             ,  & ! Surface mouillee a t+dt
      Vit1            ,  & ! Vitesse moyenne par section a t+dt
      Pm1             ,  & ! Perimetre mouille a t+dt
      CnuxV           ,  & ! Coefficient de diffusion vases
      CnuxS           ,  & ! Coefficient de diffusion vases
      ConsConv        ,  & ! parametres schema de convection
      CoucheSed       ,  & ! Parametres sedimentaires des differentes couches
      LimiteDepotG    ,  & ! numero du point au dela duquel il peut y a voir depot ou erosion (1er pt immerge)
      LimiteDepotD    ,  & ! numero du dernier pt pour lequel il peut y a voir depot ou erosion (dernier pt immerge)
      LimiteSable     ,  & ! % de sable a partir duquel la couche est traitee suivant les lois du sable
      CalcSable       ,  & ! choix du calcul avec sable
      Erreur          )

    If (Erreur%Numero /= 0) Then
      return
    Endif
  endif

  if(suspension_option) then
    call compute_suspension(  &
      CVase         ,  & ! Concentration des vases en suspension
      CSable        ,  & ! Concentration des sables en suspension
      QVaseCouche   ,  & ! Flux de depot des vases par couche (> 0 depot, < 0 erosion)
      QSableCouche  ,  & ! Flux de depot des sables par couche (> 0 depot, < 0 erosion)
      QVase         ,  & ! Flux de depot des vases (> 0 depot, < 0 erosion)
      QSable        ,  & ! Flux de depot des sables (> 0 depot, < 0 erosion)
      QApportVase   ,  & ! Flux de d'apport lineaires des vases
      QApportSable  ,  & ! Flux de d'apport lineaires des sable
      TauH          ,  & ! Contrainte hydraulique locale (depend du tirant d'eau local)
      TauHMoy       ,  & ! Contrainte hydraulique moyenne dans la section
      TauHMax       ,  & ! Contrainte hydraulique maximale dans la section
      TauE          ,  & ! Contrainte hydraulique effective (depend du rayon hydr.)
      TauEMoy       ,  & ! Contrainte hydraulique effective moyenne ds section
      TauEMax       ,  & ! Contrainte hydraulique effective maximale ds section
      Ceq           ,  & ! Concentration d'equilibre des sables locale
      CeqMoy        ,  & ! Concentration d'equilibre des sables moyenne dans la section
      DeltaH        ,  & ! Variation de hauteur sedimentaire en chaque point des profils
      ProfilCourlis ,  & ! Profils sedimentaires
      CL_Vase       ,  & ! CL amont de la concentration en Vase
      CL_Sable      ,  & ! CL amont de la concentration en Sable
      ApportVase    ,  & ! Apports en vase
      ApportSable   ,  & ! Apports en sable
      Apport        ,  & ! Apports hydrauliques
      LoiHydrau     ,  & ! Lois hydrauliques
      LoiConc       ,  & ! Lois de concentration
      TempsCourlis  ,  & ! Temps du calcul
      DtCourlis     ,  & ! Pas de temps
      Zsurf1        ,  & ! Cote de la surface libre
      Sm0           ,  & ! Surface mouillee a t
      Sm1           ,  & ! Surface mouillee a t+dt
      Vit1          ,  & ! Vitesse moyenne par section a t+dt
      Pm1           ,  & ! Perimetre mouille a t+dt
      CnuxV         ,  & ! Coefficient de diffusion vases
      CnuxS         ,  & ! Coefficient de diffusion vases
      ConsConv      ,  & ! parametres schema de convection
      CoucheSed     ,  & ! Parametres sedimentaires des differentes couches
      LimiteDepotG  ,  & ! numero du point au dela duquel il peut y a voir depot ou erosion (1er pt immerge)
      LimiteDepotD  ,  & ! numero du dernier pt pour lequel il peut y a voir depot ou erosion (dernier pt immerge)
      LimiteSable   ,  & ! % de sable a partir duquel la couche est traitee suivant les lois du sable
      CalcSable     ,  & ! choix du calcul avec sable
      Erreur        )
    If (Erreur%Numero /= 0) Then
      return
    Endif
  endif

!=========================================================================
! Bilan en masse et en volume
!=========================================================================
  call Bilan_Courlis      (  &
    FluxVase          ,  & ! Bilan sur les flux de vases
    FluxSable         ,  & ! Bilan sur les flux de sables
    MasseVase         ,  & ! Bilan sur les masses de vases
    MasseSable        ,  & ! Bilan sur les masses de sables
    DepotCumulCouche  ,  & ! Depot cumule /profil et /couche (> 0 depot, < 0 erosion)
    VolSedDepot       ,  & ! Volume de sedimt depose depuis debut du calcul
    DeltaSurfaceSed   ,  & ! Variation de la surface sedimentaire
    CVase             ,  & ! Concentration des vases en suspension
    CSable            ,  & ! Concentration des sables en suspension
    QVaseCouche       ,  & ! Flux de depot des vases par couche (> 0 depot, < 0 erosion)
    QSableCouche      ,  & ! Flux de depot des sables par couche (> 0 depot, < 0 erosion)
    QVase             ,  & ! Flux de depot des vases (> 0 depot, < 0 erosion)
    QSable            ,  & ! Flux de depot des sables (> 0 depot, < 0 erosion)
    QApportVase       ,  & ! Flux de d'apport lineaires des vases
    QApportSable      ,  & ! Flux de d'apport lineaires des sables
!    DeltaH            ,  & ! Variation de hauteur sedimentaire en chaque point des profils
    DtCourlis         ,  & ! Pas de temps
    ProfilCourlis     ,  & ! Profils sedimentaires
    ProfilCourlis%Abs ,  & ! Abscisse des sections de calcul (ProfilCourlis%Abs)
    Zsurf1            ,  & ! Cote de la surface libre
    Vit1              ,  & ! Vitesse moyenne par section
    Sm1               ,  & ! Surface mouillee
    CoucheSed         ,  & ! Parametres sedimentaires des differentes couches
    Erreur            )

  If (Erreur%Numero /= 0) Then
    return
  Endif

!=========================================================================
! Deallocation des tableaux locaux
!=========================================================================
  Deallocate (LimiteDepotG, LimiteDepotD)

!  Erreur%arbredappel = arbredappel_old

  return

End Subroutine Courlis
