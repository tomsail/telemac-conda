Subroutine Bilan_Courlis (  &

    FluxVase            ,  & ! Bilan sur les flux de vases
    FluxSable           ,  & ! Bilan sur les flux de sables
    MasseVase           ,  & ! Bilan sur les masses de vases
    MasseSable          ,  & ! Bilan sur les masses de sables
    DepotCumulCouche    ,  & ! Depot cumule /profil et /couche (> 0 depot, < 0 erosion)
    VolSedDepot         ,  & ! Volume de sedimt depose depuis debut du calcul
    DeltaSurfaceSed     ,  & ! Variation de la surface sedimentaire
    CVase               ,  & ! Concentration des vases en suspension
    CSable              ,  & ! Concentration des sables en suspension
    QVaseCouche         ,  & ! Flux de depot des vases par couche (> 0 depot, < 0 erosion)
    QSableCouche        ,  & ! Flux de depot des sables par couche (> 0 depot, < 0 erosion)
    QVase               ,  & ! Flux de depot des vases (> 0 depot, < 0 erosion)
    QSable              ,  & ! Flux de depot des sables (> 0 depot, < 0 erosion)
    QApportVase         ,  & ! Flux de d'apport lineaires des vases
    QApportSable        ,  & ! Flux de d'apport lineaires des sables
!    DeltaH              ,  & ! Variation de hauteur sedimentaire en chaque point des profils
    Dt                  ,  & ! Pas de temps
    ProfilCourlis       ,  & ! Profils sedimentaires
    Absc                ,  & ! Abscisse des sections de calcul (ProfilCourlis%Abs)
    Zsurf               ,  & ! Cote de la surface libre
    Vit                 ,  & ! Vitesse moyenne par section
    Sm                  ,  & ! Surface mouillee
    CoucheSed           ,  & ! Parametres sedimentaires des differentes couches
    Erreur              )


!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003		Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :	Calcul du bilan sedimentaire en masse
!  --------
!
!  Sous-programme appelant : Courlis
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C         ! Definition des constante tq EPS*, W0, ...
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info

use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS_T
use M_COUCHE_T            ! Definition du type COUCHE_T
use M_BILAN_FLUX_T        ! Definition du type BILAN_FLUX_T
use M_BILAN_MASSE_T       ! Definition du type BILAN_MASS_T

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

use M_MY_GLOBAL_VAR_SED

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE), intent(in) :: Dt

  real(DOUBLE)           , dimension(:)  , intent(in) :: Zsurf, Vit, Sm
  real(DOUBLE)           , dimension(:)  , intent(in) :: CVase, CSable
  real(DOUBLE)           , dimension(:,:), intent(in) :: QVaseCouche
  real(DOUBLE)           , dimension(:,:), intent(in) :: QSableCouche
  real(DOUBLE)           , dimension(:)  , intent(in) :: QVase, QSable
  real(DOUBLE)           , dimension(:)  , intent(in) :: QApportVase
  real(DOUBLE)           , dimension(:)  , intent(in) :: QApportSable
!  real(DOUBLE)           , dimension(:,:), intent(in) :: DeltaH
  type(COUCHE_T)         , dimension(:)  , intent(in) :: CoucheSed
  type(PROFIL_COURLIS_T) , dimension(:)  , intent(in) :: ProfilCourlis
  real(DOUBLE)           , dimension(:)  , intent(in) :: Absc

! Variables de sortie
  type(BILAN_FLUX_T)                  , intent(  out) :: FluxVase, FluxSable
  type(BILAN_MASSE_T)                 , intent(inout) :: MasseVase, MasseSable
  real(DOUBLE)        , dimension(:)  , intent(  out) :: DeltaSurfaceSed
  real(DOUBLE)        , dimension(:,:), intent(inout) :: DepotCumulCouche
  real(DOUBLE)                        , intent(inout) :: VolSedDepot

! Variables locales
  integer :: i, j, k     ! Compteurs
  integer :: NbProfil    ! Nombre de sections de calcul
  integer :: NbCouche    ! Nombre de couches sedimentaires
  integer :: NPt
  real(DOUBLE) :: Err

  real(DOUBLE) :: MasseVaseEauPrec, MasseSableEauPrec ! Masse de sediment presente dans l'eau au pas de tps precedent

  real(DOUBLE), dimension(:), allocatable :: FluxVaseCouche, FluxSableCouche

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  integer                       :: retour             ! Code de retour de la fonction read, allocate
!  character(132)                :: arbredappel_old    ! Ancien arbre d'appel  ! PU2017 : Mise en commentaire

!=========================================================================

  Erreur%Numero = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>Bilan'


!=========================================================================
! Initialisation
!=========================================================================

  NbProfil = size(Absc)
  NbCouche = size(CoucheSed)


!=========================================================================
! Allocation des tableaux locaux
!=========================================================================
  Allocate(FluxVaseCouche(NbCouche),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'FluxVaseCouche')
    return
  End if

  Allocate(FluxSableCouche(NbCouche),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'FluxSableCouche')
    return
  End if


!=========================================================================
!	Calcul des flux
!=========================================================================

! Initialisation
  FluxVase%Apport  = W0
  FluxSable%Apport = W0
  FluxVase%Depot   = W0
  FluxSable%Depot  = W0
  Do k = 1, NbCouche
    FluxVaseCouche  = W0
    FluxSableCouche = W0
  Enddo

! Flux de sediment entrant dans le bief
  FluxVase%Entrant  = CVase(1)  * Vit(1) * Sm(1)
  FluxSable%Entrant = CSable(1) * Vit(1) * Sm(1)

! Flux de sediment entrant dans le bief par apport
  Do i = 1, NbProfil
    FluxVase%Apport  = FluxVase%Apport  + QApportVase(i)
    FluxSable%Apport = FluxSable%Apport + QApportSable(i)
  Enddo

! Flux de sediment sortant du bief
  FluxVase%Sortant  = CVase(NbProfil)  * Vit(NbProfil) * Sm(NbProfil)
  FluxSable%Sortant = CSable(NbProfil) * Vit(NbProfil) * Sm(NbProfil)

! Flux de depot / erosion par couche dans le bief
  Do k = 1, NbCouche
    FluxVaseCouche(k)  = FluxVaseCouche(k)  + QVaseCouche(k,1)  * &
                         (Absc(2) - Absc(1)) / W2
    FluxSableCouche(k) = FluxSableCouche(k) + QSableCouche(k,1) * &
                         (Absc(2) - Absc(1)) / W2
  Enddo
  Do i = 2, NbProfil -1
    Do k = 1, NbCouche
      FluxVaseCouche(k)  = FluxVaseCouche(k)  + QVaseCouche(k,i)  * &
                           (Absc(i+1) - Absc(i-1)) / W2
      FluxSableCouche(k) = FluxSableCouche(k) + QSableCouche(k,i) * &
                           (Absc(i+1) - Absc(i-1)) / W2
    Enddo
  Enddo
  Do k = 1, NbCouche
    FluxVaseCouche(k)  = FluxVaseCouche(k)  +  &
                         QVaseCouche(k,NbProfil)  * &
                         (Absc(NbProfil) - Absc(NbProfil-1)) / W2
    FluxSableCouche(k) = FluxSableCouche(k) +  &
                         QSableCouche(k,NbProfil) * &
                         (Absc(NbProfil) - Absc(NbProfil-1)) / W2
  Enddo

! Flux de depot / erosion dans le bief
  Do k = 1, NbCouche
    FluxVase%Depot  = FluxVase%Depot  + FluxVaseCouche(k)
    FluxSable%Depot = FluxSable%Depot + FluxSableCouche(k)
  Enddo


!=========================================================================
!	Calcul des masses
!=========================================================================

! Initialisation
  MasseVaseEauPrec  = MasseVase%Eau
  MasseSableEauPrec = MasseSable%Eau

! Masse de sediment entree a l'amont du bief depuis l'intant initial
  MasseVase%Entrant  = MasseVase%Entrant  + FluxVase%Entrant  * Dt
  MasseSable%Entrant = MasseSable%Entrant + FluxSable%Entrant * Dt

! Masse de sediment entree par apport depuis l'instant initial
  MasseVase%Apport  = MasseVase%Apport  + FluxVase%Apport  * Dt
  MasseSable%Apport = MasseSable%Apport + FluxSable%Apport * Dt

! Masse de sediment sortie du bief depuis l'instant initial
  MasseVase%Sortant  = MasseVase%Sortant  + FluxVase%Sortant  * Dt
  MasseSable%Sortant = MasseSable%Sortant + FluxSable%Sortant * Dt

! Depot cumule (en tonnes) par couche dans chaque section de calcul
  Do i = 1, NbProfil
    Do k = 1, NbCouche
      DepotCumulCouche(k,i) = DepotCumulCouche(k,i) - &
                              (QVaseCouche(k,i) + QSableCouche(k,i)) * &
                              Dt / 1000._DOUBLE
    Enddo
  Enddo

! Masse de sediment deposee par couche dans le bief depuis T=0
  Do k = 1, NbCouche
    MasseVase%DepotCouche(k)  = MasseVase%DepotCouche(k)  - &
                                FluxVaseCouche(k)  * Dt
    MasseSable%DepotCouche(k) = MasseSable%DepotCouche(k) - &
                                FluxSableCouche(k) * Dt
  Enddo

! Masse de sediment dans l'eau a l'instant t
  MasseVase%Eau  = CVase(1)  * Sm(1) * (Absc(2) - Absc(1)) / W2
  MasseSable%Eau = CSable(1) * Sm(1) * (Absc(2) - Absc(1)) / W2
  Do i = 2, NbProfil-1
    MasseVase%Eau  = MasseVase%Eau  + CVase(i)  * Sm(i) * &
                     (Absc(i+1) - Absc(i-1)) / W2
    MasseSable%Eau = MasseSable%Eau + CSable(i) * Sm(i) * &
                     (Absc(i+1) - Absc(i-1)) / W2

  Enddo
  MasseVase%Eau  = MasseVase%Eau  + CVase(NbProfil)  * &
                   Sm(NbProfil) * (Absc(NbProfil) - Absc(NbProfil-1)) / W2
  MasseSable%Eau = MasseSable%Eau + CSable(NbProfil) * &
                   Sm(NbProfil) * (Absc(NbProfil) - Absc(NbProfil-1)) / W2

! Erreur relative a l'instant t
  If (Abs(MasseVase%Eau) < EPS6) then
    MasseVase%Erreur = W0
  Else
    Err = MasseVase%Eau - MasseVaseEauPrec - &
          (FluxVase%Entrant + FluxVase%Apport - &
          FluxVase%Sortant + FluxVase%Depot) * Dt

    MasseVase%Erreur = Err / MasseVase%Eau
  Endif
  If (Abs(MasseSable%Eau) < EPS6) then
    MasseSable%Erreur = W0
  Else
    Err = MasseSable%Eau - MasseSableEauPrec - &
          (FluxSable%Entrant + FluxSable%Apport - &
          FluxSable%Sortant + FluxSable%Depot) * Dt

    MasseSable%Erreur = Err / MasseSable%Eau
  Endif

!=========================================================================
!	Calcul des volumes
!=========================================================================

! Initialisation
  VolSedDepot = W0

! Calcul
  Do i = 1, NbProfil

    NPt = ProfilCourlis(i)%NbPoint
    DeltaSurfaceSed(i) = DeltaH(1,i) * (ProfilCourlis(i)%X(2) - &
                         ProfilCourlis(i)%X(1)) / W2
    Do j = 2, NPt-1
      DeltaSurfaceSed(i) = DeltaSurfaceSed(i) + DeltaH(j,i) * &
                           (ProfilCourlis(i)%X(j+1) - ProfilCourlis(i)%X(j-1)) &
                           / W2
    Enddo
    DeltaSurfaceSed(i) = DeltaSurfaceSed(i) + DeltaH(NPt,i) * &
                         (ProfilCourlis(i)%X(NPt) - ProfilCourlis(i)%X(NPt-1)) &
                         / W2

    If (i == 1) Then
      VolSedDepot = VolSedDepot + DeltaSurfaceSed(i) * &
                    (Absc(i+1) - Absc(i)) / W2
    Elseif (i == NbProfil) Then
      VolSedDepot = VolSedDepot + DeltaSurfaceSed(i) * &
                    (Absc(i) - Absc(i-1)) / W2
    Else
      VolSedDepot = VolSedDepot + DeltaSurfaceSed(i) * &
                    (Absc(i+1) - Absc(i-1)) / W2
    Endif

  Enddo



!=========================================================================
!	Deallocation des tableaux locaux
!=========================================================================

  Deallocate(FluxVaseCouche, FluxSableCouche)


!  Erreur%arbredappel = arbredappel_old

  Return

End Subroutine Bilan_Courlis
