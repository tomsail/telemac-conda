Subroutine  PostImpCourlis      ( &

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
!  VERSION : 4.0       05/2003		Copyright EDF-CETMEF
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
! INITIALISATION
!=========================================================================
  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>PostImpCourlis'

  UniteList = FicListingCourlis%Unite

  Select case (PhasePostImpCourlis)

!=========================================================================
! ECRITURE DE LA LEGENDE AU PREMIER PAS DE TEMPS
!=========================================================================
  Case (PHASE_INITIALISATION)

!    write(UniteList,1000)
!    write(UniteList,1100)


    ! Impression de l'en-tete
    !------------------------
!    write(UniteList,1200) Modele,num_pas,Temps

    ! IMPRESSION DES VARIABLES PAR SECTION
    !-------------------------------------
!    write(UniteList,1400)

    Do i = 1, NbProfil
      write(UniteList) dble(Temps)              , &
                       dble(i)                  , &
                       dble(Absc(i))            , &
                       dble(Zsurf(i))           , &
                       dble(Zref(i))            , &
                       dble(Vitesse(i))         , &
                       dble(Sm(i))              , &
                       dble(CVase(i))           , &
                       dble(CSable(i))          , &
                       dble(W0)                 , &
                       dble(DeltaSurfaceSed(i)) , &
                       dble(QVase(i))           , &
                       dble(QSable(i))          , &
                       dble(TauHMax(i))         , &
                       dble(TauHMoy(i))         , &
                       dble(TauEMoy(i))         , &
                       dble(CeqMoy(i))
    EndDo

!=========================================================================
! ECRITURE DES VARIABLES TEMPORELLES
!=========================================================================
  Case (PHASE_CALCUL)

    ! Allocation de memoire des tableaux locaux
    !------------------------------------------
    allocate (MasseDepotTotal(NbCouche),STAT = Retour)
    If (Retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'MasseDepotTotal')
      return
    Endif

    allocate (DepotCumul(NbProfil),STAT = Retour)
    If (Retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'DepotCumul')
      return
    Endif

!    Modele = 'SEDI  '  ! PU2017 : Mise en commentaire

    ! Impression de l'en-tete
    !------------------------
!    write(UniteList,1200) Modele,num_pas,Temps

!    write(UniteList,1300) Dt

!=========================================================================
! CALCUL DU DEPOT CUMULE DEPUIS LE DEBUT DU CALCUL PAR PROFIL
!=========================================================================
    Do i = 1, NbProfil
      DepotCumul(i) = W0
      Do k = 1, NbCouche
        DepotCumul(i) = DepotCumul(i) + DepotCumulCouche(k,i)
      Enddo
    Enddo

!=========================================================================
! IMPRESSION DES VARIABLES PAR SECTION
!=========================================================================
!    write(UniteList,1400)

    Do i = 1, NbProfil
      write(UniteList) dble(Temps)              , &
                       dble(i)                  , &
                       dble(Absc(i))            , &
                       dble(Zsurf(i))           , &
                       dble(Zref(i))            , &
                       dble(Vitesse(i))         , &
                       dble(Sm(i))              , &
                       dble(CVase(i))           , &
                       dble(CSable(i))          , &
                       dble(DepotCumul(i))      , &
                       dble(DeltaSurfaceSed(i)) , &
                       dble(QVase(i))           , &
                       dble(QSable(i))          , &
                       dble(TauHMax(i))         , &
                       dble(TauHMoy(i))         , &
                       dble(TauEMoy(i))         , &
                       dble(CeqMoy(i))
    EndDo

!=========================================================================
! BILAN EN MASSE ET EN FLUX SUR LE BIEF
!=========================================================================
    MasseDepVaseTotal  = W0
    MasseDepSableTotal = W0

    Do k = 1, NbCouche
      MasseDepVaseTotal  = MasseDepVaseTotal  + MasseVase%DepotCouche(k)
      MasseDepSableTotal = MasseDepSableTotal + MasseSable%DepotCouche(k)
      MasseDepotTotal(k) = MasseVase%DepotCouche(k) + MasseSable%DepotCouche(k)
    Enddo

!    MasseTotal       = MasseDepVaseTotal + MasseDepSableTotal  ! PU2017 : Mise en commentaire
    MasseEauTotal    = MasseVase%Eau     + MasseSable%Eau
!    MasseEntreeTotal = MasseVase%Entrant + MasseSable%Entrant  ! PU2017 : Mise en commentaire
!    MasseSortieTotal = MasseVase%Sortant + MasseSable%Sortant  ! PU2017 : Mise en commentaire

    MasseErreurVase  = MasseVase%Entrant  + MasseVase%Initiale                 &
                       - MasseVase%Sortant  - MasseDepVaseTotal  - MasseVase%Eau
    MasseErreurSable = MasseSable%Entrant + MasseSable%Initiale                &
                       - MasseSable%Sortant - MasseDepSableTotal - MasseSable%Eau

    If (max(MasseVase%Entrant, abs( &
                                   MasseDepVaseTotal), &
                                   MasseVase%Initiale) < EPS6) Then
      MasseErrRelVase = W0
    Else
      MasseErrRelVase = MasseErreurVase  / max( &
                                               MasseVase%Eau, &
                                               abs(MasseDepVaseTotal), &
                                               MasseVase%Initiale)
    Endif
    If (max(MasseSable%Entrant, &
            abs(MasseDepSableTotal), &
            MasseSable%Initiale) < EPS6) Then
      MasseErrRelSable = W0
    Else
      MasseErrRelSable = MasseErreurSable / max( &
                                                MasseSable%Eau, &
                                                abs(MasseDepSableTotal), &
                                                MasseSable%Initiale)
    Endif

!	   write (UniteList,2000)

    write (UniteList)  dble(temps)                       , &
                       dble(999)                         , &
                       dble(FluxVase%Depot)              , &
                       dble(FluxSable%Depot)             , &
                       dble(FluxVase%Entrant)            , &
                       dble(FluxSable%Entrant)           , &
                       dble(FluxVase%Sortant)            , &
                       dble(FluxSable%Sortant)           , &
                       dble(0), dble(0), dble(0), dble(0), &
                       dble(0), dble(0), dble(0), dble(0), dble(0)

!    write(UniteList,2200) Temps

    Do k = 1, NbCouche
      write (UniteList) dble(temps)                                , &
                        dble(1999)                                 , &
                        dble(k)                                    , &
                        dble(MasseVase%DepotCouche(k))             , &
                        dble(MasseSable%DepotCouche(k))            , &
                        dble(MasseDepotTotal(k))                   , &
                        dble(0), dble(0), dble(0), dble(0), dble(0), &
                        dble(0), dble(0), dble(0), dble(0), dble(0), dble(0)
    End Do

    write (UniteList) dble(temps)                                , &
                      dble(2999)                                 , &
                      dble(MasseVase%Eau)                        , &
                      dble(MasseSable%Eau)                       , &
                      dble(MasseEauTotal)                        , &
                      dble(MasseVase%Erreur)                     , &
                      dble(MasseSable%Erreur)                    , &
                      dble(0), dble(0), dble(0), dble(0), dble(0), &
                      dble(0), dble(0), dble(0), dble(0), dble(0)

!   write(UniteList,2500)


    write(UniteList) dble(temps)                                    , &
                     dble(3999)                                     , &
                     dble(MasseVase%Initiale)                       , &
                     dble(MasseSable%Initiale)                      , &
!                     dble(MasseVase%Initiale + MasseSable%Initiale) , &
                     dble(MasseVase%Entrant)                        , &
                     dble(MasseSable%Entrant)                       , &
!                     dble(MasseEntreeTotal)                         , &
                     dble(MasseVase%Sortant )                       , &
                     dble(MasseSable%Sortant)                       , &
!                     dble(MasseSortieTotal)                         , &
                     dble(MasseVase%Eau)                            , &
                     dble(MasseSable%Eau)                           , &
!                     dble(MasseEauTotal)                            , &
                     dble(MasseDepVaseTotal)                        , &
                     dble(MasseDepSableTotal)                       , &
!                     dble(MasseTotal)                               , &
                     dble(MasseErreurVase)                          , &
                     dble(MasseErreurSable)                         , &
                     dble(MasseErrRelVase)                          , &
                     dble(MasseErrRelSable)                         , &
                     dble(VolSedDepot)

    deallocate(MasseDepotTotal, DepotCumul)

!=========================================================================
! FERMETURE DU FICHIER LISTING
!=========================================================================
  Case (PHASE_TERMINAISON)

    close(UniteList)

  End Select

!=========================================================================
! FIN DU SOUS-PROGRAMME
!=========================================================================

!  Erreur%arbredappel = arbredappel_old

  return

End Subroutine PostImpCourlis
