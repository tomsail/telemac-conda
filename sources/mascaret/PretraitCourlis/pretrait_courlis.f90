Subroutine  PRETRAIT_COURLIS ( &

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
!  VERSION : 5.1       08-2009  Copyright EDF-CETMEF
!
!*************************************************************************
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
!=========================================================================

!======================================================================
! DECLARATIONS
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
! INITIALISATION
!=========================================================================

  UniteListing = FicListingCourlis%Unite

  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>PRETRAIT_COURLIS'

!=========================================================================
! LECTURE DU FICHIER DE MOT-CLE ET DU DICTIONNAIRE
!=========================================================================
! Ouverture du dictionnaire
  close(FichierDico%Unite)
  open(unit=FichierDico%Unite, file=FichierDico%Nom, access='SEQUENTIAL', &
       action='READ'         , form='FORMATTED'    , iostat=RETOUR      , &
       position='rewind'     , status='OLD'     )

  If (retour /= 0) Then
    Erreur%Numero = 3
    Erreur%ft   = err_3
    Erreur%ft_c = err_3c
    call TRAITER_ERREUR (Erreur, fichierDico%Nom)
    return
  Endif

! Ouverture du fichier mot-cle
  close(FichierMotCle%Unite)
  open(unit=FichierMotCle%Unite, file=FichierMotCle%Nom, access='SEQUENTIAL', &
       action='READ'           , form='FORMATTED'      , iostat=RETOUR      , &
       position='rewind'       , status='OLD'     )

  If (retour /= 0) Then
    Erreur%Numero = 3
    Erreur%ft   = err_3
    Erreur%ft_c = err_3c
    call TRAITER_ERREUR (Erreur, fichierMotCle%Nom)
    return
  Endif

! Ouverture du fichier listing DAMOCLES
  open(unit=fichier_listing_damoc%Unite, file=fichier_listing_damoc%Nom, &
       access='SEQUENTIAL', action='WRITE'     , form='FORMATTED'      , &
       iostat=RETOUR      , position='rewind'  , status='REPLACE'      )

  If (retour /= 0) Then
    Erreur%Numero = 4
    Erreur%ft   = err_4
    Erreur%ft_c = err_4c
    call TRAITER_ERREUR (Erreur, fichier_listing_damoc%Nom)
    return
  End if

  DIMENS(1,:) = 0
  DIMENS(2,:) = 0
  DIMENS(3,:) = 0
  DIMENS(4,:) = 0

  call DAMOCLESMASC             ( &
    ADRESS                      , &
    DIMENS                      , &
    NMAX                        , &
    impression_doc              , &
    langue                      , &
    fichier_listing_damoc%Unite , &
    MOTINT                      , &
    MOTREA                      , &
    MOTLOG                      , &
    MOTCAR                      , &
    MOTCLE                      , &
    TROUVE                      , &
    FichierDico%Unite           , &
    FichierMotCle%Unite         , &
    .false.                     )

!  OptionCourlis = MOTLOG(ADRESS(3,600))
! NICOLE   CHARRIAGE     =   MOTLOG(ADRESS(3,668))
  CalcSable = MOTLOG(ADRESS(3,607))

  PretraitCourlis : If (OptionCourlis) Then

!=========================================================================
! VERIFICATION DE COMPATIBILITE DES OPTIONS (HYDRO,COURLIS)
!=========================================================================

!    ! Le noyau de calcul hydraulique doit etre MASCARET ou REZODT
!    If (Noyau == NOYAU_SARAP) Then
!      Erreur%Numero = 407
!      Erreur%ft   = err_407
!      Erreur%ft_c = err_407c
!      call TRAITER_ERREUR (Erreur, 'le noyau de calcul SARAP')
!      return
!    End if
!
!    ! Le noyau de calcul hydraulique doit etre MASCARET ou REZODT
!    If (OptionCasier) Then
!      Erreur%Numero = 407
!      Erreur%ft   = err_407
!      Erreur%ft_c = err_407c
!      call TRAITER_ERREUR (Erreur, 'l''option CASIER')
!      return
!    End if
!
!    ! il est preferable de choisir l'option pas de temps variable avec le noyau transcritique
!    If ((Noyau == NOYAU_MASCARET) .and. (.NOT.PasTempsVariable)) Then
!      Erreur%Numero = 409
!      Erreur%ft   = err_409
!      Erreur%ft_c = err_409c
!      call TRAITER_ERREUR (Erreur, 'il est recommande d''avoir ', &
!      'un pas de temps variable, avec le noyau MASCARET.')
!       return
!    End if
!
!    ! Le maillage doit etre section de calcul = profil
!    If (Type_Maillage /= TYPE_MAILLAGE_PROFIL) Then
!      Erreur%Numero = 407
!      Erreur%ft   = err_407
!      Erreur%ft_c = err_407c
!      call TRAITER_ERREUR (Erreur, 'Un type de maillage autre que PROFIL')
!      return
!    End if
!
!    ! Le critere d'arret du calcul est de preferance un temps final
!    If (CritereArret  /= TEMPS_MAXIMUM) Then
!      Erreur%Numero = 407
!      Erreur%ft   = err_407
!      Erreur%ft_c = err_407c
!      call TRAITER_ERREUR (Erreur, &
!       'Le critere d''arret de calcul NB PAS DE TEMPS MAXIMUM')
!      return
!    End if
!
!    ! Bief unique
!    If (Nbief /= 1) Then
!      Erreur%Numero = 407
!      Erreur%ft   = err_407
!      Erreur%ft_c = err_407c
!      call TRAITER_ERREUR (Erreur, 'Un modele avec plusieurs biefs')
!      return
!    End if

!=========================================================================
! LECTURE DES PARAMETRES D'IMPRESSION ET DE RESULTATS
!  ET OUVERTURE DU FICHIER D'IMPRESSION DES PARAMETRES
!=========================================================================

    call LecImpResCourlis ( &
      FichierMotCle         , &
      FicListingCourlis     , &
      FicStockPLongCourlis  , &
      FicStockPTransCourlis , &
      FicControleCourlis    , &
      FicErreurCourlis      , &
      FicResuGeom           , &
      FicResuMES            , &
      ImpressionSedim       , &
      ImpressionGeom        , &
      ImpressionCouplage    , &
      ImpressionConcIni     , &
      ImpressionLoiConc     , &
      ImpressionApport      , &
      PasImpressionCourlis  , &
      PasStockLongCourlis   , &
      PasStockTransCourlis  , &
      MOTINT                , &
      MOTCAR                , &
      MOTLOG                , &
      ADRESS                , &
      Erreur                )

    If (Erreur%numero /= 0)  return

!=========================================================================
! LECTURE DES PARAMETRES DE SEDIMENTS
!=========================================================================

    call LecParamSedim ( &
      UniteListing    , & ! Unite logique de FicListingCourlis
      FichierSedim    , &
      ImpressionSedim , &
      NbCouche        , &
      CoucheSed       , &
      Talus           , &
      LimiteSable     , &
      CnuxV           , &
      CnuxS           , &
      ConsConv        , &
!      FracH           , &
      MOTINT          , &
      MOTREA          , &
      MOTCAR          , &
      ADRESS          , &
      MOTLOG          , &
      Erreur          )

    If (Erreur%numero /= 0)  return

!=========================================================================
! LECTURE DE LA GEOMETRIE DES LITS DES RIVIERES
! (ET COMPARAISONS AVEC MASCARET)
!=========================================================================

    NbInterface = NbCouche + 1
    NbProf = size(Profil)

    call LecGeomCourlis ( &
      UniteListing   , & ! Unite logique de FicListingCourlis
      FicGeomCourlis , & ! Fichier des profils des interfaces
      ImpressionGeom , &
      NbInterface    , & ! Nb d'interfaces
      NbProf         , & ! Nombre de profils
      Profil         , & ! Profil
      ProfilCourlis  , & ! Profils de la geometrie
      MOTCAR         , &
      ADRESS         , &
      Erreur         )

    If (Erreur%numero /= 0)  return

!=========================================================================
! LECTURE DES CONCENTRATIONS INITIALES
!=========================================================================

    NbProfCourlis = size(ProfilCourlis)

    allocate(Abscisse(NbProfCourlis), STAT = retour)
    Abscisse(:) = ProfilCourlis(:)%Abs
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'Abscisse')
      return
    Endif

    Abscisse(:) = ProfilCourlis(:)%Abs

    If (suspension_option) Then
      call LecConcIni ( &
        UniteListing      , & ! Unite du fichier d'impression des parametres
        ImpressionConcIni , & ! Choix d'impression des concentrations initiales
        FicCMESIni        , & ! Fichier des concentrations de sable et vase
                              ! presentes initialement dans la retenue
        Abscisse          , & ! Abscisses curvilignes des sections de calcul
        NbProfCourlis     , & ! Nombre de sections de calcul
        OptionCourlis     , & ! commutateur : VRAI si calcul COURLIS
        Traceurs0         , & ! Concentrations initiales des traceurs
                              ! Lecture des mots du dictionnaires
        MOTINT            , &
        MOTREA            , &
        MOTCAR            , &
        ADRESS            , &
        Erreur            ) ! Traitement des erreurs

      If (Erreur%numero /= 0)  return



      allocate(CVaseIni(NbProfCourlis), STAT = retour)
      If (retour /= 0) Then
        Erreur%Numero = 5
        Erreur%ft   = err_5
        Erreur%ft_c = err_5c
        call TRAITER_ERREUR (Erreur, 'CVaseIni')
        return
      End if

      allocate(CSableIni(NbProfCourlis), STAT = retour)
      If (retour /= 0) Then
        Erreur%Numero = 5
        Erreur%ft   = err_5
        Erreur%ft_c = err_5c
        call TRAITER_ERREUR (Erreur, 'CSableIni')
        return
      End if

      CVaseIni(:)  = Traceurs0(1,:)
      CSableIni(:) = Traceurs0(2,:)
    Endif

!=========================================================================
! LECTURE DES CHRONIQUES DE CONCENTRATION
!=========================================================================

    call LecLoiConc ( &
      UniteListing      , &
      ImpressionLoiConc , &
      FicLoiConc        , &
      NbLoiConc         , &
      LoiConc           , &
      CritereArret      , &
      TempsMaximum      , &
      MOTINT            , &
      MOTREA            , &
      MOTCAR            , &
      ADRESS            , &
      Erreur            )

    If (Erreur%numero /= 0)  return

!=========================================================================
! LECTURE DES CARACTERISTIQUES DES APPORTS
!=========================================================================

    call LecApportCourlis ( &
      UniteListing     , &
      ImpressionApport , &
      Apport           , &
      NbLoiConc        , &
      ApportVase       , &
      ApportSable      , &
      CL_Vase          , &
      CL_Sable         , &
      ! Lecture des mots du dictionnaires
      MOTINT           , &
      ADRESS           , &
      ! Traitement des erreurs
      Erreur           )

    If (Erreur%numero /= 0)  return

!=========================================================================
! LECTURE DES PARAMETRES DE COUPLAGE
!=========================================================================

    call LecCouplage ( &
      UniteListing       , &
      ImpressionCouplage , &
      NbIterHydro        , &
      NbIterSedim        , &
      MOTINT             , &
      ADRESS             , &
      Erreur             )

    If (Erreur%numero /= 0)  return

!=========================================================================
! FIN DU SOUS-PROGRAMME
!=========================================================================
  Endif PretraitCourlis

  close(FichierDico%Unite)
  close(FichierMotCle%Unite)
  close(fichier_listing_damoc%Unite)

!  Erreur%arbredappel = arbredappel_old

End Subroutine PRETRAIT_COURLIS
