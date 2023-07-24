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
!  FracH             , & ! MS2018: nouvelle variable, clipping evolution pourcentage de la hauteur d eau
  MOTINT            , &
  MOTREA            , &
  MOTCAR            , &
  ADRESS            , &
  MOTLOG            , &
  Erreur            )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL - M. Jodeau
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
! INITIALISATION
!=========================================================================

  Erreur%Numero = 0
!  arbredappel_old = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LecParamSedim'

!  If(ImpressionSedim) write(UniteListing,2000)

!=========================================================================
! CHOIX DU MODE DE SAISIE
!=========================================================================

  ModeParamSedim = MOTINT(ADRESS(1,600))

!=========================================================================
! TRAITEMENT DES DONNEES EN FONCTION DU MODE DE SAISIE
!=========================================================================

! SAISIE PAR FICHIER => APPEL DU SS-PROG LecFichierSedim
! ------------------------------------------------------
  ModeSaisie: If (ModeParamSedim == SAISIE_PAR_FICHIER) Then

    FichierSedim%Nom = MOTCAR(ADRESS(4,600))

!   If (ImpressionSedim) write(UniteListing,2001) FichierSedim%Nom

    call LecFichierSedim ( FichierSedim, &
                           NbCouches   , &
                           CoucheSed   , &
                           Talus       , &
                           LimiteSable , &
                           CnuxV       , &
                           CnuxS       , &
!                           ConsConv    , &
                           Erreur      )


    If (Erreur%numero /= 0) return


  ! SAISIE AU CLAVIER => ENREGISTREMENT A PARTIR DES MOTS-CLES SAUVEGARDEES PAR L'IHM
  ! ---------------------------------------------------------------------------------
  ElseIf (ModeParamSedim == SAISIE_PAR_CLAVIER) Then

    If (ImpressionSedim) Then
      !write(UniteListing,2002)
      !write(UniteListing,2003)
    Endif

    ! Nombre de couches
    !------------------
    NbCouches = MOTINT(ADRESS(1,601))
    If (NbCouches < 1) Then
      Erreur%Numero = 408
      Erreur%ft   = err_408
      Erreur%ft_c = err_408c
      call TRAITER_ERREUR (Erreur, 'Le nombre de couches', 'au moins egal a 1')
      return
    ElseIf(NbCouches > 6) Then
      Erreur%Numero = 408
      Erreur%ft   = err_408
      Erreur%ft_c = err_408c
      call TRAITER_ERREUR (Erreur, 'Le nombre de couches', ' au maximum de 6')
      return
    Endif

    ! Allocation de memoire du tableau des couches de sediments
    !----------------------------------------------------------
    allocate (CoucheSed(NbCouches), STAT = retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'CoucheSed (couches de sediments)')
      return
    Endif

    ! Boucle de definition des parametres de couches de sediments
    !------------------------------------------------------------------
    Do iCouche = 1, NbCouches
      CoucheSed(iCouche)%Nom    = MOTCAR(ADRESS(4,601)+iCouche-1)(1:30)
      CoucheSed(iCouche)%Cfond  = MOTREA(ADRESS(2,601)+iCouche-1)
      CoucheSed(iCouche)%Psable = MOTREA(ADRESS(2,602)+iCouche-1)
      CoucheSed(iCouche)%D50    = MOTREA(ADRESS(2,603)+iCouche-1)
      CoucheSed(iCouche)%Wc     = MOTREA(ADRESS(2,604)+iCouche-1)
      CoucheSed(iCouche)%TauCE  = MOTREA(ADRESS(2,605)+iCouche-1)
      CoucheSed(iCouche)%MPart  = MOTREA(ADRESS(2,606)+iCouche-1)
      CoucheSed(iCouche)%Kp     = MOTREA(ADRESS(2,607)+iCouche-1)
      CoucheSed(iCouche)%Kt     = MOTREA(ADRESS(2,608)+iCouche-1)
      CoucheSed(iCouche)%TauCD  = 0.    ! nul sauf pour iCouche = 1 (cf. dessous)
    Enddo

    ! Vitesse de chute des vases (Couche No. 1)
    !------------------------------------------
    CoucheSed(1)%Wc    = MOTREA(ADRESS(2,609))
    CoucheSed(1)%TauCD = MOTREA(ADRESS(2,610))

    ! Pourcentage critique de sable
    !------------------------------
    LimiteSable   = MOTREA(ADRESS(2,611))

    ! MS2018 Clipping evolution par un pourcentage de la hauteur d'eau
    FracH = MOTREA(ADRESS(2,662))
    WRITE(*,*)
    WRITE(*,"(A,E10.4)") " Clip Evolution = ", FracH

    ! MS2018 option de pente locale
    optionPente = MOTLOG(ADRESS(3,663))
    WRITE(*,*)
    WRITE(*,"(A,L1)") " Pente locale : ", optionPente

    ! MS2018 diametre moyen, d84 et d16 pour lefort et recking
    warning = 0
    d84 = MOTREA(ADRESS(2,664))
    dm  = MOTREA(ADRESS(2,665))
    d16 = MOTREA(ADRESS(2,666))

    IF (d84 == 0.0) THEN
      d84 = 2.1 * CoucheSed(1)%D50
      warning = 1
    ENDIF

    IF (dm == 0.0) THEN
      dm = 1.1 * CoucheSed(1)%D50
      warning = 1
    ENDIF

    IF (d16 == 0.0) THEN
      d16 = 0.5 * CoucheSed(1)%D50
      warning = 1
    ENDIF

    qstar_option = MOTLOG(ADRESS(3,675))
    porosite = MOTREA(ADRESS(2,673))
    without_voids = MOTLOG(ADRESS(3,674))
    debug_bedload = MOTLOG(ADRESS(3,676))
    equilibrium_slope_option = MOTLOG(ADRESS(3,677))
    equilibrium_slope = MOTREA(ADRESS(2,678))

    strickler_slope_option = MOTLOG(ADRESS(3,663))
    round_slope_option = MOTLOG(ADRESS(3,679))
    round_precision = MOTINT(ADRESS(1,680))

    WRITE(*,*)

    IF (warning == 1) THEN
      WRITE(*,*)"Attention, diametres nuls ou non renseignes dans ", &
                "le fichier cas"
      WRITE(*,*)"Utilisation de formules a partir du d50 pour ",     &
                "determiner les diametres ci-dessous :"
      WRITE(*,*)
    ENDIF

    WRITE(*,*)"Parametres utilises seulement pour les formules de",  &
              " transport de Recking et de Lefort"
    WRITE(*,"(A,E10.4)")" D84 = ", d84
    WRITE(*,"(A,E10.4)")" Diametre moyen = ", dm
    WRITE(*,*)
    WRITE(*,*)"Parametre utilise seulement pour la formule de ",     &
              "transport de Lefort"
    WRITE(*,"(A,E10.4)")" D16 = ", d16
    WRITE(*,*)

    bedload_option = MOTLOG(ADRESS(3,667))

    WRITE(*,*)
    WRITE(*,"(A,L1)")" Bedload option : ", bedload_option

    suspension_option = MOTLOG(ADRESS(3,668))
    WRITE(*,*)
    WRITE(*,"(A,L1)")" Suspension option : ", suspension_option

    sediment_slide_option = MOTLOG(ADRESS(3,669))
    WRITE(*,*)
    WRITE(*,"(A,L1)")" sediment slide option : ", sediment_slide_option

    clipping_option = MOTLOG(ADRESS(3,670))
    WRITE(*,*)
    WRITE(*,"(A,L1)")" planim clipping option : ", clipping_option

    absolute_clip = MOTREA(ADRESS(2,671))
    IF (clipping_option .EQV. .FALSE.) then
      WRITE(*,*)
      WRITE(*,"(A,E10.4)")" absolute clipping = ", absolute_clip
    ENDIF

    bedload_transport_law = MOTINT(ADRESS(1,672))
    write(*,*) bedload_transport_law
    IF (bedload_transport_law .EQ. 1) then
      WRITE(*,*)
      WRITE(*,"(A)")" bedload transport law is Meyer-Peter-Mueller "
    ELSEIF (bedload_transport_law .EQ. 2) then
      WRITE(*,*)
      WRITE(*,"(A)")" bedload transport law is Lefort 2014 "
    ELSEIF (bedload_transport_law .EQ. 3) then
      WRITE(*,*)
      WRITE(*,"(A)")" bedload transport law is Recking 2013 "
    ELSEIF (bedload_transport_law .EQ. 4) then
      WRITE(*,*)
      WRITE(*,"(A)")" bedload transport law is Recking 2015 "
    ENDIF

    recking_morpho = MOTINT(ADRESS(1,681))
    recking_q_star_option = MOTLOG(ADRESS(3,682))


    !coefficient de diffusion
    !------------------------
    CnuxV    = MOTREA(ADRESS(2,618))
    CnuxS    = MOTREA(ADRESS(2,652))

    !parametres pour le schema de convection
    !---------------------------------------
!    nbtrac=1  ! PU2017 : Mise en commentaire
!    allocate (ConsConv , STAT = retour)

    ib=1 !,nbtrac

    ConsConv%Conv                 = MOTLOG(ADRESS(3,701)+ib-1)
    ConsConv%Scheconv             = MOTINT(ADRESS(1,701))
!    ConsConv(ib)%DIFF             = MOTLOG (ADRESS(3,702)+ib-1)
!    ConsConv(ib)%OptionCalculDisp = MOTINT (ADRESS(1,702))
!    ConsConv(ib)%CoefDiffu(1)     = MOTREA (ADRESS(2,702))
!    ConsConv(ib)%CoefDiffu(2)     = MOTREA (ADRESS(2,703))
    ConsConv%OrdreVF              = MOTINT(ADRESS(1,840))
    ConsConv%ParamW               = MOTREA(ADRESS(2,701))
    ConsConv%LimiteurPente        = MOTLOG(ADRESS(3,703))

    if (ConsConv%Scheconv > 4) Then
      Erreur%Numero = 508
      Erreur%ft   = err_508
      Erreur%ft_c = err_508c
      call TRAITER_ERREUR  (Erreur, 'Schema de convection', '< ou egal a 4')
      return
    endif

    if (ConsConv%OrdreVF > 3) Then
      Erreur%Numero = 508
      Erreur%ft   = err_508
      Erreur%ft_c = err_508c
      call TRAITER_ERREUR  (Erreur, 'Ordre du schema VF', '< ou egal a 3')
      return
    endif

    if ((ConsConv%ParamW < -1).or.(ConsConv%ParamW > 1)) Then
      Erreur%Numero = 508
      Erreur%ft   = err_508
      Erreur%ft_c = err_508c
      call TRAITER_ERREUR  (Erreur, 'Parametre W', 'compris entre -1 et 1')
      return
    endif

    ! Definition des talus
    !---------------------
    Talus%Modele  = MOTINT(ADRESS(1,602))
    Talus%PstabI  = MOTREA(ADRESS(2,612))
    Talus%PstabE  = MOTREA(ADRESS(2,613))

    ! Allocation de memoire du tableau des talus
    !-------------------------------------------
    allocate (Talus%Gamma(NbCouches),STAT = retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'Talus%Gamma(poids volum. des sed.)')
      return
    Endif

    ! Initialisation avant lecture
    Talus%Lambda   = -1.
    Talus%ResRed   = -1.
    Talus%GammaW   = -1.
    Talus%Gamma(:) = -1.

    ! Lecture des parametres necessaires a l'utilisation du modele de mecanique des sols
    !-----------------------------------------------------------------------------------
    If (Talus%Modele == MODELE_TALUS_GLISSEMENT) Then

      ValSum = 0.

      ! Lecture des parametres
      Talus%Lambda = MOTREA(ADRESS(2,614))
      Talus%ResRed = MOTREA(ADRESS(2,615))
      Talus%GammaW = MOTREA(ADRESS(2,616))
      Do iCouche = 1, NbCouches
        Talus%Gamma(iCouche) = MOTREA(ADRESS(2,617)+iCouche-1)
        ValSum = Talus%Gamma(iCouche) + ValSum
      Enddo

      ! Message d'erreur si ces parametres sont absents du fichier des mots-cle
      ValSum = Talus%Lambda + Talus%ResREd + Talus%GammaW + ValSum
      If (ValSum <= -1.) Then
        Erreur%Numero   = 409
        Erreur%ft   = err_409
        Erreur%ft_c = err_409c
        call TRAITER_ERREUR (Erreur, 'Parametres de meca. sols ', &
                             'absents du fichier des mots-cle.')
        return
      Endif

    Else

      Talus%Lambda = 0.
      Talus%ResRed = 0.
      Talus%GammaW = 0.
      Do iCouche = 1, NbCouches
        Talus%Gamma(iCouche) = 0.
      Enddo

    Endif

  ! TRAITEMENT DES ERREURS SUR LE MODE DE SAISIE
  ! --------------------------------------------
  Else

    Erreur%Numero = 410
    Erreur%ft   = err_410
    Erreur%ft_c = err_410c
    call TRAITER_ERREUR (Erreur, 'les parametres sedimentaires')
    return

  ! FIN CHOIX MODE DE SAISIE
  ! ----------------------
  End if ModeSaisie

!============================================================================
! VERIFICATION SUR LE POURCENTAGE DE SABLE DES PREMIERES COUCHES DE SEDIMENTS
!============================================================================
  If (suspension_option) Then
    If (CoucheSed(1)%Psable /= 0.) Then
      Erreur%ft   = err_408
      Erreur%ft_c = err_408c
      call TRAITER_ERREUR (Erreur, 'Le % de sable de la couche 1 ', &
                                   '(depot des vases) egal a 0.')
      return
    ElseIf (CoucheSed(2)%Psable /= 100.) Then
      Erreur%Numero = 408
      Erreur%ft   = err_408
      Erreur%ft_c = err_408c
      call TRAITER_ERREUR (Erreur, 'Le % de sable de la couche 2 ', &
                                   '(depot des sables) egal a 100.')
      return
    Endif
  Endif

!=========================================================================
! IMPRESSION DES RESULTATS DE LECTURE
!=========================================================================

  If (ImpressionSedim) Then

!    write(UniteListing,2004) 'No.',    &
!                             'Nom',    &
!                             'Cf',     &
!                             'Psable', &
!                             'D50',    &
!                             'Wc',     &
!                             'TauCE',  &
!                             'MPart',  &
!                             'Kp'

!    Do iCouche = 1, NbCouches
!      write(UniteListing,2005) iCouche  ,                  &
!                               CoucheSed(iCouche)%Nom  ,   &
!                               CoucheSed(iCouche)%Cfond ,  &
!                               CoucheSed(iCouche)%Psable , &
!                               CoucheSed(iCouche)%D50  ,   &
!                               CoucheSed(iCouche)%Wc  ,    &
!                               CoucheSed(iCouche)%TauCE ,  &
!                               CoucheSed(iCouche)%MPart ,  &
!                               CoucheSed(iCouche)%Kp
!    Enddo

!    If (iCouche == 1 ) write(UniteListing,2006) CoucheSed(iCouche)%TauCD

!    write(UniteListing,2007) LimiteSable

!    write(UniteListing,2008) Talus%Modele, Talus%PstabI, Talus%PstabE
!    If (Talus%Modele == MODELE_TALUS_GLISSEMENT) Then
!      write(UniteListing,2009) Talus%Lambda
!      write(UniteListing,2010) Talus%ResRed
!      write(UniteListing,2011) Talus%GammaW
!      write(UniteListing,2012) 'No.','Gamma'
!      Do iCouche = 1, NbCouches
!        write(UniteListing,2013) iCouche,Talus%Gamma(iCouche)
!      Enddo
!    Endif

  Endif

!  Erreur%arbredappel = arbredappel_old

  return

!=========================================================================
! SERIE DE FORMATS
!=========================================================================

!=========================================================================
! FIN DU SOUS-PROGRAMME
!=========================================================================
End Subroutine LecParamSedim
