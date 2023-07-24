Subroutine  LecImpResCourlis( &

  FichierMotCle         , & ! Fichier des mots-cle
  FicListingCourlis     , & ! Fichier de sortie listing des resultats
  FicStockPLongCourlis  , & ! Fichier graphique des res. selon profil en long du bief
  FicStockPTransCourlis , & ! Fichier graphique des res. selon profil en travers du b.
  FicControleCourlis    , & ! Ficher de controle
  FicErreurCourlis      , & ! Fichier d'erreur
  FicResuGeom           , & ! Fichier de la geometrie finale des couches
  FicResuMES            , & ! Fichier des concentrations en MES a fin calcul a ch. x
  ImpressionSedim       , & ! Choix Impression des parametres sedimentaires
  ImpressionGeom        , & ! Choix Impression de la geometrie des interfaces sedim.
  ImpressionCouplage    , & ! Choix Impression des parametres de couplage
  ImpressionConcIni     , & ! Choix Impression des Concentrations initiales
  ImpressionLoiConc     , & ! Choix Impression des Lois de concentration
  ImpressionApport      , & ! Choix Impression des apports Courlis
  PasImpressionCourlis  , & ! Periodicite des enregistrements listing
  PasStockLongCourlis   , & ! Periodicite des enregistremts graph. profil en long
  PasStockTransCourlis  , & ! Periodicite des enregistremts graph. profil en travers
  MOTINT                , &
  MOTCAR                , &
  MOTLOG                , &
  ADRESS                , &
  Erreur                )   ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture du des paramatres d'impression - resultat
!  --------
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================


!=========================================================================
! DECLARATIONS
!=========================================================================
use M_PRECISION
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info
use M_FICHIER_T            ! Definition du type FICHIER_T

use M_ERREUR_T             ! Type ERREUR_T
use M_MESSAGE_C            ! Messages d'erreur
use M_TRAITER_ERREUR_I     ! Traitement de l'erreur

!.. Implicit Declarations ..
  implicit none

! Variables d'entree et sortie
  type(FICHIER_T), intent(in   ) :: FichierMotCle
  type(FICHIER_T), intent(inout) :: FicListingCourlis
  type(FICHIER_T), intent(inout) :: FicStockPLongCourlis
  type(FICHIER_T), intent(inout) :: FicStockPTransCourlis
  type(FICHIER_T), intent(inout) :: FicControleCourlis
  type(FICHIER_T), intent(inout) :: FicErreurCourlis
  type(FICHIER_T), intent(inout) :: FicResuGeom
  type(FICHIER_T), intent(inout) :: FicResuMES

  integer           , dimension(:)  , intent(in   ) :: MOTINT
  character(LEN=144), dimension(:)  , intent(in   ) :: MOTCAR
  integer           , dimension(:,:), intent(in   ) :: ADRESS
  logical           , dimension(:)  , intent(in   ) :: MOTLOG

! Variables de sortie
  logical    , intent(  out) :: ImpressionSedim
  logical    , intent(  out) :: ImpressionGeom
  logical    , intent(  out) :: ImpressionCouplage
  logical    , intent(  out) :: ImpressionConcIni
  logical    , intent(  out) :: ImpressionLoiConc
  logical    , intent(  out) :: ImpressionApport
  integer    , intent(  out) :: PasImpressionCourlis
  integer    , intent(  out) :: PasStockLongCourlis
  integer    , intent(  out) :: PasStockTransCourlis

! Variables locales
  integer        :: IdxPoint        ! Index du caractere '.'
  integer        :: UniteListing    ! Unite du fichier de Listing

! Traitement des erreurs
  integer     :: retour      ! Code de retour de la fonction read
!  character(132) :: arbredappel_old  ! Arbre d'appel initial  ! PU2017 : Mis en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================
! INITIALISATION
!=========================================================================

  Erreur%Numero = 0
!  arbredappel_old = trim(Erreur%arbredappel)  ! PU2017 : Mis en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LecLoiConc'

!=========================================================================
! FICHIERS DE PARAMETRAGE
!=========================================================================

! Fichier Listing des resultats
  FicListingCourlis%Nom      = MOTCAR(ADRESS(4,604))

! Fichiers de sortie graphique des resultats
  FicStockPLongCourlis%Nom   = MOTCAR(ADRESS(4,605))
  FicStockPTransCourlis%Nom  = MOTCAR(ADRESS(4,606))

! Recherche de l'index du caractere 'x'
  IdxPoint = 0
  IdxPoint = index(FichierMotCle%Nom,'.',.true.)

! Fichier de controle et fichier d'erreur
  If (IdxPoint == 0) Then
    ! Si le nom du fichier n'a pas de suffixe, ajouter 1 suffixe a la suite
    write(FicControleCourlis%Nom,'(2a)') FichierMotCle%Nom,'_Courlis.controle'
    write(FicErreurCourlis%Nom,'(2a)')   FichierMotCle%Nom,'_Courlis.erreur'
  Else
    ! Sinon, enlevez le suffixe et le remplacer
    write(FicControleCourlis%Nom,'(2a)') FichierMotCle%Nom(1:IdxPoint-1) , &
                                         '_Courlis.controle'

    write(FicErreurCourlis%Nom,'(2a)')   FichierMotCle%Nom(1:IdxPoint-1) , &
                                         '_Courlis.erreur'
  End if

! Fic de geometrie finale des couches et fichier des concentration en CMES
  If (IdxPoint == 0) Then
    ! Si le nom du fichier n'a pas de suffixe, ajouter 1 suffixe a la suite
    write(FicResuGeom%Nom,'(2a)') FichierMotCle%Nom,'_Geom.resu'
    write(FicResuMES%Nom,'(2a)')  FichierMotCle%Nom,'_CMES.resu'
  Else
    ! Sinon, enlevez le suffixe et le remplacer
    write(FicResuGeom%Nom,'(2a)') FichierMotCle%Nom(1:IdxPoint-1),'_Geom.resu'
    write(FicResuMES%Nom,'(2a)')  FichierMotCle%Nom(1:IdxPoint-1),'_CMES.resu'
  End if


!=========================================================================
! OUVERTURE DU FICHIER D'IMPRESSION DES COMMENTAIRES ET DE POST-TRAITEMENT
!=========================================================================
  UniteListing = FicListingCourlis%Unite

  open(unit=UniteListing, file=FicListingCourlis%Nom, access='SEQUENTIAL', &
       action='WRITE'   , form='UNFORMATTED'      , iostat=retour, &
       position='rewind', status='REPLACE'      )

  If (retour /= 0) Then
    Erreur%Numero = 4
    Erreur%ft   = err_4
    Erreur%ft_c = err_4c
    call TRAITER_ERREUR (Erreur, trim(FicListingCourlis%Nom))
    return
  Endif

! Impression du titre du fichier
!  write(UniteListing,1000)

  open(unit=FicStockPLongCourlis%Unite, file=FicStockPLongCourlis%Nom, &
       access='SEQUENTIAL', action='WRITE', form='FORMATTED',       &
       iostat=RETOUR, position='rewind', status='REPLACE')

  If (retour /= 0) Then
    Erreur%Numero = 4
    Erreur%ft   = err_4
    Erreur%ft_c = err_4c
    call TRAITER_ERREUR (Erreur, trim(FicStockPLongCourlis%Nom))
    return
  Endif

  open(unit=FicStockPTransCourlis%Unite, file=FicStockPTransCourlis%Nom, &
       access='SEQUENTIAL', action='WRITE', form='FORMATTED',       &
       iostat=RETOUR, position='rewind', status='REPLACE')

  If (retour /= 0) Then
    Erreur%Numero = 4
    Erreur%ft   = err_4
    Erreur%ft_c = err_4c
    call TRAITER_ERREUR (Erreur, trim(FicStockPTransCourlis%Nom))
    return
  Endif

  open(unit=FicResuGeom%Unite, file=FicResuGeom%Nom,       &
       access='SEQUENTIAL', action='WRITE', form='FORMATTED', &
       iostat=RETOUR, position='rewind', status='REPLACE')

  If (retour /= 0) Then
    Erreur%Numero = 4
    Erreur%ft   = err_4
    Erreur%ft_c = err_4c
    call TRAITER_ERREUR  (Erreur, trim(FicResuGeom%Nom))
    return
  Endif

  open(unit=FicResuMES%Unite, file=FicResuMES%Nom,         &
       access='SEQUENTIAL', action='WRITE', form='FORMATTED', &
       iostat=RETOUR, position='rewind', status='REPLACE')

  If (retour /= 0) Then
    Erreur%Numero = 4
    Erreur%ft   = err_4
    Erreur%ft_c = err_4c
    call TRAITER_ERREUR  (Erreur, trim(FicResuMES%Nom))
    return
  Endif

!=========================================================================
! IMPRESSIONS - RESULTATS
!=========================================================================

!  write(UniteListing,1010)

! Impression des parametres de sediments
!---------------------------------------
  ImpressionSedim = MOTLOG(ADRESS(3,601))

!  If (ImpressionSedim) Then
!    write(UniteListing,1020) 'OUI'
!  Else
!    write(UniteListing,1020) 'NON'
!  Endif

! Impression de la geometrie des interfaces de sediments
!-------------------------------------------------------
  ImpressionGeom = MOTLOG(ADRESS(3,602))

!  If (ImpressionGeom) Then
!    write(UniteListing,1030) 'OUI'
!  Else
!    write(UniteListing,1030) 'NON'
!  Endif

! Impression des parametres de couplage
!--------------------------------------
  ImpressionCouplage = MOTLOG(ADRESS(3,603))

!  If (ImpressionCouplage) Then
!    write(UniteListing,1040) 'OUI'
!  Else
!    write(UniteListing,1040) 'NON'
!  Endif

! Impression des concentrations initiales
!----------------------------------------
  ImpressionConcIni = MOTLOG(ADRESS(3,604))

!  If (ImpressionConcIni) Then
!    write(UniteListing,1050) 'OUI'
!  Else
!    write(UniteListing,1050) 'NON'
!  Endif

! Impression des lois de concentration
!--------------------------------------
  ImpressionLoiConc = MOTLOG(ADRESS(3,605))

!  If (ImpressionLoiConc) Then
!    write(UniteListing,1060) 'OUI'
!  Else
!    write(UniteListing,1060) 'NON'
!  Endif

! Impression des parametres d'apport
!--------------------------------------
  ImpressionApport = MOTLOG(ADRESS(3,606))

!  If (ImpressionApport) Then
!    write(UniteListing,1070) 'OUI'
!  Else
!    write(UniteListing,1070) 'NON'
!  Endif

!=========================================================================
! DEFINITION DU PAS D'IMPRESSION
!=========================================================================

  PasImpressionCourlis = MOTINT(ADRESS(1,606))
  PasStockLongCourlis  = MOTINT(ADRESS(1,607))
  PasStockTransCourlis = MOTINT(ADRESS(1,608))

!  Erreur%arbredappel = arbredappel_old

!=========================================================================
! FORMATS
!=========================================================================

!=========================================================================
! FIN DU SOUS-PROGRAMME
!=========================================================================
End Subroutine LecImpResCourlis
