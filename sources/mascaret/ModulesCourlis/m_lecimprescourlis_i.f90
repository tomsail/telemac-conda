Module M_LecImpResCourlis_I

Interface

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

End Subroutine LecImpResCourlis

End Interface

End Module M_LecImpResCourlis_I
