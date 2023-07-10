Module M_LecConcIni_I

Interface

Subroutine  LecConcIni  ( &

  UniteListing       , & ! Unite du fichier d'impression des parametres
  ImpressionConcIni  , & ! Choix d'impression des concentrations initiales
  FicCMESIni         , & ! Fichier des concentrations de sable et vase
! presentes initialement dans la retenue
  Abscisse           , & ! Abscisse curviligne des sections de calcul
  NbProfCourlis      , & ! Nombre de sections de calcul
  OptionCourlis      , & ! commutateur : VRAI si calcul COURLIS de sediments
  Traceurs0          , & ! Concentr. initiale des traceurs
! Lecture des mots du dictionnaires
  MOTINT             , &
  MOTREA             , &
  MOTCAR             , &
  ADRESS             , &
! Traitement des erreurs
  Erreur             )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture des concentrations des matieres en suspension
!  --------    initialement presentes dans le bief
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele : LecFicConcIni
!  ---------------------
!
!=========================================================================

use M_PRECISION            ! Definition de la precision DOUBLE ou SIMPLE
use M_FICHIER_T            ! Definition du type FICHIER_T
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info, mode de saisie

use M_ERREUR_T             ! Type ERREUR_T
use M_MESSAGE_C            ! Messages d'erreur
use M_TRAITER_ERREUR_I     ! Traitement de l'errreur

use M_INTERPOLATION_S      ! Sous-programme d'interpolation

use M_LecFicConcIni_I      ! Module d'interface

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(FICHIER_T)                   , intent(inout) :: FicCMESIni
  real(DOUBLE)      , dimension(:)  , pointer       :: Abscisse
  logical                           , intent(in   ) :: OptionCourlis
  integer                           , intent(in   ) :: NbProfCourlis

  logical                           , intent(in   ) :: ImpressionConcIni
  integer                           , intent(in   ) :: UniteListing

  integer           , dimension(:)  , intent(in   ) :: MOTINT
  real(DOUBLE)      , dimension(:)  , intent(in   ) :: MOTREA
  character(LEN=144), dimension(:)  , intent(in   ) :: MOTCAR
  integer           , dimension(:,:), intent(in   ) :: ADRESS

! Variables de sortie
  real(DOUBLE)    , dimension(:,:)  , pointer    :: Traceurs0

! Variables locales
  integer                :: ModeCMESIni   ! Mode d'entree de la concentration initiale de vase
  integer                :: NbPtsConcIni  ! Nb pts position des traceurs
  integer                :: iTraceur      ! Compteur de traceurs
  integer                :: iPts          ! Compteur de points
  integer                :: NbTraceurs

  real(DOUBLE), dimension(:)  , pointer :: AbscIni     ! Abscisse curviligne des valeurs de concentration initiale
  real(DOUBLE), dimension(:,:), pointer :: Traceurs    ! Nombre de points devrivant les concentrations initiales

! Traitement des erreurs
  integer                       :: retour      ! Code de retour Read
  type(ERREUR_T), intent(inout) :: Erreur
!  character(132)        :: arbredappel_old  ! Arbre d'appel initial  ! PU2017 : Mise en commentaire

!=========================================================================

End Subroutine LecConcIni

End Interface

End Module M_LecConcIni_I

