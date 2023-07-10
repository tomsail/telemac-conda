Module M_LecFicConcIni_I

Interface

Subroutine  LecFicConcIni( &

    FicCMESIni     , & ! Fichier des concentrations de sable et vase presentes initialement dans le bief
    OptionCourlis  , & ! commutateur : VRAI si calcul COURLIS de sediments
    AbscIni        , & ! Abscisse curviligne ou sont donnees les valeurs de concentration initiale
    Traceurs       , & ! Concentr. initiale des traceurs
    Erreur         )   ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture du fichier contenant les concentrations initiales
!  --------    des MES (format OPTHYCA)
!
!  Sous-programme appelant : LecConcIni
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================
!   Commentaires :
!   ------------
!   !!!!!!!  CE SOUS-PROGRAMME POURRA ETRE UTILISE ULTERIEUREMENT POUR
!   !!!!!!!                   LA LECTURE DES TRACEURS.
!   !!!!!!!
!   !!!!!!!             IL RESTE CEPENDANT A PROGRAMMER
!   !!!!!!!            LES PARTIES SPECIFIQUES AUX TRACEURS.
!
!=========================================================================


!=========================================================================
!   DECLARATIONS
!=========================================================================

use M_PRECISION            ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info
use M_LIRE_CHAINE_S        ! Lecture de lignes de commentaire du fichier
use M_OPT2FORT_I           ! Interface de sous-programme
use M_FICHIER_T            ! Definition du type FICHIER_T

use M_ERREUR_T             ! Definition du type ERREUR_T
use M_MESSAGE_C            ! Messages d'erreur
use M_TRAITER_ERREUR_I     ! Traitement de l'errreur

!.. Implicit Declarations ..
  implicit none

! Constantes
  integer     , parameter :: LEN_CHAINE         = 80
  character(1), parameter :: CHAINE_COMMENTAIRE = "#" ! Caractere commentaire qui debute une ligne commentaire

! Variables d'entree
  type(FICHIER_T)   , intent(in) :: FicCMESIni
  logical           , intent(in) :: OptionCourlis

! Variables de sortie
  real(DOUBLE), dimension(:  ), pointer :: AbscIni
  real(DOUBLE), dimension(:,:), pointer :: Traceurs

! Variables locales
!  integer  :: ModeMESIni  ! Mode d'entree de la concentration initiale de vase  ! PU2017 : Mise en commentaire
  integer  :: NbPtsConcIni ! Nombre de points devrivant les concentrations initiales
  integer  :: NbTraceurs   ! Nombre de traceurs
  integer  :: iPts,i       ! Compteur de points
  integer  :: UniteConc    ! Unite du fichier de sedimentation a lire
  integer  :: rang         ! position du mot cle sur la ligne
  integer  :: ivar         ! compteur
  integer  :: PosAbs       ! position de l'abscisse curviligne
  integer  :: PosVase      ! position de la colonne de qmin
  integer  :: PosSable     ! position de la colonne de qmin
  integer  :: NbVarStocke  ! nombre de variables stockees

  real(DOUBLE), dimension(:), allocatable :: var

  character(LEN_CHAINE) :: chaine      ! Chaine contenant une ligne du fichier
  character(LEN_CHAINE) :: chaine_opt  ! chaine au format opthyca
  character(LEN_CHAINE) :: chaine_fort ! chaine convertie au format fortran


! Traitement des erreurs
  integer                       :: retour ! code de retour des fonctions d'e/s
!  character(132) :: arbredappel_old       ! ancien arbre  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================

End Subroutine LecFicConcIni

End Interface

End Module M_LecFicConcIni_I

