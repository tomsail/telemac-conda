Module M_LecGeomCourlis_I

Interface

Subroutine  LecGeomCourlis  ( &

    UniteListing    , & ! Unite logique fichier listing
    FicGeomCourlis  , & ! Fichier des profils definissant les interfaces
    ImpressionGeom  , & ! Choix d'impression de la geometrie des interfaces sed.
    NbInterfaces    , & ! Nb d'interfaces
    NbProf          , & ! Nombre de profils du tableau "profil"
    Profil          , & ! profils de Mascaret
    ProfilCourlis   , & ! Profils de la geometrie des rivieres, lus dans COURLIS
    MOTCAR          , &
    ADRESS          , &
    Erreur          )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!  Modifiee le 21/10/2003  par  Ch. Bertier
!          Suppression du doublement des points extremes des profils
!
!=========================================================================
!  Fonction : Lecture du fichier de geometrie comprenant la description
!  --------    des couches sedimentaires sous forme de profils en travers
!             Verification de la coherence des donnees
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele : - TestGeom
!  ---------------------
!=========================================================================
!  Commentaires : Description du fichier lu
!  ------------
! Toute ligne de commentaire est debutee par le caractere "#"
! Ligne 1 : mot-cle MOT_CLE, NomBief, NomProfil, abscisse profil.
!           sans blanc dans les noms : le blanc est le caractere separateur
!           Ex : <MOT_CLE bief1 profil1 340.3>
!
! Ligne 2 et jusqu'au prochain mot-cle MOT ou la fin de fichier :
!   (T, Z1, Z2, Z3, ...)
!   Exemple :
!     <  0. 2000.  2005. 2010. >
!     < 10. 3000.  3005. 3010. >
!     <100. 4000.  4005. 4010. >
!     etc.
!
! ATTENTION : Contrairement a l'hydraulique, le premier et le dernier point
!            des profils en travers ne sont pas doubles. Les profils Courlis
!            ont donc 2 points de moins que les profils hydrauliques.
!
!=========================================================================


!=========================== Declarations ================================

use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
use M_FICHIER_T
use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS
use M_PROFIL_T            ! Definition du type PROFIL
use M_LIRE_CHAINE_S       ! lecture de lignes de commentaire
use M_DECODER_GEOM_I      ! Interface de sous-programme
use M_PARAMETRE_C         ! Parametres
use M_TestGeom_I          ! Interface de sous-programme

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'erreur

!.. Implicit Declarations ..
  implicit none


! Variables d'entree
  type(FICHIER_T)     , intent(inout) :: FicGeomCourlis

  integer       , intent(in   ) :: UniteListing
  logical       , intent(in   ) :: ImpressionGeom
  integer       , intent(in   ) :: NbInterfaces
  integer       , intent(in   ) :: NbProf

  type(PROFIL_T)    , dimension(:)  , pointer       :: Profil
  character(LEN=144), dimension(:)  , intent(in   ) :: MOTCAR
  integer           , dimension(:,:), intent(in   ) :: ADRESS


  ! Variables de sortie
  type(PROFIL_COURLIS_T), dimension(:), pointer   :: ProfilCourlis


  ! Constantes
  integer, parameter :: LEN_CHAINE = 80


  ! Constantes sur les mots-cles
  integer, parameter :: NB_MOT_CLE = 3
  character(*),dimension(NB_MOT_CLE), parameter :: MOT_CLE = (/"PROFIL", &
                                                               "Profil", &
                                                               "profil"  /)
  character(1), parameter :: CHAINE_COMMENTAIRE = "#" ! Caractere commentaire qui
                                                      ! debute une ligne commentaire

  logical  :: PresenceMotCle ! Test de presence du mot cle
  integer  :: UniteGeom      ! Unite logique du fichier de geometrie

  integer  :: iProf          ! Compteur sur les profils
  integer  :: NbProfCourlis  ! Nb de profils lus dans le fichier de geom.

  integer       :: iPts      ! Compteur sur les points d'un profil
  integer       :: iInterf   ! Compteur d'interface
  integer       :: NbPoints  ! Nombre de points d'un profil
  character(30) :: NomProf   ! Nom du profil
  character(30) :: NomBief   ! Nom du bief
  real(DOUBLE)  :: Abs       ! Abscisse relative du profil
!  real(DOUBLE)  :: CoteMax   ! Cote extreme maximum d'un profil  ! PU2017 : Mise en commentaire
  real(DOUBLE)  :: z_ref     ! Point bas du lit

  character(LEN_CHAINE) :: chaine ! Chaine contenant une ligne du fichier

! Traitement des erreurs
  integer        :: retour      ! Code de retour de la fonction read, allocate
  integer        :: Retour_L    ! Code de retour a la 2ieme lecture du fichier
!  character(132) :: arbredappel_old ! Arbre d'appel initial  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================


End Subroutine LecGeomCourlis

End Interface

End Module M_LecGeomCourlis_I
