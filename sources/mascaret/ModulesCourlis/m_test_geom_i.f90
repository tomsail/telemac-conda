Module M_TestGeom_I

Interface

Subroutine  TestGeom  ( &

    NbInterfaces  , & ! Nb d'interfaces
    NbProf        , & ! Nombre de sections
    NbProfCourlis , & ! Nombre de sections des profils de Courlis
    Profil        , & ! Nombre de profils dans le champ "profil"
    ProfilCourlis , & ! Profils des interfaces de couche de sediments
    Erreur        )   ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!  ModifiÃ©e le 21/10/2003  par  Ch. Bertier
!          Suppression du doublement des points extremes des profils
!
!=========================================================================
!  Fonction : Verification de la correcte stratification des interfaces
!  --------   Verification de la coherence de la geometrie de COURLIS et
!              de celle de l'hydraulique
!             Verification de l'abscence de lit majeur
!
!  Sous-programme appelant : LecGeomCourlis
!  -----------------------
!
!  Sous-programme appele : TRAITER_ERREUR
!  ---------------------
!
!=========================================================================


!=========================================================================
! DECLARATIONS
!=========================================================================

use M_PRECISION
use M_FICHIER_T        ! Definition du type FICHIER_T
use M_PROFIL_COURLIS_T ! Definition du type PROFIL_COURLIS
use M_PROFIL_T         ! Definition du type PROFIL
use M_PARAMETRE_C      ! Parametres numeriques

use M_ERREUR_T         ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I ! Traitement de l'erreur

!.. Implicit Declarations ..
  implicit none


! Variables d'entrÃ©e
  integer            , intent(in   ) :: NbInterfaces
  integer            , intent(in   ) :: NbProf
  integer            , intent(in   ) :: NbProfCourlis
  type(PROFIL_T)    , dimension(:),pointer  :: Profil
  type(PROFIL_COURLIS_T), dimension(:),pointer  :: ProfilCourlis

! Variables locales
  integer          :: iProf            ! Compteur sur les profils
  integer          :: iPoint           ! Compteur sur les points d'un profil
  integer          :: iInterf          ! Compteur sur les interfaces de couches
  integer          :: NbPoints         ! Nombre de points d'un profil
  integer          :: NbPointsCourlis  ! Nombre de points d'un profil Courlis
!  character(30)    :: NomProf          ! Nom du profil  ! PU2017 : Mise en commentaire
!  character(30)    :: NomBief          ! Nom du bief  ! PU2017 : Mise en commentaire
  real(DOUBLE)     :: Abs              ! Abscisse relative du profil
!  real(DOUBLE)     :: CoteMax          ! Cote extreme maximum d'un profil  ! PU2017 : Mise en commentaire
!  logical          :: LimMin1,LimMin2  ! Tests de Limites inferieures  ! PU2017 : Mise en commentaire

! Traitement des erreurs
!  character(132)  :: arbredappel_old  ! Arbre d'appel initial  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!==========================================================================

End Subroutine TestGeom

End Interface

End Module M_TestGeom_I
