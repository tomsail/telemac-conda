Module M_SurfaceLibre_I

Interface

Subroutine  SurfaceLibre( &

    NbProfil            , & ! Nombre de profils du tableau "ProfilCourlis"
    PtRiveG             , & ! ABSCISSE DU POINT DE LA DROITE A LA HAUTEUR Zsurf
    PtRiveD             , & ! ABSCISSE DU POINT DE LA DROITE A LA HAUTEUR Zsurf
    ProfilCourlis       , & ! Profils geom. des rivieres, lus dans COURLIS
    Zsurf               , & ! Cote de la surface libre
    Erreur              )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       05/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Determine les points de rencontre RG et RD des berges et
!  --------    de la surface libre pour chaque profil en travers
!
!  Sous-programme appelant : StockPTransCourlis
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE

use M_PROFIL_COURLIS_T ! Definition du type PROFIL_COURLIS

use M_ERREUR_T         ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I ! Traitement de l'errreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(PROFIL_COURLIS_T), dimension(:), intent(in) :: ProfilCourlis
  real(DOUBLE)          , dimension(:), intent(in) :: Zsurf
  integer                             , intent(in) :: NbProfil

! Variables de sortie
  real(DOUBLE)          , dimension(:), pointer    :: PtRiveG, PtRiveD

! Variables locales
  real(DOUBLE) :: EpsI = 0.1   ! Valeur de tolerance
  real(DOUBLE) :: X1, X2, Z1, Z2
  real(DOUBLE) :: AErod, BErod
!  real(DOUBLE) :: NbPts        ! Nb de points du profil 'i'
  integer      :: NbPts        ! Nb de points du profil 'i'  ! PU2017 : Changement de format
  integer      :: i,j

! Traitement des erreurs
  integer                         :: Retour
  type(ERREUR_T)  , intent(inout) :: Erreur
!  character(132)                  :: arbredappel_old  ! PU2017 : Mise en commentaire

!=========================================================================

End Subroutine SurfaceLibre

End Interface

End Module M_SurfaceLibre_I
