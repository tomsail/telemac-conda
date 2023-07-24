Module M_EcrFicGeom_I

Interface

Subroutine  EcrFicGeom  (  &

    FicResuGeom    ,  & ! Fichier de la geometrie finale
    NbProfil       ,  & ! nombre de profils
    ProfilCourlis  ,  & ! Profils sedimentaire
    NbInterface    ,  & ! nombre de d'interfaces sedimentaires
    TitreCas       ,  & ! Titre du cas de calcul
    Temps          ,  & ! Temps du calcul
    Erreur         )    ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Ecriture du fichier contenant des profils geometriques
!  --------    des differentes interfaces sedimentaires a la fin du calcul
!             Ce fichier sert de geometrie initiale dans le cas d'une
!              suite de calcul
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!=========================================================================
!  Commentaire : Description du fichier :
!  -----------
!   Ligne 1 :  MOT_CLE, NomBief, NomProfil, abscisse profil.
!        sans blanc dans les noms : le blanc est le caractere separateur
!        Ex : <MOT_CLE bief1 profil1 340.3>
!
!   Ligne 2 et jusqu'au prochain mot-cle MOT ou la fin de fichier :
!        (T, Z1, Z2, Z3, ...)
!        Exemple :
!        <  0. 2000.  2005. 2010. >
!        < 10. 3000.  3005. 3010. >
!        <100. 4000.  4005. 4010. >
!        etc.
!=========================================================================


!=========================================================================
!   DECLARATIONS
!=========================================================================

use M_PRECISION            ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info
use M_PARAMETRE_C          ! Definition des constante tq EPS*, W0, ...

use M_FICHIER_T            ! Definition du type FICHIER_T
use M_PROFIL_COURLIS_T     ! Definition du type PROFIL_COURLIS

use M_ERREUR_T             ! Definition du type ERREUR_T
use M_MESSAGE_C            ! Messages d'erreur
use M_TRAITER_ERREUR_I     ! Traitement de l'errreur

!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Constantes
  integer, parameter    :: LEN_CHAINE  = 80

! Variables d'entree
  type(FICHIER_T), intent(in   ) :: FicResuGeom

  integer, intent(in   ) :: NbProfil
  integer, intent(in   ) :: NbInterface

  type(PROFIL_COURLIS_T), dimension(:  ), intent(in   ) :: ProfilCourlis

  character(LEN_CHAINE), intent(in   ) :: TitreCas
  real(DOUBLE),          intent(in   ) :: Temps

! Variables de sortie

! Variables locales
  integer :: i,j,k       ! Compteurs
  integer :: Unite       ! Unite du fichier de sedimentation a ecrire

! Traitement des erreurs
  integer        :: retour       ! code de retour des fonctions d'e/s
!  character(132) :: arbredappel_old   ! ancien arbre  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================

End Subroutine EcrFicGeom

End Interface

End Module M_EcrFicGeom_I
