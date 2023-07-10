Module M_ResIniTalus_I

Interface

Subroutine ResIniTalus  (  &

  ProfilCourlis    ,  & ! Profils sedimentaires
  Talus            ,  & ! Parametres relatifs aux talus
  CoucheSed        ,  & ! Parametres sedimentaires des differentes couches
  Resini           ,  & ! Resistance initiale des blocs au mouvement
  Erreur        )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Calcule de la resistance initiale des colonnes sedimentaires
!  --------    au mouvements
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_PRECISION            ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C          ! Definition des constante tq EPS*, W0, ...
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info

use M_PROFIL_COURLIS_T     ! Definition du type PROFIL_COURLIS
use M_COUCHE_T             ! Definition du type COUCHE_T
use M_TALUS_T              ! Definition du type TALUS_T

use M_ERREUR_T             ! Type ERREUR_T
use M_MESSAGE_C            ! Messages d'erreur
use M_TRAITER_ERREUR_I     ! Traitement de l'erreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(PROFIL_COURLIS_T), dimension(:)  , intent(in   )  :: ProfilCourlis
  type(COUCHE_T)        , dimension(:)  , intent(in   )  :: CoucheSed
  type(TALUS_T)                         , intent(in   )  :: Talus

! Variables de sortie
  real(DOUBLE)          , dimension(:,:), intent(  out)  :: ResIni

! Variables locales
  integer :: i, j         ! Compteurs  ! PU2017 : Mise en commentaire de k
  integer :: NProfil      ! Nombre de profils
  integer :: NInt         ! Nombre d'interfaces sedimentaires
  integer :: NPt          ! Nombre de points decrivant le profil en travers

  real(DOUBLE) :: HsG, HsD, HsM           ! Hauteurs de la colonne sedimentaire (gauche, droite, moyenne)
!  real(DOUBLE) :: HsGC, HsDC, HsMC        ! Hauteurs de la couche en cours  ! PU2017 : Mise en commentaire
  real(DOUBLE) :: Dx                      ! Pas de discretisation horizontal
!  real(DOUBLE) :: DzS, DxzS               ! Pas de discretisation vertical, hypothenuse a la surface du bloc  ! PU2017 : Mise en commentaire
  real(DOUBLE) :: DzF, DxzF               ! Pas de discretisation vertical, hypothenuse du fond du bloc
!  real(DOUBLE) :: SinS, CosS, SinF, CosF  ! Angles du bloc  ! PU2017 : Mise en commentaire

  real(DOUBLE), dimension(:,:), allocatable :: Z      ! Cotes des interfaces sedimentaires

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  integer                       :: retour  ! Code de retour de la fonction read, allocate
!  character(132) :: arbredappel_old      ! Ancien arbre d'appel  ! PU2017 : Mise en commentaire

! Constantes

!=========================================================================


End Subroutine ResIniTalus

End Interface

End Module M_ResIniTalus_I



