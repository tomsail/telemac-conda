Module M_LecCouplage_I

Interface

Subroutine  LecCouplage  ( &

  UniteListing        , & ! Unite du fichier d'impression des parametres
  ImpressionCouplage  , & ! Choix d'impression des parametres de couplage
  NbIterHydro         , & ! Nb d'iter. pour l'hydraulique entre 2 echanges
  NbIterSedim         , & ! Nb d'iter. pour la sedimento entre 2 echanges
! Lecture des mots du dictionnaires
  MOTINT              , &
  ADRESS              , &
! Traitement des erreurs
  Erreur              )   ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture parametre de couplage entre la sedimento et
!  --------    l'hydraulique
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

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE
use M_FICHIER_T        ! Definition du type FICHIER_T

use M_ERREUR_T         ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I ! Traitement de l'erreur

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  integer, dimension(:)  , intent(in   ) :: MOTINT
  integer, dimension(:,:), intent(in   ) :: ADRESS
  integer                , intent(in   ) :: UniteListing
  logical                , intent(in   ) :: ImpressionCouplage


! Variables de sortie
  integer         , intent(  out) :: NbIterHydro
  integer         , intent(  out) :: NbIterSedim

! Variables locales
!  integer :: IdxPoint      ! Index du caractere '.'  ! PU2017 : Mise en commentaire

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
!  character(132) :: arbredappel_old    ! Arbre d'appel initial  ! PU2017 : Mise en commentaire

!=========================================================================

End Subroutine LecCouplage

End Interface

End Module M_LecCouplage_I
