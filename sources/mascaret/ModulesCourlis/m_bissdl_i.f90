Module M_Bissdl_I

Interface

Subroutine Bissdl  (  &

    Z      ,  & ! Resultat de la diffusion
    A      ,  & ! ! Coefficient
    B      ,  & ! ! de la
    C      ,  & ! ! matrice de
    D      ,  & ! ! diffusion
    Imax   ,  & ! Nombre de point des tableaux
    Erreur )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       09/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Resolution d'un systeme tridiagonal
!  --------
!
!  Sous-programme appelant : Diffu
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_PRECISION         ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C       ! Definition des constante tq EPS*, W0, ...

use M_ERREUR_T          ! Type ERREUR_T
use M_MESSAGE_C         ! Messages d'erreur
use M_TRAITER_ERREUR_I  ! Traitement de l'errreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  integer :: Imax

  real(DOUBLE), dimension(:), intent(inout)  :: A, B, C, D

! Variables de sortie
  real(DOUBLE), dimension(:), intent(  out)  :: Z

! Variables locales
! PU2017 : Mise en commentaire des var loc
!  integer :: Im1    ! Nombre de sections de calcul - 1
!  integer :: i      ! Compteur

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  character(132)                :: arbredappel_old  ! Ancien arbre d'appel

!=========================================================================

End Subroutine Bissdl

End Interface

End Module M_Bissdl_I
