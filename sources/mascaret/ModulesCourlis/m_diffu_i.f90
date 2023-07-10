Module M_Diffu_I

Interface

Subroutine Diffu  (  &

 Conc   ,  & ! Concentration a t+Dt
 T2     ,  & ! Grandeur convectee (SmC) a t+Dt
 Flux   ,  & ! Flux lineaire de MES (kg/s/m)
 Sm     ,  & ! Surface mouillee a t+DT
 Vit    ,  & ! Vitesse a t+dt
 Absc   ,  & ! Abscisse des sections de calcul
 Dt     ,  & ! Pas de temps
 Cnux   ,  & ! Coefficient de diffusion
 Erreur )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Calcule de la diffusion de la concentration
!  --------
!
!  Sous-programme appelant : DansLo
!  -----------------------
!
!  Sous-programme appele :  BISSDL
!  ---------------------
!
!=========================================================================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C      ! Definition des constante tq EPS*, W0, ...

use M_ERREUR_T         ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I ! Traitement de l'errreur

use M_BISSDL_I

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE), intent(in) :: Dt
  real(DOUBLE), intent(in) :: Cnux

  real(DOUBLE), dimension(:), intent(in   ) :: T2
  real(DOUBLE), dimension(:), intent(in   ) :: Flux
  real(DOUBLE), dimension(:), intent(in   ) :: Sm
  real(DOUBLE), dimension(:), intent(in   ) :: Vit
  real(DOUBLE), dimension(:), intent(in   ) :: Absc


! Variables de sortie
  real(DOUBLE), dimension(:), intent(  out) :: Conc

! Variables locales
  integer :: NbProf  ! Nombre de sections de calcul
  integer :: i       ! Compteur

  real(DOUBLE), dimension(:), allocatable :: AAA, BBB, CCC, DDD ! coefficients de la matrice
  real(DOUBLE), dimension(:), allocatable :: A, B, C, D         ! coefficients de la matrice

  real(DOUBLE) :: Alpha            ! Variable de calcul
  real(DOUBLE) :: Ai, APi          ! Variable de calcul
  real(DOUBLE) :: Hi, HiP1, HHi    ! Variable de calcul
  real(DOUBLE) :: Cst1, Cst2, Cst3 ! Variable de calcul

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  integer                       :: retour ! Code de retour de la fonction read, allocate
  character(132)                :: arbredappel_old   ! Ancien arbre d'appel

!=========================================================================

End Subroutine Diffu

End Interface

End Module M_Diffu_I
