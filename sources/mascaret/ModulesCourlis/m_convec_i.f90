Module M_Convec_I

Interface

Subroutine Convec  (  &

    T2              ,  & ! Grandeur convectee (Sm.Conc) a t+dt
    Absc            ,  & ! Abscisse des sections de calcul (ProfilCourlis%Abs)
    Conc            ,  & ! Concentration en suspension a t
    CL_amont        ,  & ! Concentration a l'amont a t+dt
    Sm0             ,  & ! Surface mouillee a t
    Sm1             ,  & ! Surface mouillee a t+dt
    V1              ,  & ! Vitesse a t+dt
    Dt              ,  & ! Pas de temps
    ConsConv        ,  & ! constantes de la convection
    Erreur          )    ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER - M. Jodeau
!
!  VERSION : 5.1       008-2009    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Calcul de la convection de la concentration
!  --------
!
!  Sous-programme appelant : DansLo
!  -----------------------
!
!  Sous-programmes appeles : Hyp1fa  : meth. caract. conv.faible ici cons.
!  ---------------------     Godunov : volumes finis ordre 1
!                            Muscl_hancock : volumes finis ordre 2 & 3
!
!=========================================================================

use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C         ! Definition des constante tq EPS*, W0, ...
use M_CONSTANTES_TRACER_T !

use M_HYP1FA_I
use M_GODUNOV_I
use M_MUSCL_HANCOCK_I

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE) :: Dt

  real(DOUBLE), dimension(:), intent(in   ) :: Conc
  real(DOUBLE), dimension(:), intent(in   ) :: Sm0, Sm1
  real(DOUBLE), dimension(:), intent(in   ) :: Absc

  real(DOUBLE)              , intent(in   ) :: CL_amont

  real(DOUBLE), dimension(:) , intent(inout)::  V1

! Variables de sortie
  real(DOUBLE), dimension(:), intent(  out) :: T2

! Variables locales
  real(DOUBLE), dimension(:), allocatable   :: T1       ! Grandeur convectee (Sm.Conc) a t
  real(DOUBLE), dimension(:), allocatable   :: Nul      ! Vecteur nul
  real(DOUBLE)                              :: Umax
  integer                                   :: i        ! compteur

  real(DOUBLE)  :: CG, CD                               ! Conditions limites a t+dt a gauche et a droite
  real (DOUBLE) :: FLUENT, FLUSOR

  integer :: IM                                         ! dimension du systeme

! Parametres pour la convection

  type (CONSTANTES_TRACER_T), intent(inout) :: ConsConv

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  integer                       :: retour               ! Code de retour de la fonction read, allocate
!  character(132) :: arbredappel_old                     ! Ancien arbre d'appel  ! PU2017 : Mise en commentaire

!..Intrinsic Functions..
  intrinsic ABS


!=========================================================================

End Subroutine Convec

End Interface

End Module M_Convec_I
