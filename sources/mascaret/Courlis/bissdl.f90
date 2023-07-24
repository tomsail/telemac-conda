Subroutine Bissdl (  &

    Z      , & ! Resultat de la diffusion
    A      , & ! ! Coefficient
    B      , & ! ! de la
    C      , & ! ! matrice de
    D      , & ! ! diffusion
    Imax   , & ! Nombre de point des tableaux
    Erreur )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       09/2003  Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction : Resolution d'un systeme tridiagonal
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

  real(DOUBLE), dimension(:), intent(inout) :: A, B, C, D

! Variables de sortie
  real(DOUBLE), dimension(:), intent(  out) :: Z

! Variables locales
  integer :: Im1  ! Nombre de sections de calcul - 1
  integer :: i    ! Compteur

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  character(132)                :: arbredappel_old ! Ancien arbre d'appel

!=========================================================================

!=========================================================================
! Initialisations
!=========================================================================

  Erreur%Numero = 0
  arbredappel_old    = trim(Erreur%arbredappel)
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>Bissdl'


  Im1 = Imax -1


!=========================================================================
! Resolution du systeme
!=========================================================================

  Do i = 2, Im1

    If (abs(B(i-1)) < EPS10) Then
      Erreur%Numero = 103
      Erreur%ft   = err_103
      Erreur%ft_c = err_103c
      call TRAITER_ERREUR (Erreur, B(i-1))
      return
    Endif

    B(i) = B(i) - A(i) * C(i-1) / B(i-1)
    D(i) = D(i) - A(i) * D(i-1) / B(i-1)

  Enddo

  If (abs(A(Imax) * C(Im1) - B(Im1) * B(Imax)) < EPS10) Then
    Erreur%Numero = 103
    Erreur%ft   = err_103
    Erreur%ft_c = err_103c
    call TRAITER_ERREUR (Erreur, A(Imax) * C(Im1) - B(Im1) * B(Imax))
    return
  Endif

  Z(Imax) = (A(Imax) * D(Im1) - B(Im1) * D(Imax)) / (A(Imax) * C(Im1) - B(Im1) * B(Imax))


  Do i = Im1, 1, -1

    If (abs(B(i)) < EPS10) Then
      Erreur%Numero = 103
      Erreur%ft   = err_103
      Erreur%ft_c = err_103c
      call TRAITER_ERREUR (Erreur, B(i))
      return
    Endif

    Z(i) = (D(i) - C(i) * Z(i+1)) / B(i)

  Enddo


!=========================================================================
! Fin du programme
!=========================================================================

!  Erreur%arbredappel = arbredappel_old


  return

End Subroutine Bissdl
