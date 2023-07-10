Subroutine Diffu (  &

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
!  VERSION : 4.0       07/2003  Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction : Calcule de la diffusion de la concentration
!  --------
!
!  Sous-programme appelant : DansLo
!  -----------------------
!
!  Sous-programme appele : Bissdl
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

!=========================================================================
!           Initialisations
!=========================================================================

  Erreur%Numero      = 0
  arbredappel_old    = trim(Erreur%arbredappel)
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>Diffu'



!=========================================================================
! Allocation des tableaux locaux
!=========================================================================

  NbProf = size(Absc)

  Allocate(AAA(NbProf),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'AAA')
    return
  End if

  Allocate(BBB(NbProf),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'BBB')
    return
  End if

  Allocate(CCC(NbProf),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'CCC')
    return
  End if

  Allocate(DDD(NbProf),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'DDD')
    return
  End if

  Allocate(A(NbProf),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'A')
    return
  End if

  Allocate(B(NbProf),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'B')
    return
  End if

  Allocate(C(NbProf),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'C')
    return
  End if

  Allocate(D(NbProf),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'D')
    return
  End if

!=========================================================================
! Conditions limites amont
!=========================================================================

  BBB(1) = W1
  CCC(1) = W0
  DDD(1) = T2(1) / Sm(1)

!=========================================================================
! Conditions limites aval
!=========================================================================

  If (Vit(NbProf) >= W0) Then
    AAA(NbProf) = -W1
    BBB(NbProf) =  W1
    DDD(NbProf) =  W0
  Else
    AAA(NbProf) =  W0
    BBB(NbProf) =  W1
    DDD(NbProf) =  T2(NbProf) / Sm(NbProf)
  Endif

!=========================================================================
! Calcul des coefficients courants
!=========================================================================

  Alpha = W1 / Dt

  Do i = 2, NbProf-1

    Hi   = Absc(i  ) - Absc(i-1)
    HiP1 = Absc(i+1) - Absc(i  )
    HHi  = Absc(i+1) - Absc(i-1)

    Ai   = Sm(i)
    APi  = (Sm(i) - Sm(i-1)) / Hi

    Cst1 =  W2 / (HiP1 * HHi )
    Cst2 =  W2 / (Hi   * HHi )
    Cst3 = -W2 / (Hi   * HiP1)

    AAA(i) =            - Cnux * Ai * Cst2 + Cnux * APi / Hi
    BBB(i) = Alpha * Ai - Cnux * Ai * Cst3 - Cnux * APi / Hi
    CCC(i) =            - Cnux * Ai * Cst1
    DDD(i) = T2(i) * Alpha

  Enddo

  Do i = 1, NbProf
    A(i) = AAA(i)
    B(i) = BBB(i)
    C(i) = CCC(i)
    D(i) = DDD(i) + Flux(i)
  Enddo

!=========================================================================
! Resolution du systeme
!=========================================================================

  call Bissdl  (  &
        Conc   ,  & ! Resultat de la diffusion
        A      ,  & ! ! Coefficient
        B      ,  & ! ! de la
        C      ,  & ! ! matrice de
        D      ,  & ! ! diffusion
        NbProf ,  & ! Nombre de point des tableaux
        Erreur   )

  If (Erreur%Numero /= 0) Then
    return
  Endif

!=========================================================================
! Dellocation des tableaux locaux
!=========================================================================

  deallocate(AAA, BBB, CCC, DDD)
  deallocate(A, B, C, D)

!  Erreur%arbredappel = arbredappel_old

  return

End Subroutine Diffu
