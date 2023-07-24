Subroutine Convec       (  &

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
!  VERSION : 5.1       08/2009		Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Calcul de la convection d'une concentration
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

!=========================================================================
!	INITIALISATIONS ET ALLOCATION DES TABLEAUX LOCAUX
!=========================================================================

  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>Convec'

  IM=size(Absc)

  Allocate(T1(IM),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'T1')
    return
  End if

  Allocate(Nul(IM),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'Nul')
    return
  End if

  Nul(:) = W0

!=========================================================================
!	CALCUL DE LA GRANDEUR CONVECTEE A L'INSTANT T
!=========================================================================
  !**************** INITIALISATION **************************

  Umax = 0.
  do i = 1,IM
!    if (U(i) >= Umax) then
    if (ABS(V1(i)) >= Umax) then
      Umax = V1(i)
    endif
  enddo

  !**************** CONVECTION DU TRACEUR *******************

  FLUENT=W0
  FLUSOR=W0

  if ((.not.ConsConv%Conv).OR.(Umax==0.)) then ! pas de convection

    if (ConsConv%Scheconv == 2) then

      do i = 1,IM
        T2(i) = Conc(i)
      end do

    else

      do i = 1,IM
        T2(i) = Sm1(i) * Conc(i)
      end do

    endif

  else

    if (ConsConv%Scheconv == 2) then     ! Forme non conservative - Caracteristiques en convection faible (convection de C)

      CG = CL_amont ! cond lim amont/aval
      CD = W0
      do i = 1,IM
        T1(i) = Conc(i)
      end do
      call HYP1FA(T2,T1,V1,Absc,DT,1,IM,CG,CD,Nul,Nul,Erreur)
      do i= 1, IM
        T2(i)=T2(i)*Sm1(i)
      end do

    elseif (ConsConv%Scheconv == 3) then ! Forme conservative - Caracteristiques en convection faible (convection de A*C)

      CG = CL_amont * Sm1(1) ! cond lim amont
      CD = W0                ! cond lim aval nulle
      do i = 1,IM
        T1(i) = Sm0(i) * Conc(i)
      end do
      call HYP1FA(T2,T1,V1,Absc,DT,0,IM,CG,CD,Nul,Nul,Erreur)

    elseif (ConsConv%Scheconv == 4) then ! Forme conservative - Volumes finis (convection de A*C)

      CG = CL_amont * Sm1(1) ! cond lim amont
      CD = W0                ! cond lim aval nulle
      do i = 1,IM
        T1(i) = Sm0(i) * Conc(i)
      end do
          if (ConsConv%ordreVF == 1) then
            call GODUNOV(T2,T1,V1,CG,CD,Absc,DT,IM,FLUENT,FLUSOR,Erreur)
          elseif ((ConsConv%ordreVF == 2) .OR. (ConsConv%ordreVF == 3)) then
        call MUSCL_HANCOCK(T2,T1,V1,ConsConv,CG,CD,Absc,DT,IM,FLUENT,FLUSOR,Erreur)
      endif
    end if

  end if

!=========================================================================
!	DEALLOCATION DES TABLEAUX LOCAUX
!=========================================================================

  Deallocate(T1, Nul)

!  Erreur%arbredappel = arbredappel_old


  return

End Subroutine Convec
