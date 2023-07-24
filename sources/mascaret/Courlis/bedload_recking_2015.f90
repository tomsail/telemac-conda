Subroutine bedload_recking_2015 (  &
        dm                    ,  & ! diametre moyen
        d84                   ,  & ! diametre 84
        d50                   ,  & ! diametre median
        penteJ                ,  & ! pente de la ligne d'energie
        Rho                   ,  & ! Masse volumique eau
        RhoS                  ,  & ! Masse volumique sediment
        width_inv             ,  & ! Inverse de la largeur du profil
        Q                     ,  & ! Debit
        Qsed                  ,  & ! Flux de sediment (> 0 depot, < 0 erosion)
        porosite              ,  & ! Porosite
        recking_morpho        ,  & ! option for type of morphology river (1 for pool-riffle, 2 for others)
        recking_q_star_option ,  & ! option to use expression of Tau with Q (True) or with Rh (false)
        Sm                    ,  & ! wet section
        Pm                       & ! wet perimeter
                                  )

!*************************************************************************
!  PROGICIEL : COURLIS           M. Secher
!
!  VERSION : 8.3       10-2021		Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :	Calcul de la capacite de transport solide selon la formule
!  --------   de Recking 2015
!
!  Sous-programme appelant : Courlis
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================
  use M_PRECISION
  use M_PARAMETRE_C

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE), intent(in)  :: dm, d84, d50
  real(DOUBLE), intent(in)  :: penteJ, Rho, RhoS, width_inv, Q
  real(DOUBLE), intent(in)  :: porosite
  real(DOUBLE), intent(in)  :: Sm, Pm
  logical     , intent(in)  :: recking_q_star_option
  integer     , intent(in)  :: recking_morpho

  real(DOUBLE), intent(out) :: Qsed

! Variables locales
  real(DOUBLE)              :: TauM_star, q_star, p, Tau84_star, S, phi, Rh

  ! relative density
  S = RhoS / Rho

  Rh = Sm / Pm

  if (recking_morpho == 1) Then

    TauM_star = (5 * penteJ + 0.06) * ((d84 / d50) ** (4.4 * sqrt(penteJ) - 1.5))

  else if (recking_morpho == 2) Then

    TauM_star = 1.5 * PenteJ ** 0.75

  endif

  if (recking_q_star_option) then

    q_star = Q * width_inv / sqrt(Gpes * penteJ * d84**3)

    if (q_star < 100) then

      p = 0.23

    else

      p = 0.31

    endif

    Tau84_star = 0.015 * penteJ * q_star ** (2 * p) / ((S - 1) * p ** 2.5)

  else

    Tau84_star = Rh * PenteJ / ((s - 1) * d84)

  endif

  phi = 14 * Tau84_star ** 2.5 / (1 + (TauM_star / Tau84_star) ** 4)

  Qsed = phi * sqrt(Gpes * (S - 1) * d84 ** 3) / ((1 - porosite) * width_inv)

  return

End subroutine bedload_recking_2015
