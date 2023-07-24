Subroutine bedload_recking_2013 (  &
        dm              ,  & ! diametre moyen
        d84             ,  & ! diametre 84
        d50             ,  & ! diametre median
        penteJ          ,  & ! pente de la ligne d'energie
        Rho             ,  & ! Masse volumique eau
        RhoS            ,  & ! Masse volumique sediment
        width_inv       ,  & ! Inverse de la largeur du profil
        Q               ,  & ! Debit
        Qsed            ,  & ! Flux de sediment (> 0 depot, < 0 erosion)
        porosite           & ! Porosite
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
!  --------   de Recking 2013
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

  real(DOUBLE), intent(out) :: Qsed

! Variables locales
  real(DOUBLE)              :: TauM_star, q_star, p, Tau84_star, S, phi

  !relative density
  S = RhoS / Rho

  if (dm .GT. 0.002) Then

    TauM_star = (5 * penteJ + 0.06) * ((d84 / d50) ** (4.4 * sqrt(penteJ) - 1.5))

  else

    TauM_star = 0.045

  endif

  q_star = Q * width_inv / sqrt(Gpes * penteJ * d84**3)

  if (q_star < 100) then

    p = (1 - 0.545) / 2

  else

    p = (1 - 0.395) / 2

  endif

  Tau84_star = penteJ / ((S - 1) * d84 *                                   &
                         (2 * width_inv                                    &
                       + 74 * p ** 2.6 * (Gpes * PenteJ) ** p              &
                       * (Q * width_inv) ** (-2 * p) * d84 ** (3 * p - 1)  &
                         )                                                 &
                        )

  phi = 14 * Tau84_star ** 2.5 / (1 + (TauM_star / Tau84_star) ** 4)

  Qsed = phi * sqrt(Gpes * (S - 1) * d84 ** 3) / ((1 - porosite) * width_inv)

  return

End subroutine bedload_recking_2013
