Subroutine bedload_lefort_2014 (  &
        dm              ,  & ! diametre moyen
        d84             ,  & ! diametre 84
        d50             ,  & ! diametre median
        d16             ,  & ! diametre 16
        Ks              ,  & ! Strickler total
        Kr              ,  & ! Strickler de peau
        penteJ          ,  & ! pente de la ligne d'energie
        nu              ,  & ! viscosité dynamique de l'eau
        Rho             ,  & ! Masse volumique eau
        RhoS            ,  & ! Masse volumique sediment
        width_inv       ,  & ! Inverse de la largeur du profil
        Q               ,  & ! Debit
        Qsed            ,  & ! Flux de sediment (> 0 depot, < 0 erosion)
        porosite        ,  & ! Porosite
        qstar_option    ,  &
        debug_bedload      &
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
!  --------   de Lefort 2014
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
  real(DOUBLE), intent(in)  :: dm, d84, d50, d16, Ks, Kr
  real(DOUBLE), intent(in)  :: penteJ, nu, Rho, RhoS, width_inv, Q
  real(DOUBLE), intent(in)  :: porosite
  real(DOUBLE), intent(out) :: Qsed

  logical     , intent(in)  :: qstar_option
  logical     , intent(in)  :: debug_bedload

! Variables locales
  real(DOUBLE)              :: S, dm_star, cd, Q0, n, cor
  real(DOUBLE)              :: q_star, MM, z, F, m, KsOverKr

  !relative density
  S = RhoS / Rho

  If (penteJ > 0.D0) Then

    q_star = Q * width_inv / sqrt(Gpes * penteJ * dm**3)

    If (qstar_option) Then
      If (q_star .LT. 200) Then
        KsOverKr = 0.75 * (q_star / 200) ** 0.23
      Else
        KsOverKr = 0.75
      Endif
    Else
      KsOverKr = Ks / Kr
    Endif

    dm_star = dm * (Gpes * (S - 1) / nu**2)**W13

    cd = 0.0444 * (1 + 15 / (1 + dm_star) - 1.5 * exp(-dm_star / 75))

    n = 1.6 + 0.06 * log10(penteJ)
    Q0 = sqrt(Gpes * ((S - 1) * dm)**3) * cd * (dm * width_inv)**W13 &
         * (KsOverKr)**(-0.5) * penteJ**(-n) / width_inv
    if (debug_bedload) then
      Write(*,*) "Debit de mise en mouvement Q0", Q0
    endif

    if (KsOverKr .LT. 0.63 .AND. dm_star < 14) Then
      cor = 1 - 1.4 * exp(-0.9 * ((KsOverKr)**2 * sqrt(Q/Q0)))
    else
      cor = 1
    endif


    if (q_star .LT. 200) Then
      MM = (q_star + 2.5) / 200
    else
      MM = 1
    endif

    if (Q/Q0 .GT. 3.4) Then
      z = 1 + 0.38 / dm_star**0.45 * (Q * width_inv / sqrt(Gpes * dm**3))**0.192
    else
      z = 1
    endif

    if (Q .LT. Q0) Then
      F = 0.06 * MM * (Q / Q0)
    else
      F = (6.1 * (1 - 0.938 * (Q0 / Q)**0.284)**1.66)**z
    endif

    m = 1.8 + 0.08 * log10(penteJ)

    Qsed = Q * 1.7 * penteJ**m * S / (S - 1)**1.65 * (0.5 * (d84/d50 + d50/d16))**0.2 &
           * cor * F / (S * (1 - porosite))

  else
    Qsed = 0.D0

  endif

  return

End subroutine bedload_lefort_2014
