Subroutine calcul_pente_energie (  &
        Vit                      ,  & ! Vitesse moyenne
        Zsurf                    ,  & ! Cote surface libre
        Y                        ,  & ! Abscisse longitudinale
        NbProfil                 ,  & ! Nombre de profils
        penteJ                   ,  & ! pente de la ligne d'energie
        round_slope_option       ,  & ! option arrondi pente
        round_precision          ,  & ! precision option arrondi pente
        strickler_slope_option   ,  & ! option to choose Strickler method
        Ks                       ,  & ! Strickler
        Sm                       ,  & ! Surface mouillee
        Pm                          & ! Perimetre mouillee
                                  )

!*************************************************************************
!  PROGICIEL : COURLIS           M. Secher
!
!  VERSION : 8.3       10-2021		Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :	Calcul de la pente de la ligne d'energie J
!  --------
!
!  Sous-programme appelant : compute_bedload
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
  real(DOUBLE), dimension(:), intent(in)   :: Vit, Zsurf, Y
  integer     ,               intent(in)   :: NbProfil
  real(DOUBLE), dimension(:), intent(out)  :: penteJ
  real(DOUBLE), dimension(:), intent(in)   :: Sm, Pm

  logical     ,               intent(in)   :: round_slope_option
  logical     ,               intent(in)   :: strickler_slope_option
  integer     ,               intent(in)   :: round_precision


  real(DOUBLE),               intent(in)   :: Ks

! Variables locales
  integer :: i, j, k
  real(DOUBLE) :: Pente, OrdAbs
  real(DOUBLE), dimension(:), allocatable :: charge
  real(DOUBLE)                            :: Rh, Q

  if (strickler_slope_option) then

    do i = 1, NbProfil

      Q = Sm(i) * Vit(i)
      if (Pm(i) > EPS8) then
        Rh = Sm(i) / Pm(i)
        penteJ(i) = Q ** 2 / (Ks ** 2 * Sm(i) ** 2 * Rh**(4._DOUBLE / 3._DOUBLE))
      else
        penteJ(i) = 0
      endif

    enddo

  else

    Allocate(charge(NbProfil))

    do i = 1, NbProfil

      charge(i) = Zsurf(i) + Vit(i) / (2 * Gpes)

    enddo

    do i = NbProfil, 2, -1

      if (charge(i) > charge(i - 1)) then
        j = i - 1
        do while(charge(i) > charge(j))

          if (j == 1) then
            charge(j) = charge(i)
          else
            j = j - 1
          endif
        enddo

        Pente = (charge(j)- charge(i)) / (Y(j) - Y(i))
        OrdAbs = charge(j) - Pente * Y(j)

        do k = i-1, j+1, -1
          charge(k) = Y(k) * Pente + OrdAbs
        enddo
      endif
    enddo

    do i = 1, NbProfil - 1

      penteJ(i) = -(charge(i) - charge(i+1)) / (Y(i) - Y(i+1))

    enddo

    penteJ(NbProfil) = penteJ(NbProfil-1)

  endif

  if (round_slope_option) then

    do i = 1, NbProfil

      penteJ(i) = ANINT(penteJ(i) * 10 ** (round_precision)) &
                                  / 10 ** (round_precision)

    enddo

  endif

  return

End subroutine calcul_pente_energie
