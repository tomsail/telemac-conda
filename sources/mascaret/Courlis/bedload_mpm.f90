Subroutine bedload_mpm (  &
        Tau             ,  & ! Contrainte adimensionnelle
        d50m            ,  & ! Diametre moyen
        Rho             ,  & ! Masse volumique eau
        RhoS            ,  & ! Masse volumique sediment
        width_inv       ,  & ! Inverse de la largeur du profil
        Qsed               & ! Flux de sediment (> 0 depot, < 0 erosion)
                                  )

!*************************************************************************
!  PROGICIEL : COURLIS           M. Secher
!
!  VERSION : 8.3       10-2021		Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :	Calcul le flux sédimentaire en fonction de
!  --------
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
  real(DOUBLE), intent(in)  :: Tau, Rho, RhoS, width_inv, d50m
  real(DOUBLE)              :: RG, R
  real(DOUBLE), intent(out) :: Qsed

  R = (RhoS-Rho)/Rho
  RG = R * Gpes

  if (Tau > 0.047_DOUBLE ) then
    Qsed =  (8_DOUBLE * sqrt( RG*d50m**3_DOUBLE ) &
                      * (Tau-0.047_DOUBLE)**(3._DOUBLE/2._DOUBLE)) &
                      / width_inv
  else
    Qsed = w0
  endif


  return

End subroutine bedload_mpm
