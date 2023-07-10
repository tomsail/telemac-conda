Module M_calcul_pente_energie_I

Interface

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
        Pm                          & ! Rayon hydraulique
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

  logical     ,               intent(in)   :: round_slope_option
  logical     ,               intent(in)   :: strickler_slope_option
  integer     ,               intent(in)   :: round_precision

  real(DOUBLE),               intent(in)   :: Ks
  real(DOUBLE), dimension(:), intent(in)   :: Sm, Pm


End subroutine calcul_pente_energie

End Interface

End Module M_calcul_pente_energie_I
