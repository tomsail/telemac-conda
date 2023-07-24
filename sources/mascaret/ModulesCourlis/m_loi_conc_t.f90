Module M_LOI_CONC_T

use M_PRECISION

! Constantes reperant le  type de "loi"
!--------------------------------------
  implicit none
  integer, parameter :: LOI_UNITE_SECONDE = 1 !  Seconde "S"
  integer, parameter :: LOI_UNITE_MINUTE  = 2 !  Minute  "M"
  integer, parameter :: LOI_UNITE_HEURE   = 3 !  Heure   "H"
  integer, parameter :: LOI_UNITE_JOUR    = 4 !  Jour    "J"

  integer, parameter :: LOI_UNITE_NB_MAX = 15

  TYPE LOI_CONC_T
    sequence
    character(30)                        :: Nom        ! Nom de la couche
    real(DOUBLE), dimension(:), pointer  :: Temps      ! temps
    real(DOUBLE), dimension(:), pointer  :: Conc       ! concentration en MES
  END TYPE LOI_CONC_T

End Module M_LOI_CONC_T
