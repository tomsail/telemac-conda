Module M_BILAN_FLUX_T

use M_PRECISION

  TYPE :: BILAN_FLUX_T
    sequence
    real(DOUBLE)  :: Entrant  ! Flux de sediments entrant dans  le bief (CL amont)
    real(DOUBLE)  :: Apport   ! Flux de sediments entrant par apport
    real(DOUBLE)  :: Sortant  ! Flux de sediments sortant du bief
    real(DOUBLE)  :: Depot    ! Flux de sediments depose dans le bief
                              ! (>0 pour depot, <0 pour erosion)
  END TYPE BILAN_FLUX_T


End Module M_BILAN_FLUX_T
