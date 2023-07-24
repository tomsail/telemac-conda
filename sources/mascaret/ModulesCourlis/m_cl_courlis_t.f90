Module M_CL_COURLIS_T

!=========================== Declarations ==============================

use M_PRECISION

  TYPE CL_COURLIS_T
    sequence
    character(30) :: Nom        ! Nom de la condition limite
    integer       :: NumeroLoi  ! Numero de la loi associee
    real(DOUBLE)  :: Debit      ! Debit correspondant
    real(DOUBLE)  :: Conc       ! Concentration
  END TYPE CL_COURLIS_T

End module M_CL_COURLIS_T
