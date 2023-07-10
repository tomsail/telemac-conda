Module M_PROFIL_COURLIS_T

use M_PRECISION

  TYPE PROFIL_COURLIS_T
     sequence
     character(30) :: Nom ! Nom du profil
     real(DOUBLE)  :: Abs ! Abscisse absolue du profil

     real(DOUBLE), dimension(:)  , pointer  :: X    ! Abscisse angulaire
     real(DOUBLE), dimension(:,:), pointer  :: Z    ! Cote des pts defin. les interfaces
     real(DOUBLE), dimension(:)  , pointer  :: Zref ! Point bas des interfaces

     real(DOUBLE), dimension(:)  , pointer  :: Frac ! Fraction multi granulo

     integer :: NbPoint ! Nombre de points du profil

  END TYPE PROFIL_COURLIS_T

End Module M_PROFIL_COURLIS_T
