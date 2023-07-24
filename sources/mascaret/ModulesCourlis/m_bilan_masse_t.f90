Module M_BILAN_MASSE_T

use M_PRECISION

  TYPE :: BILAN_MASSE_T
    sequence
    real(DOUBLE)              :: Initiale ! Masse presente initialement dans le bief
    real(DOUBLE)              :: Entrant  ! Masse entree dans le bief (CL amont) depuis le debut du calcul
    real(DOUBLE)              :: Sortant  ! Masse de sediment sortie du bief depuis le debut du calcul
    real(DOUBLE)              :: Apport   ! Masse entree par apport depuis le debut du calcul
    real(DOUBLE)              :: Depot    ! Masse deposee depuis le debut du calcul
    real(DOUBLE)              :: Eau      ! masse de sediment dans l'eau
    real(DOUBLE)              :: Erreur   ! erreur relative sur la masse
    real(DOUBLE), dimension(:), pointer :: DepotCouche  ! Masse deposee par couche depuis le debut du calcul
  END TYPE BILAN_MASSE_T


End Module M_BILAN_MASSE_T
