Module M_COUCHE_T

use M_PRECISION

  TYPE :: COUCHE_T
    sequence
    character(30)               :: Nom    ! Nom de la couche
    real(DOUBLE)                :: Cfond  ! Concentration de la couche
    real(DOUBLE)                :: Psable ! Pourcentage de sable
    real(DOUBLE)                :: D50    ! Diametre moyen du sable
    real(DOUBLE)                :: Wc     ! Vitesse de chute
    real(DOUBLE)                :: TauCE  ! Contrainte critique d'erosion
    real(DOUBLE)                :: Mpart  ! Coefficient des Partheniades
    real(DOUBLE)                :: Kp     ! Strickler de peau
    real(DOUBLE)                :: Kt     ! Strickler total
    real(DOUBLE)                :: TauCD  ! Contrainte critique de depot des vases
                                          ! (Utilise uniquement pour la couche 1)
  END TYPE COUCHE_T

End module M_COUCHE_T
