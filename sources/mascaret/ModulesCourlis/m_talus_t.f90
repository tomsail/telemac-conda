Module M_TALUS_T

use M_PRECISION

  TYPE TALUS_T
     sequence
     integer                     :: Modele     ! Choix du modele de traitement des berges
                                               ! (1 => pente ; 2 => glissement)
     real(DOUBLE)                :: PstabI     ! pente de stabilite des talus immerges
     real(DOUBLE)                :: PstabE     ! pente de stabilite des talus emerges
     real(DOUBLE)                :: Lambda     ! coefficient d'homothetie pour glissement
                                               ! d'une colonne de sediment (si modele = 2)
     real(DOUBLE)                :: ResRed     ! Coef. de resist. residuelle (si modele=2)
     real(DOUBLE)                :: GammaW     ! poids volum. de l'eau (si modele = 2)

     real(DOUBLE),dimension(:),pointer :: Gamma  ! Coefficient des Partheniades
  END TYPE TALUS_T

End Module M_TALUS_T
