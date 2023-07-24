Module M_INDEX_VARIABLE_COURLIS_C

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       08/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Module qui definit les constantes servant a reperer les
!  --------   variables
!=========================================================================

  !=========================== Declarations ==============================

  !  use M_PRECISION

  ! Liste des indices sur les tableaux de variables
  !=================================================

  ! 1. Variables principales
  !--------------------------

  integer, parameter :: VARCO_X        = 1   ! maillage
  ! `X' doit etre la 1ere variable (indicee par 1)
  integer, parameter :: VARCO_Zdur     = 2   ! cote basse des fonds durs
  integer, parameter :: VARCO_Vit      = 3   ! vitesse
  integer, parameter :: VARCO_Sm       = 4   ! surface mouillee
  integer, parameter :: VARCO_Pm       = 5   ! perimetre mouille
  integer, parameter :: VARCO_Zsurf    = 6   ! cote de la surface libre
  integer, parameter :: VARCO_CVase    = 7   ! concentration de vase en suspension
  integer, parameter :: VARCO_CSable   = 8   ! concentration de sable en suspension
  integer, parameter :: VARCO_Qvase    = 9   ! flux de depot des vases
  integer, parameter :: VARCO_Qsable   = 10  ! flux de depot des sables
  integer, parameter :: VARCO_DSurSed  = 11  ! variation de surface sedimentaire
  integer, parameter :: VARCO_TauHMax  = 12  ! contrainte locale maximale
  integer, parameter :: VARCO_TauHMoy  = 13  ! contrainte locale moyenne
  integer, parameter :: VARCO_TauEMax  = 14  ! contrainte effective maximale
  integer, parameter :: VARCO_TauEMoy  = 15  ! contrainte effective moyenne
  integer, parameter :: VARCO_CeqMoy   = 16  !

  ! concentration traceurs
  integer, dimension(7) :: VARCO_Zref
  integer, dimension(7) :: VARCO_DepC
  data VARCO_Zref / 17,18,19,20,21,22,23 /
  data VARCO_DepC / 24,25,26,27,28,29,30 /

  ! Constante representant le nombre de variables principales

  integer, parameter :: NB_VARCO_PRINCIPAL = 30

  ! 2. Variables optionnelles
  !--------------------------

  ! Constante representant le nombre total de variables

  integer, parameter :: NB_TOT_VARCO = 30

End Module M_INDEX_VARIABLE_COURLIS_C
