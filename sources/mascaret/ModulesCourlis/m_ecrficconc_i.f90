Module M_EcrFicConc_I

Interface

Subroutine  EcrFicConc  (  &

    FicResuMES ,  & ! Fichier des MES (sable et vase) finales
    NbProfil   ,  & ! nombre de profils
    Absc       ,  & ! Abscisse curviligne (ProfilCourlis%Abs
    CVase      ,  & ! Concentration des vases en suspension
    CSable     ,  & ! Concentration des sables en suspension
    TitreCas   ,  & ! Titre du cas de calcul
    Temps      ,  & ! Temps du calcul
    Erreur        ) ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Ecriture du fichier contenant les concentration en
!  --------    suspension de sable et de vase a la fin du calcul
!             Ce fichier sert de condition initiale dans le cas d'une
!              suite de calcul
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!=========================================================================
!  Commentaire : Description du fichier
!  -----------
!   #Resultat du calcul <<TitreCas>> au temps <<Temps>>
!   #29/07/2003
!   [variables]
!   "Abscisse curviligne"  ;"X"   ;"m"  ;1
!   "Concentration en vase"  ;"CVASE" ;"g/l";3
!   "Concentration en sable";"CSABLE";"g/l";3
!   [resultats]
!   0;     0.5;    0.1
!   10;    0.5;    0.1
!   20;    0.5;    0.1
!   30;    0.5;    0.1
!   40;    0.5;    0.1
!   50;    0.5;    0.1
!=========================================================================


!=========================================================================
!   DECLARATIONS
!=========================================================================

use M_PRECISION            ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info

use M_FICHIER_T            ! Definition du type FICHIER_T

use M_ERREUR_T             ! Definition du type ERREUR_T
use M_MESSAGE_C            ! Messages d'erreur
use M_TRAITER_ERREUR_I     ! Traitement de l'errreur

!=========================================================================

!.. Implicit Declarations ..

  implicit none

! Constantes
  integer, parameter :: LEN_CHAINE = 80

! Variables d'entree
  type(FICHIER_T),        intent(in   ) :: FicResuMES

  integer,                    intent(in   ) :: NbProfil
  real(DOUBLE), dimension(:), intent(in   ) :: Absc
  real(DOUBLE), dimension(:), intent(in   ) :: CVase
  real(DOUBLE), dimension(:), intent(in   ) :: CSable

  character(LEN_CHAINE),    intent(in   ) :: TitreCas
  real(DOUBLE),             intent(in   ) :: Temps

! Variables de sortie

! Variables locales
  integer :: i           ! Compteur de points
  integer :: Unite       ! Unite du fichier de sedimentation a ecrire

! Traitement des erreurs
  integer        :: retour       ! code de retour des fonctions d'e/s
!  character(132) :: arbredappel_old   ! ancien arbre  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================

End Subroutine EcrFicConc

End Interface

End Module M_EcrFicConc_I
