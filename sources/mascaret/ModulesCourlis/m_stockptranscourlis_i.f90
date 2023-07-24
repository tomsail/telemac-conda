Module M_StockPTransCourlis_I

Interface

Subroutine  StockPTransCourlis  ( &

  FicStockPTransCourlis   , & ! Unite logique fichier listing
  PhaseStockPTransCourlis , & ! Phase de la simulation (init, calcul, ??)
  Temps                   , & ! Temps courant
  NbProfil                , & ! Nombre de profils
  NbInterface             , & ! Nombre d'interfaces
  ProfilCourlis           , & ! Profils sedimentaires
  Zsurf                   , & ! Cote de la surface libre
  TauH                    , & ! Contrainte hydr. loc. (depend du tirant d'eau local)
  TauE                    , & ! Contrainte hydr. eff. (depend du rayon hydr.)
  Ceq                     , & ! Conc. d'equilibre des sables
  Erreur                  )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       05/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Ecriture du fichier resultat de Courlis pour POSTCOURLIS
!  --------    - trace des profils en travers
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele :  SurfaceLibre
!  ---------------------
!
!=========================================================================

use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info

use M_FICHIER_T           ! Definition du type FICHIER_T
use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

use M_SurfaceLibre_I      ! Interface du sous-programme SurfaceLibre

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

!
! Variables d'entree
!-------------------
  real(DOUBLE)        :: Temps
  integer, intent(in) :: NbInterface
  integer, intent(in) :: NbProfil
  integer, intent(in) :: PhaseStockPTransCourlis

  type(FICHIER_T)             , intent(in) :: FicStockPTransCourlis
  real(DOUBLE), dimension(:)  , intent(in) :: Zsurf
  real(DOUBLE), dimension(:,:), intent(in) :: TauH, TauE, Ceq

  type(PROFIL_COURLIS_T), dimension(:), intent(in)  :: ProfilCourlis

! Variables locales
!------------------
  integer :: NbCouche  ! Nb de couches de sediments
  integer :: i, j, k    ! indices
  integer :: NbVar
  integer :: Unite      ! Unite logique de fichier ouvert en ecriture
  real(DOUBLE), dimension(:), pointer  :: PtRiveG, PtRiveD

  ! Noms de variables
  character(10) :: C_ProfAbs , C_Zdur , C_TauH , C_TauE , C_Ceq
  character(10) :: Cu_ProfAbs, Cu_Zref, Cu_TauH, Cu_TauE, Cu_Ceq
  character(5), dimension(7) :: C_Zref

! Traitement des erreurs
!-----------------------
  integer        :: retour
!  character(132) :: arbredappel_old  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================

End Subroutine StockPTransCourlis

End Interface

End Module M_StockPTransCourlis_I
