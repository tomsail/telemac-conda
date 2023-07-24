Module M_CalcApport_I

Interface

Subroutine CalcApport  (  &

    CL_Vase         ,  & ! CL amont de la concentration en Vase
    CL_Sable        ,  & ! CL amont de la concentration en Sable
    QApportVase     ,  & ! Debit de l'apport en vase en (kg/s/m)
    QApportSable    ,  & ! Debit de l'apport en sable en (kg/s/m)
    ApportVase      ,  & ! Apports en vase
    ApportSable     ,  & ! Apports en sable
    Apport          ,  & ! Apports hydrauliques
    LoiHydrau       ,  & ! Lois hydrauliques
    LoiConc         ,  & ! Lois de concentration
    Temps           ,  & ! Temps
    Absc            ,  & ! Abscisse des sections de calcul (ProfilCourlis%Abs)
    Erreur          )    ! Erreur


!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Extrait des lois de debit et de concentration Q, Cvase,
!  --------    Csable au temps Temps pour - la condition limite amont
!                                         - les apports
!
!  Sous-programme appelant : DansLo
!  -----------------------
!
!  Sous-programme appele : Qcl_Courlis
!  ---------------------
!
!=========================================================================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C      ! Definition des constante tq EPS*, W0, ...

use M_APPORT_T         ! Definition du type APPORT_T
use M_SOURCE_TRACER_T  ! Donnees des sources d'un traceur
use M_CL_COURLIS_T     ! Definition du type CL_COURLIS_T
use M_LOI_T            ! Definition du type LOI_T
use M_LOI_CONC_T       ! Definition du type LOI_T

use M_Qcl_Courlis_I    ! Sous-programme QCL_COURLIS

use M_ERREUR_T         ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I ! Traitement de l'errreur


!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Constante  pour les interpolations
!  integer , parameter :: ORDRE_INTERPOLATION = 1  ! PU2017 : Mis en commentaire

! Variables d'entree
  real(DOUBLE), intent(in) :: Temps

  real(DOUBLE)    , dimension(:), intent(in) :: Absc

  type(LOI_T)     , dimension(:), intent(in) :: LoiHydrau
  type(LOI_CONC_T), dimension(:), intent(in) :: LoiConc

! Variables de sortie
  type(SOURCE_TRACER_T), dimension(:), intent(inout) :: ApportVase
  type(SOURCE_TRACER_T), dimension(:), intent(inout) :: ApportSable
  type(APPORT_T)       , dimension(:), intent(inout) :: Apport
  type(CL_COURLIS_T)                 , intent(inout) :: CL_Vase, CL_Sable
  real(DOUBLE)         , dimension(:), intent(  out) :: QApportVase
  real(DOUBLE)         , dimension(:), intent(  out) :: QApportSable

! Variables locales
  integer :: nb_apport      ! Nombre d'apports
  integer :: iapp, is      ! Compteur
!  integer :: num_loi      ! Numero de la loi utilisee
  integer :: ns_debut, ns_fin  ! numero de section de debut et fin d'un apport

  real(DOUBLE) :: longueur_apport_modele  ! Longueur de l'apport
  real(DOUBLE) :: sourceV, sourceS      ! Debit d'apport de la source

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
!  character(132) :: arbredappel_old    ! Ancien arbre d'appel  ! PU2017 : Mis en commentaire

!=========================================================================

End Subroutine CalcApport

End Interface

End Module M_CalcApport_I
