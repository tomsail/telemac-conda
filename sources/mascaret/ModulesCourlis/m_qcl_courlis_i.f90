Module M_Qcl_Courlis_I

Interface

Subroutine Qcl_Courlis  (  &

  CL_Vase         ,  & ! CL amont de la concentration en Vase
  CL_Sable        ,  & ! CL amont de la concentration en Sable
  ApportVase      ,  & ! Apports en vase
  ApportSable     ,  & ! Apports en sable
  Apport          ,  & ! Apports hydrauliques
  LoiHydrau       ,  & ! Lois hydrauliques
  LoiConc         ,  & ! Lois de concentration
  Temps           ,  & ! Temps
  Erreur          )    ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Extrait des lois de debit et de concentration Q, Cvase,
!  --------	  Csable au temps Temps pour - la condition limite amont
!                                        - les apports
!
!  Sous-programme appelant : CalcApport
!  -----------------------
!
!  Sous-programme appele : Interpolation_s
!  ---------------------
!
!=========================================================================

use M_PRECISION                 ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C               ! Definition des constante tq EPS*, W0, ...

use M_APPORT_T                  ! Definition du type APPORT_T
use M_SOURCE_TRACER_T           ! Donnees des sources d'un traceur
use M_CL_COURLIS_T              ! Definition du type CL_COURLIS_T
use M_LOI_T                     ! Definition du type LOI_T
use M_LOI_CONC_T                ! Definition du type LOI_T

use M_INTERPOLATION_S           ! Sous-programme INTERPOLATION_S

use M_ERREUR_T                  ! Type ERREUR_T
use M_MESSAGE_C                 ! Messages d'erreur
use M_TRAITER_ERREUR_I          ! Traitement de l'erreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Constante  pour les interpolations
  integer , parameter :: ORDRE_INTERPOLATION = 1

! Variables d'entree
  real(DOUBLE), intent(in) :: Temps

  type(LOI_T)     , dimension(:), intent(in) :: LoiHydrau
  type(LOI_CONC_T), dimension(:), intent(in) :: LoiConc

! Variables de sortie
  type(SOURCE_TRACER_T), dimension(:), intent(inout) :: ApportVase, ApportSable
  type(APPORT_T)       , dimension(:), intent(inout) :: Apport
  type(CL_COURLIS_T)                 , intent(inout) :: CL_Vase, CL_Sable

! Variables locales
  integer :: nb_apport          ! Nombre d'apports
  integer :: iapp               ! Compteur
  integer :: num_loi            ! Numero de la loi utilisee

  real(DOUBLE) :: ApportConc    ! Concentration de l'apport a l'instant Temps

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
!  character(132) :: arbredappel_old ! Ancien arbre d'appel  ! PU2017 : Mise en commentaire

!=========================================================================

End Subroutine Qcl_Courlis

End Interface

End Module M_Qcl_Courlis_I
