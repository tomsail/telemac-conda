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
!  VERSION : 4.0       07/2003		Copyright EDF-CETMEF
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

!=========================================================================
!           Initialisations
!=========================================================================


  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>Qcl_Courlis'


!=========================================================================
!           Calcul du debit et des concentrations des apports
!=========================================================================

  nb_apport = size(Apport)


  If (nb_apport /= 0) Then

      Do iapp = 1, nb_apport

      ! Debit des apports
      num_loi = Apport(iapp)%NumeroLoi

      call INTERPOLATION_S                 ( &
        Apport(iapp)%Debit                 , &
        Temps                              , &
        ORDRE_INTERPOLATION                , &
        LoiHydrau(num_loi)%Temps           , &
        LoiHydrau(num_loi)%Debit           , &
        size(LoiHydrau(num_loi)%Temps)     , &
        Erreur                             )

      If (Erreur%Numero /= 0) Then
        return
      End if

      ! Concentration en vase des apports
      num_loi = ApportVase(iapp)%NumeroLoi

      call INTERPOLATION_S                 ( &
        ApportConc                         , &
        Temps                              , &
        ORDRE_INTERPOLATION                , &
        LoiConc(num_loi)%Temps             , &
        LoiConc(num_loi)%Conc              , &
        size(LoiConc(num_loi)%Temps)       , &
        Erreur                             )

      If (Erreur%Numero /= 0) Then
        return
      End if

      ApportVase(iapp)%Debit_source = Apport(iapp)%Debit * ApportConc


      ! Concentration en sable des apports
      num_loi = ApportSable(iapp)%NumeroLoi

      call INTERPOLATION_S                 ( &
        ApportConc                         , &
        Temps                              , &
        ORDRE_INTERPOLATION                , &
        LoiConc(num_loi)%Temps             , &
        LoiConc(num_loi)%Conc              , &
        size(LoiConc(num_loi)%Temps)       , &
        Erreur                             )

      If (Erreur%Numero /= 0) Then
        return
      End if

      ApportSable(iapp)%Debit_source = Apport(iapp)%Debit * ApportConc

    End do

  End if

!=========================================================================
!   Calcul du debit et des concentrations de la condition limite amont
!=========================================================================

  ! Concentration en vase amont
  num_loi = CL_Vase%NumeroLoi

  call INTERPOLATION_S             ( &
    CL_Vase%Conc                   , &
    Temps                          , &
    ORDRE_INTERPOLATION            , &
    LoiConc(num_loi)%Temps         , &
    LoiConc(num_loi)%Conc          , &
    size(LoiConc(num_loi)%Temps)   , &
    Erreur                         )

  If (Erreur%Numero /= 0) Then
    return
  End if

  ! Concentration en sable amont
  num_loi = CL_Sable%NumeroLoi

  call INTERPOLATION_S             ( &
    CL_Sable%Conc                  , &
    Temps                          , &
    ORDRE_INTERPOLATION            , &
    LoiConc(num_loi)%Temps         , &
    LoiConc(num_loi)%Conc          , &
    size(LoiConc(num_loi)%Temps)   , &
    Erreur                         )

  If (Erreur%Numero /= 0) Then
    return
  End if

!=========================================================================
!  Erreur%arbredappel = arbredappel_old

return

End Subroutine Qcl_Courlis
