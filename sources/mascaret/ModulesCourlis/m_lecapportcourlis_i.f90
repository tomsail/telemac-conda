Module M_LecApportCourlis_I

Interface

Subroutine  LecApportCourlis( &

    UniteList         , & ! Unite du fichier d'impression des parametres
    ImpressionApport  , & ! Choix d'impression des apports en vase et sable
    Apport            , & ! Variable definissant les donnees d'apport de l'hydro
    NbLoiConc         , & ! Nombre de Lois de concentration
    ApportVase        , & ! Var. contenant les donnees sedim. des apports de vase
    ApportSable       , & ! Var. contenant les donnees sedim. des apports de sable
    CL_Vase           , & ! CL amont de la concentration en Vase
    CL_Sable          , & ! CL amont de la concentration en Sable
! Lecture des mots du dictionnaires
    MOTINT            , &
    ADRESS            , &
! Traitement des erreurs
    Erreur             ) ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture des donnees sedimentaires relatives aux apports
!  --------
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

USE M_MY_GLOBAL_VAR_SED
use M_SOURCE_TRACER_T     ! Definition du type TRACEUR_T
use M_APPORT_T            ! Definition du type APPORT_T
use M_CL_COURLIS_T        ! Definition du type CL_COURLIS_T

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(APPORT_T), dimension(:)  , pointer       :: Apport
  integer       , dimension(:)  , intent(in   ) :: MOTINT
  integer       , dimension(:,:), intent(in   ) :: ADRESS

  logical            , intent(in   ) :: ImpressionApport
  integer            , intent(in   ) :: UniteList
  integer            , intent(in   ) :: NbLoiConc

! Variables de sortie
  type(CL_COURLIS_T),                  intent(  out) :: CL_Vase, CL_Sable
  type(SOURCE_TRACER_T), dimension(:), pointer       :: ApportVase
  type(SOURCE_TRACER_T), dimension(:), pointer       :: ApportSable


! Variables locales
  integer :: NbApports     ! Nombre d'apports
  integer :: iApport       ! Numero ou indice de l'apport
  integer :: LoiVaseAmont  ! Numero de la loi pour la CL amont en vase
  integer :: LoiSableAmont ! Numero de la loi pour la CL amont en sable
  character(72) :: txt     ! Chaine de caractere temporaire

! Traitement des erreurs
  integer                       :: retour    ! Code de retour Read
  type(ERREUR_T), intent(inout) :: Erreur
!  character(132)        :: arbredappel_old  ! PU2017 : Mise en commentaire

End Subroutine LecApportCourlis

End Interface

End Module M_LecApportCourlis_I

