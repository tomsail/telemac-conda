Module M_lecLoiConc_I

Interface

Subroutine  lecLoiConc  ( &

    UniteListing      , & ! Unite logique fichier listing
    ImpressionLoiConc , & ! Choix d'impression des lois de concentration
    FicLoiConc        , & ! Fichier de l'evolut temporelle de conc en vase
    NbLoiConc         , & ! Nombre de lois de concentration
    LoiConc           , & ! Structure de donnnees des lois de concentration
    CritereArret      , & ! Critere d'arret des calculs
    TempsMaximum      , & ! Temps maximum de calcul lu dans Pretrait_Mascaret
! Lecture des mots du dictionnaires
    MOTINT            , &
    MOTREA            , &
    MOTCAR            , &
    ADRESS            , &
! Traitement des erreurs
    Erreur            )


!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       03/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture des lois temporelles d'evolution de concentration
!  --------    (pour CL et apports)
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele : LecFicLoiConc
!  ---------------------
!
!=========================================================================

use M_FICHIER_T           ! Definition du type FICHIER_T
use M_LOI_CONC_T          ! Definition du type LOI_CONC_T
use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
use M_LecFicLoiConc_I     ! Interface de sous-programme

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none


! Variables d'entree
  type(FICHIER_T)          , intent(inout) :: FicLoiConc

  integer           , dimension(:)  , intent(in   ) :: MOTINT
  real(DOUBLE)      , dimension(:)  , intent(in   ) :: MOTREA
  character(LEN=144), dimension(:)  , intent(in   ) :: MOTCAR
  integer           , dimension(:,:), intent(in   ) :: ADRESS

  integer              , intent(in   ) :: UniteListing
  logical              , intent(in   ) :: ImpressionLoiConc
  integer              , intent(in   ) :: CritereArret
  real(DOUBLE)         , intent(in   ) :: TempsMaximum

! Variables de sortie
  integer                           , intent(  out) :: NbLoiConc
  type(LOI_CONC_T)  , dimension(:)  , pointer       :: LoiConc


! Variables locales
  integer        :: ModeLoiConc ! Mode de saisie des lois relatives aux vases
  integer        :: iLoi        ! Indice des lois
  integer        :: NbPts       ! Nombre de points decrivant la loi
  integer        :: iPts,i      ! Indice des points decrivant la loi
  integer        :: UniteTemps  ! unite de temps des lois
  character(72)  :: txt         ! Chaine de caractere temporaire


! Traitement des erreurs
  integer     :: retour      ! Code de retour de la fonction read
!  character(132) :: arbredappel_old  ! Arbre d'appel initial  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur


!=========================================================================

End Subroutine LecLoiConc

End Interface

End Module M_lecLoiConc_I

