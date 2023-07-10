Module M_LecFicLoiConc_I

Interface

Subroutine  LecFicLoiConc  ( &

    FicLoiConc      , & ! Fic. contenant une evolution temporelle de conc
    LoiConc0        , & ! Concentration initiale des traceurs
    UniteTemps      , & ! unite de temps des chroniques temporelles
    NumLoi          , & ! Numero de loi
    Erreur          )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture du fichier contenant une loi de type temps-concentration
!  --------
!
!  Sous-programme appelant : LecLoiConc
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!=========================================================================
!
!  Commentaires : Description du fichier lu
!  ------------
!  Toute ligne de commentaire commence par le caractere "#"
!
!  Exemple :
!
!    #ESSAI
!    #18/12/1995
!    # Concentration en vase (g/l)
!    #    T        C
!    10.00     1.00
!    20.00     1.10
!    30.00     1.20
!    35.00     1.00
!    46.00     1.10
!    54.00     1.20
!
! Exemple :
!
!    #ESSAI
!    #18/12/1995
!    # courbe de tarage
!    0.00     1.000
!    4.00     1.876
!    # 1er pic
!    20.00    5.500
!    ...
!=========================================================================


!============================ Declarations ===============================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE
use M_LIRE_CHAINE_S    ! Lecture de lignes de commentaire du fichier
use M_FICHIER_T        ! Definition du type FICHIER_T
use M_LOI_CONC_T       ! Definition du type LOI_CONC_T

use M_ERREUR_T         ! Definition du type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I ! Traitement de l'errreur

!.. Implicit Declarations ..
  implicit none

! Constantes
  integer     , parameter :: LEN_CHAINE         = 80
  character(1), parameter :: CHAINE_COMMENTAIRE = "#" ! Caractere commentaire qui
                                                      ! debute une ligne commentaire

! Variables d'entree
  type(FICHIER_T) , intent(in   ) :: FicLoiConc
  integer         , intent(in   ) :: NumLoi

! Variables de sortie
  type(LOI_CONC_T), intent(  out) :: LoiConc0
  integer         , intent(  out) :: UniteTemps

! Variables locales
  integer    :: iPts          ! Compteur de points
  integer    :: NbPts         ! Nombre de points decrivant la loi
  integer    :: UniteLoi      ! Unite logique du fichier des lois
  integer    :: rang          ! position du mot cle sur la ligne
  character(72)  :: txt       ! chaine de caractere temporaire

  character(LEN_CHAINE) :: chaine      ! Chaine contenant une ligne du fichier
!  character(LEN_CHAINE) :: chaine_opt  ! chaine au format opthyca  ! PU2017 : Mise en commentaire
!  character(LEN_CHAINE) :: chaine_fort ! chaine convertie au format fortran  ! PU2017 : Mise en commentaire
  character(1)          :: ChaineVar


! Traitement des erreurs
  integer        :: retour            ! code de retour des fonctions d'e/s
!  character(132) :: arbredappel_old   ! ancien arbre  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================

EndSubroutine LecFicLoiConc

End Interface

End Module M_LecFicLoiConc_I

