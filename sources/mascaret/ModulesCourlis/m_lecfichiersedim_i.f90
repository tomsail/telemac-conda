Module M_LecFichierSedim_I

Interface

Subroutine  LecFichierSedim  ( &

    FichierSedim               , & ! Fichier contenant les donnees sedim. rel. a ch. couche + donnees concernant la stabilite des berges
    NbCouche                   , & ! Nb de couches sedimentaires
    CoucheSed                  , & ! variable contenant ttes les donnees rel. a ch. couche
    Talus                      , & ! variable contenant ttes les donnees rel. aux talus
    LimiteSable                , & ! % de sable a part. dql la couche est traitee suivant les lois du sable
    CnuxV                      , & ! Coefficient de diffusion vases
    CnuxS                      , & ! Coefficient de diffusion sables
    !ConsConv                   , & ! parametres schema de convection
    Erreur                     )


!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL - M. Jodeau
!
!  VERSION : 5.1       08-2009    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture du fichier contenant les parametres des couches
!  --------    sedimentaires constituant le lit du bief
!
!  Sous-programme appelant : LecParamSedim
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================


!=========================== Declarations ================================

use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
use M_LIRE_CHAINE_S       ! lecture de lignes de commentaire du fichier
use M_FICHIER_T           ! Definition du type FICHIER_T
use M_COUCHE_T            ! Definition du type COUCHE_T
use M_TALUS_T             ! Definition du type TALUS_T
use M_CONSTANTES_TRACER_T ! parametres schema convection

use M_ERREUR_T            ! Definition du type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(FICHIER_T)        , intent(in   ) :: FichierSedim

! Variables de sortie
  integer                 , intent(  out)    :: NbCouche
  type(TALUS_T)           , intent(  out)    :: Talus
  real(DOUBLE)            , intent(  out)    :: LimiteSable
  real(DOUBLE)            , intent(  out)    :: CnuxV
  real(DOUBLE)            , intent(  out)    :: CnuxS
!  type(CONSTANTES_TRACER_T), intent(out) :: ConsConv

  type(COUCHE_T), dimension(:), pointer :: CoucheSed

! Constantes
  integer, parameter :: LEN_CHAINE = 80

! Variables locales
  integer        :: UniteSedim        ! Unite du fichier de sedimentation a lire
  character(72)  :: txt, DesignVar
  integer        :: iCouche           ! indice de couche
  integer        :: NumCouche         ! Numero de couche

  character(LEN_CHAINE) :: chaine     ! Chaine contenant une ligne du fichier

  character(1), parameter :: CHAINE_COMMENTAIRE = "#" ! Caractere commentaire qui debute une ligne commentaire

! Traitement des erreurs
  integer         :: retour(8)        ! Code de retour de la fonction read
  integer         :: retour0
!  character(132) :: arbredappel_old    ! ancien arbre  ! PU2017 : Mis en commentaire
  type(ERREUR_T), intent(inout) :: Erreur


!=========================================================================

End Subroutine LecFichierSedim

End Interface

End Module M_LecFichierSedim_I

