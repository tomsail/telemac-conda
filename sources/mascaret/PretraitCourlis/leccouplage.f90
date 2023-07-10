Subroutine  LecCouplage  ( &

  UniteListing        , & ! Unite du fichier d'impression des parametres
  ImpressionCouplage  , & ! Choix d'impression des parametres de couplage
  NbIterHydro         , & ! Nb d'iter. pour l'hydraulique entre 2 echanges
  NbIterSedim         , & ! Nb d'iter. pour la sedimento entre 2 echanges
! Lecture des mots du dictionnaires
  MOTINT              , &
  ADRESS              , &
! Traitement des erreurs
  Erreur              )   ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture des parametres de couplage entre la sedimento et
!  --------    l'hydraulique
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================


!=========================================================================
! DECLARATIONS
!=========================================================================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE
use M_FICHIER_T        ! Definition du type FICHIER_T

use M_ERREUR_T         ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I ! Traitement de l'erreur

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  integer, dimension(:)  , intent(in   ) :: MOTINT
  integer, dimension(:,:), intent(in   ) :: ADRESS
  integer                , intent(in   ) :: UniteListing
  logical                , intent(in   ) :: ImpressionCouplage


! Variables de sortie
  integer         , intent(  out) :: NbIterHydro
  integer         , intent(  out) :: NbIterSedim

! Variables locales
!  integer :: IdxPoint      ! Index du caractere '.'  ! PU2017 : Mise en commentaire

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
!  character(132) :: arbredappel_old    ! Arbre d'appel initial  ! PU2017 : Mise en commentaire

!=========================================================================
! INITIALISATION
!=========================================================================

  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LecCouplage'

  If (ImpressionCouplage)  write(UniteListing,1000)

!=========================================================================
! ENREGISTREMENT DES PARAMETRES
!=========================================================================

! Nombre d'iterations hydr. entre 2 echanges
  NbIterHydro = MOTINT(ADRESS(1,610))
  If (ImpressionCouplage)  write(UniteListing,1010) NbIterHydro

! Nombre d'iteration Courlis entre 2 echanges
  NbIterSedim = MOTINT(ADRESS(1,611))
  If (ImpressionCouplage)  write(UniteListing,1020) NbIterSedim

!  Erreur%arbredappel = arbredappel_old

!=========================================================================
! FORMATS
!=========================================================================

  1000 format (/,'PARAMETRES DE COUPLAGE',/, &
               &   '----------------------',/)
  1010 format ("Nb d'iter. pour l'hydr.  entre 2 echanges : ",I3)
  1020 format ("Nb d'iter. pour la sedim entre 2 echanges : ",I3)

!=========================================================================
! FIN DU SOUS-PROGRAMME
!=========================================================================
End Subroutine LecCouplage
