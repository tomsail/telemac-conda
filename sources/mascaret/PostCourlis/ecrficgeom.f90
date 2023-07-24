Subroutine  EcrFicGeom  (  &

    FicResuGeom    ,  & ! Fichier de la geometrie finale
    NbProfil       ,  & ! nombre de profils
    ProfilCourlis  ,  & ! Profils sedimentaire
    NbInterface    ,  & ! nombre de d'interfaces sedimentaires
    TitreCas       ,  & ! Titre du cas de calcul
    Temps          ,  & ! Temps du calcul
    Erreur         )    ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Ecriture du fichier contenant des profils geometriques
!  --------    des differentes interfaces sedimentaires a la fin du calcul
 !            Ce fichier sert de geometrie initiale dans le cas d'une
!              suite de calcul
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!=========================================================================
!  Commentaire : Description du fichier :
!  -----------
!   Ligne 1 :  MOT_CLE, NomBief, NomProfil, abscisse profil.
!        sans blanc dans les noms : le blanc est le caractere separateur
!        Ex : <MOT_CLE bief1 profil1 340.3>
!
!   Ligne 2 et jusqu'au prochain mot-cle MOT ou la fin de fichier :
!        (T, Z1, Z2, Z3, ...)
!        Exemple :
!        <  0. 2000.  2005. 2010. >
!        < 10. 3000.  3005. 3010. >
!        <100. 4000.  4005. 4010. >
!        etc.
!=========================================================================

!=========================================================================
!   DECLARATIONS
!=========================================================================

use M_PRECISION            ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info
use M_PARAMETRE_C          ! Definition des constante tq EPS*, W0, ...

use M_FICHIER_T            ! Definition du type FICHIER_T
use M_PROFIL_COURLIS_T     ! Definition du type PROFIL_COURLIS

use M_ERREUR_T             ! Definition du type ERREUR_T
use M_MESSAGE_C            ! Messages d'erreur
use M_TRAITER_ERREUR_I     ! Traitement de l'errreur

!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Constantes
  integer, parameter    :: LEN_CHAINE  = 80

! Variables d'entree
  type(FICHIER_T), intent(in   ) :: FicResuGeom

  integer, intent(in   ) :: NbProfil
  integer, intent(in   ) :: NbInterface

  type(PROFIL_COURLIS_T), dimension(:  ), intent(in   ) :: ProfilCourlis

  character(LEN_CHAINE), intent(in   ) :: TitreCas
  real(DOUBLE),          intent(in   ) :: Temps

! Variables de sortie

! Variables locales
  integer :: i,j,k       ! Compteurs
  integer :: Unite       ! Unite du fichier de sedimentation a ecrire

! Traitement des erreurs
  integer        :: retour       ! code de retour des fonctions d'e/s
!  character(132) :: arbredappel_old   ! ancien arbre  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================
!  INITIALISATION
!=========================================================================

  Erreur%Numero       = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel  = trim(Erreur%arbredappel)//'=>EcrFicGeom'

  Unite = FicResuGeom%Unite

!=========================================================================
! Ouverture du fichier a lire
!=========================================================================

  open(unit=Unite       , file=FicResuGeom%Nom, access='SEQUENTIAL', &
       action='WRITE'   , form='FORMATTED'    , iostat=RETOUR      , &
       position='rewind', status='OLD'        )

  If (RETOUR /= 0) Then
    Erreur%Numero = 4
    Erreur%ft   = err_4
    Erreur%ft_c = err_4c
    call TRAITER_ERREUR (Erreur, FicResuGeom%Nom)
    return
  End if

!=========================================================================
!  ECRITURE DE L'EN-TETE
!=========================================================================

  write(Unite, 1000) TitreCas, Temps

!=========================================================================
!  ECRITURE DES CONCENTRATIONS
!=========================================================================

  Do i = 1, NbProfil
    write(Unite, 1010) ProfilCourlis(i)%Nom, ProfilCourlis(i)%Abs
    Do j = 1, ProfilCourlis(i)%NbPoint ! PU2017: Modif des bornes pour la prise en compte des points fixes des profils
      write(Unite, 1020) ProfilCourlis(i)%X(j),  &
                        (ProfilCourlis(i)%Z(k,j), k=1, NbInterface)
    Enddo
  Enddo

!=========================================================================
! Fermeture du fichier
!=========================================================================
  close(Unite)

!  Erreur%arbredappel = arbredappel_old

  return

!=========================================================================
! FORMATS
!=========================================================================

 1000 format('# Resultats du calcul ', A30, ' au temps', F18.3, ' secondes')
 1010 format('Profil Bief1 ', A20, F8.2)
! 1020 format(F14.2, NbInterface(' ',F8.3))
 1020 format(F14.2, 7(' ',F8.3))

!=========================================================================
!  FIN DU SOUS-PROGRAMME
!=========================================================================
End Subroutine EcrFicGeom
