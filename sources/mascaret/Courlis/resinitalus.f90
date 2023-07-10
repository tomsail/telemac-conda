Subroutine ResIniTalus  (  &

  ProfilCourlis    ,  & ! Profils sedimentaires
  Talus            ,  & ! Parametres relatifs aux talus
  CoucheSed        ,  & ! Parametres sedimentaires des differentes couches
  Resini           ,  & ! Resistance initiale des blocs au mouvement
  Erreur        )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Calcul de la resistance initiale des colonnes sedimentaires
!  --------    au mouvements
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_PRECISION            ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C          ! Definition des constante tq EPS*, W0, ...
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info

use M_PROFIL_COURLIS_T     ! Definition du type PROFIL_COURLIS
use M_COUCHE_T             ! Definition du type COUCHE_T
use M_TALUS_T              ! Definition du type TALUS_T

use M_ERREUR_T             ! Type ERREUR_T
use M_MESSAGE_C            ! Messages d'erreur
use M_TRAITER_ERREUR_I     ! Traitement de l'erreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(PROFIL_COURLIS_T), dimension(:)  , intent(in   )  :: ProfilCourlis
  type(COUCHE_T)        , dimension(:)  , intent(in   )  :: CoucheSed
  type(TALUS_T)                         , intent(in   )  :: Talus

! Variables de sortie
  real(DOUBLE)          , dimension(:,:), intent(  out)  :: ResIni

! Variables locales
  integer :: i, j         ! Compteurs  ! PU2017 : Mise en commentaire de k
  integer :: NProfil      ! Nombre de profils
  integer :: NInt         ! Nombre d'interfaces sedimentaires
  integer :: NPt          ! Nombre de points decrivant le profil en travers

  real(DOUBLE) :: HsG, HsD, HsM           ! Hauteurs de la colonne sedimentaire (gauche, droite, moyenne)
!  real(DOUBLE) :: HsGC, HsDC, HsMC        ! Hauteurs de la couche en cours  ! PU2017 : Mise en commentaire
  real(DOUBLE) :: Dx                      ! Pas de discretisation horizontal
!  real(DOUBLE) :: DzS, DxzS               ! Pas de discretisation vertical, hypothenuse a la surface du bloc  ! PU2017 : Mise en commentaire
  real(DOUBLE) :: DzF, DxzF               ! Pas de discretisation vertical, hypothenuse du fond du bloc
!  real(DOUBLE) :: SinS, CosS, SinF, CosF  ! Angles du bloc  ! PU2017 : Mise en commentaire

  real(DOUBLE), dimension(:,:), allocatable :: Z      ! Cotes des interfaces sedimentaires

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  integer                       :: retour  ! Code de retour de la fonction read, allocate
!  character(132) :: arbredappel_old      ! Ancien arbre d'appel  ! PU2017 : Mise en commentaire

! Constantes

!=========================================================================
! La variable ResIni est utile uniquement si on a choisi le modele de mecasol
!=========================================================================
  If (Talus%Modele /= MODELE_TALUS_GLISSEMENT) return

!=========================================================================
  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>ResIniTalus'

  NProfil = size(ProfilCourlis)
  NInt    = size(CoucheSed) + 1

! Boucle sur les profils
!=========================================================================
  Do i = 1, NProfil

! Allocations des tableaux locaux dependant de NPt et initialisation
!=========================================================================
    NPt = size(ProfilCourlis(i)%X)

    Allocate(Z(NInt, NPt),STAT=retour)
    If (retour /= 0) then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'Z')
      return
    End if

    Z(:,:) = ProfilCourlis(i)%Z(:,:)


    Do j = 1, NPt-1

    ! Calcul des distances et angles du bloc
      HsG = Z(1,j)   - Z(NInt,j)
      HsD = Z(1,j+1) - Z(NInt,j+1)
      HsM = (HsG + HsD) / W2

      Dx   = ProfilCourlis(i)%X(j+1) - ProfilCourlis(i)%X(j)
!      DzS  = Z(1,j+1) - Z(1,j)  ! PU2017 : Mise en commentaire

!      DxzS = sqrt(Dx**2 + DzS**2)  ! PU2017 : Mise en commentaire
      DzF  = Z(NInt,j+1) - Z(NInt,j)
      DxzF = sqrt(Dx**2 + DzF**2)

!      SinS = - DzS / DxzS  ! PU2017 : Mise en commentaire
!      CosS =   Dx  / DxzS  ! PU2017 : Mise en commentaire
!      SinF = - DzF / DxzF  ! PU2017 : Mise en commentaire
!      CosF =   Dx  / DxzF  ! PU2017 : Mise en commentaire

!      Do k = 1, NInt-1                ! PU2017 : Mise en commentaire
!        HsGC = Z(k,j)   - Z(k+1,j)    ! PU2017 : Mise en commentaire
!        HsDC = Z(k,j+1) - Z(k+1,j+1)  ! PU2017 : Mise en commentaire
!        HsMC = (HsGC + HsDC) / W2     ! PU2017 : Mise en commentaire
!      Enddo                           ! PU2017 : Mise en commentaire

    ! Calcul de la resistance initiale
      ResIni(j,i) = (1.468_DOUBLE * HsM + 0.162_DOUBLE) * DxzF !!!!Cette formule de la resistance initiale
                                !!!! n'est valable que pour Grangent (Loire)

    Enddo ! fin de la boucle sur les points du profil

!=========================================================================
!  Deallocation ds tableaux locaux
!=========================================================================
    Deallocate (Z)

  Enddo ! fin de la boucle sur les profils

!  Erreur%arbredappel = arbredappel_old

  return

End Subroutine ResIniTalus
