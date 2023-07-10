Subroutine  TestGeom  ( &

    NbInterfaces  , & ! Nb d'interfaces
    NbProf        , & ! Nombre de sections
    NbProfCourlis , & ! Nombre de sections des profils de Courlis
    Profil        , & ! Nombre de profils dans le champ "profil"
    ProfilCourlis , & ! Profils des interfaces de couche de sediments
    Erreur        )   ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!  Modifiee le 21/10/2003  par  Ch. Bertier
!  Suppression du doublement des points extremes des profils
!
!=========================================================================
!  Fonction : Verification de la correcte stratification des interfaces
!  --------   Verification de la coherence de la geometrie de COURLIS et
!              de celle de l'hydraulique
!             Verification de l'abscence de lit majeur
!
!  Sous-programme appelant : LecGeomCourlis
!  -----------------------
!
!  Sous-programme appele : TRAITER_ERREUR
!  ---------------------
!
!=========================================================================


!=========================================================================
! DECLARATIONS
!=========================================================================

use M_PRECISION
use M_FICHIER_T        ! Definition du type FICHIER_T
use M_PROFIL_COURLIS_T ! Definition du type PROFIL_COURLIS
use M_PROFIL_T         ! Definition du type PROFIL
use M_PARAMETRE_C      ! Parametres numeriques

use M_ERREUR_T         ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I ! Traitement de l'erreur

!.. Implicit Declarations ..
  implicit none


! Variables d'entree
  integer            , intent(in   ) :: NbInterfaces
  integer            , intent(in   ) :: NbProf
  integer            , intent(in   ) :: NbProfCourlis

  type(PROFIL_T)        , dimension(:), pointer :: Profil
  type(PROFIL_COURLIS_T), dimension(:), pointer :: ProfilCourlis

! Variables locales
  integer          :: iProf            ! Compteur sur les profils
  integer          :: iPoint           ! Compteur sur les points d'un profil
  integer          :: iInterf          ! Compteur sur les interfaces de couches
  integer          :: NbPoints         ! Nombre de points d'un profil
  integer          :: NbPointsCourlis  ! Nombre de points d'un profil Courlis
!  character(30)    :: NomProf          ! Nom du profil  ! PU2017 : Mise en commentaire
!  character(30)    :: NomBief          ! Nom du bief  ! PU2017 : Mise en commentaire
  real(DOUBLE)     :: Abs              ! Abscisse relative du profil
!  real(DOUBLE)     :: CoteMax          ! Cote extreme maximum d'un profil  ! PU2017 : Mise en commentaire
!  logical          :: LimMin1,LimMin2  ! Tests de Limites inferieures  ! PU2017 : Mise en commentaire

! Traitement des erreurs
!  character(132)  :: arbredappel_old  ! Arbre d'appel initial  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!==========================================================================

! Arbre d'erreur
  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>TestGeom'

!==========================================================================
! Verification entre les parametres de Courlis et de Mascaret
!==========================================================================

! Verification de correspondance des nombres de profils
!------------------------------------------------------
  If (NbProf /= NbProfCourlis) Then
    Erreur%Numero  = 400
    Erreur%ft  = err_400
    Erreur%ft_c  = err_400c
    call TRAITER_ERREUR (Erreur, NbProfCourlis, NbProf)
  Endif

! Boucle sur les profils
!-----------------------
  boucle_profils: Do iProf = 1, NbProfCourlis

    NbPoints    = size(Profil    (iProf)%X(:))
    NbPointsCourlis = size(ProfilCourlis(iProf)%X(:))

    ! Verification de correspondance des nombres de points pour chaque profil
    !chb-21.10.03  if(NbPoints /= NbPointsCourlis) then
    If (NbPoints /= NbPointsCourlis + 2) Then
      Erreur%Numero  = 401
      Erreur%ft  = err_401
      Erreur%ft_c  = err_401c
      call TRAITER_ERREUR (Erreur,iProf,NbPointsCourlis,NbPoints)
    Endif

    ! Verification des correspondances des abscisses curvilignes
    If (abs(Profil(iProf)%AbsAbs-ProfilCourlis(iProf)%Abs)> EPS1) Then
      Erreur%Numero  = 402
      Erreur%ft  = err_402
      Erreur%ft_c  = err_402c
      call TRAITER_ERREUR (Erreur,iProf)
      return
    Endif

    ! Verification de l'absence de lit majeur
    !chb-21.10.03  if(  Profil(iProf)%LimiteMin(1)/=2        .and. &
    !chb-21.10.03    Profil(iProf)%LimiteMin(2)/=NbPointsCourlis-1) then
    If ((Profil(iProf)%LimiteMin(1)/=2) .and. &
       (Profil(iProf)%LimiteMin(2)/=NbPoints-1)) Then
      Erreur%Numero  = 405
      Erreur%ft  = err_405
      Erreur%ft_c  = err_405c
      call TRAITER_ERREUR (Erreur,iProf)
    Endif

    ! Boucle sur les points du profil iProf
    boucle_points: Do iPoint = 1, NbPointsCourlis

      ! Verification des correspondances des abscisses angulaires
      !chb-21.10.03  if(abs(Profil(iProf)%X(iPoint)-ProfilCourlis(iProf)%X(iPoint)) > EPS2) then
      If (abs(Profil(iProf)%X(iPoint+1) - ProfilCourlis(iProf)%X(iPoint)) &
          > EPS2) Then
        Erreur%Numero  = 403
        Erreur%ft  = err_403
        Erreur%ft_c  = err_403c
        call TRAITER_ERREUR (Erreur,iProf,iPoint)
      Endif

      ! Verification des correspondances des cotes pour l'interface 1
      !chb-21.10.03  if(abs(Profil(iProf)%Y(iPoint)-ProfilCourlis(iProf)%Z(1,iPoint)) > EPS2) then
      If (abs(Profil(iProf)%Y(iPoint+1)-ProfilCourlis(iProf)%Z(1,iPoint)) &
          > EPS2) Then
        Erreur%Numero  = 404
        Erreur%ft  = err_404
        Erreur%ft_c  = err_404c
        call TRAITER_ERREUR (Erreur,iProf,iPoint)
      Endif

      ! Verification que les interfaces ne se chevauchent pas
      Do iInterf = 1, NbInterfaces-1
        If (ProfilCourlis(iProf)%Z(iInterf,iPoint) < ProfilCourlis(iProf)%Z(iInterf+1,iPoint)) Then
          Erreur%Numero  = 406
          Erreur%ft  = err_406
          Erreur%ft_c  = err_406c
          call TRAITER_ERREUR (Erreur,iProf,iInterf,iInterf+1)
        Endif
      Enddo

    Enddo boucle_points

  Enddo boucle_profils

!  Erreur%arbredappel = arbredappel_old

  return

!=========================================================================
! FIN SOUS-PROGRAMME
!=========================================================================
End Subroutine TestGeom
