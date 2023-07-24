Subroutine LimiteDepot (  &

  LimiteDepotG       , & ! numero du point au dela duquel il peut y a voir depot ou erosion (1er pt immerge)
  LimiteDepotD       , & ! numero du dernier pt pour lequel il peut y a voir depot ou erosion (dernier pt immerge)
  Zsurf              , & ! Cote de la surface libre
  ProfilCourlis      , & ! Profils sedimentaires
  NbProfils          , & ! Nombre de profils
  Erreur             )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Calcul les indices des bornes RG et RD (LimitedepotG et LimiteDepotD)
!  --------   de la partie immergee de chaque profil.
!             Il ne peut y avoir depot - erosion qu'entre ces points.
!
!  Sous-programme appelant : Courlis
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!=========================================================================

use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'erreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  integer, intent(in) :: NbProfils

  real(DOUBLE),           dimension(:), intent(in   ) :: Zsurf
  type(PROFIL_COURLIS_T), dimension(:), intent(in   ) :: ProfilCourlis

! Variables de sortie
  integer,                dimension(:), intent(  out) :: LimiteDepotG, LimiteDepotD

! Variables locale
  integer :: i, j

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
!  character(132) :: arbredappel_old   ! Ancien arbre d'appel  ! PU2017 : Mise en commentaire

!=========================================================================

!=========================================================================
! Initialisations
!=========================================================================
  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LimiteDepot'

!=========================================================================
! Calcul des points de berges
!=========================================================================

  Do i = 1, NbProfils
    LimiteDepotG(i) = 1
    LimiteDepotD(i) = ProfilCourlis(i)%NbPoint
  Do j = 1, ProfilCourlis(i)%NbPoint - 1
    If ((ProfilCourlis(i)%Z(1,j) < Zsurf(i)) .and. &
       (ProfilCourlis(i)%Z(1,j+1)) >= Zsurf(i)) Then
      !mjcor LimiteDepotD = j
      LimiteDepotD(i) = j
    Endif
    If ((ProfilCourlis(i)%Z(1,j) >= Zsurf(i)) .and. &
       (ProfilCourlis(i)%Z(1,j+1)) < Zsurf(i)) Then
      !mjcor LimiteDepotG = j + 1
      LimiteDepotG(i) = j + 1
    Endif
  Enddo
     ! PU2017
!     If (LimiteDepotG(i).eq.LimiteDepotD(i)) then
!       print *,"Lim G = Lim D"
!       read *
!     endif
  Enddo

!=========================================================================
!  Erreur%arbredappel = arbredappel_old

  return

End Subroutine LimiteDepot
