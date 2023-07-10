Subroutine  SurfaceLibre( &

    NbProfil            , & ! Nombre de profils du tableau "ProfilCourlis"
    PtRiveG             , & ! ABSCISSE DU POINT DE LA DROITE A LA HAUTEUR Zsurf
    PtRiveD             , & ! ABSCISSE DU POINT DE LA DROITE A LA HAUTEUR Zsurf
    ProfilCourlis       , & ! Profils geom. des rivieres, lus dans COURLIS
    Zsurf               , & ! Cote de la surface libre
    Erreur              )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       05/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Determine les points de rencontre RG et RD des berges et
!  --------    de la surface libre pour chaque profil en travers
!
!  Sous-programme appelant : StockPTransCourlis
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE

use M_PROFIL_COURLIS_T ! Definition du type PROFIL_COURLIS

use M_ERREUR_T         ! Type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I ! Traitement de l'errreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(PROFIL_COURLIS_T), dimension(:), intent(in) :: ProfilCourlis
  real(DOUBLE)          , dimension(:), intent(in) :: Zsurf
  integer                             , intent(in) :: NbProfil

! Variables de sortie
  real(DOUBLE)          , dimension(:), pointer    :: PtRiveG, PtRiveD

! Variables locales
  real(DOUBLE) :: EpsI = 0.1   ! Valeur de tolerance
  real(DOUBLE) :: X1, X2, Z1, Z2
  real(DOUBLE) :: AErod, BErod
!  real(DOUBLE) :: NbPts        ! Nb de points du profil 'i'
  integer      :: NbPts        ! Nb de points du profil 'i'  ! PU2017 : Changement de format
  integer      :: i,j

! Traitement des erreurs
  integer                         :: Retour
  type(ERREUR_T)  , intent(inout) :: Erreur
!  character(132)                  :: arbredappel_old  ! PU2017 : Mise en commentaire

!=========================================================================
! INITIALISATIONS
!=========================================================================

  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>SurfaceLibre'

!=========================================================================
! Allocation de memoire du tableau des points de rive (G et D)
!=========================================================================

  allocate (PtRiveG(NbProfil),STAT = retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'PtRiveG')
    return
  Endif

  allocate (PtRiveD(NbProfil),STAT = retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'PtRiveD')
    return
  Endif

!=========================================================================
! DEFINITION DES RIVES
!=========================================================================

  EpsI = 0.1

  Label_Profils : Do i = 1, NbProfil

    NbPts = ProfilCourlis(i)%NbPoint

    ! Rive Gauche
    ! -----------
    PtRiveG(i) = ProfilCourlis(i)%X(1)

    Do j = 1, NbPts-1
      If ((Zsurf(i) <= ProfilCourlis(i)%Z(1,j  )) .AND. &
          (Zsurf(i) >  ProfilCourlis(i)%Z(1,j+1))      )  Then

        X1 = ProfilCourlis(i)%X(j)
        X2 = ProfilCourlis(i)%X(j+1)
        Z1 = ProfilCourlis(i)%Z(1,j)
        Z2 = ProfilCourlis(i)%Z(1,j+1)

        ! ------------------------------------------------------
        ! EQUATION DE LA DROITE SEDIMENT JOIGNANT (i,j) ¡ (i,j+1)
        !    Zsurf=AErod*DX+BErod
        ! -------------------------------------------------------
        ! this shift is done for superposed abscissae
        If (X2 .EQ. X1) Then
          X2 = X1 + 10E-15
        Endif
        AErod = (Z2    - Z1)    / (X2-X1)
        BErod = (X2*Z1 - X1*Z2) / (X2-X1)

        ! PtRiveG EST L'ABSCISSE DU POINT DE LA DROITE A LA HAUTEUR Zsurf
        PtRiveG(i) = (Zsurf(i)-BErod) / AErod

      Endif
    Enddo

    ! Rive Droite
    ! -----------
    PtRiveD(i)=ProfilCourlis(i)%X(NbPts)

    Do j = 1, NbPts-1

      If ((Zsurf(i) >  ProfilCourlis(i)%Z(1,j  )).AND. &
          (Zsurf(i) <= ProfilCourlis(i)%Z(1,j+1))     )  Then

        X1 = ProfilCourlis(i)%X(j)
        X2 = ProfilCourlis(i)%X(j+1)
        Z1 = ProfilCourlis(i)%Z(1,j)
        Z2 = ProfilCourlis(i)%Z(1,j+1)

        ! -------------------------------------------------------
        ! EQUATION DE LA DROITE SEDIMENT JOIGNANT (i,j) ¡ (i,j+1)
        !   Zsurf=AErod*DX+BErod
        ! -------------------------------------------------------
        ! this shift is done for superposed abscissae
        If (X2 .EQ. X1) Then
          X2 = X1 + 10E-15
        Endif
        AErod = (Z2    - Z1)    / (X2-X1)
        BErod = (X2*Z1 - X1*Z2) / (X2-X1)

        ! PtRiveG EST L'ABSCISSE DU POINT DE LA DROITE A LA HAUTEUR Zsurf
        PtRiveD(i) = (Zsurf(i)-BErod) / AErod

      Endif
    Enddo

    !
    ! TEST POUR SAVOIR SI LES POINTS PtRiveG ET PtRiveD NE CORRESPONDENT PAS A
    ! DES NOEUDS EXISTANTS. (PROBLEME POUR LE FICHIER RUBENS)
    !
    Do j = 1, NbPts
      If (PtRiveG(i) == ProfilCourlis(i)%X(j)) PtRiveG(i) = PtRiveG(i) + EpsI
      If (PtRiveD(i) == ProfilCourlis(i)%X(j)) PtRiveD(i) = PtRiveD(i) - EpsI
    Enddo

  Enddo Label_Profils

!=========================================================================
! FIN DU SOUS-PROGRAMME
!=========================================================================

!  Erreur%arbredappel = arbredappel_old

End Subroutine SurfaceLibre
