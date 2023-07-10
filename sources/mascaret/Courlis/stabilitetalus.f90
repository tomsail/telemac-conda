Subroutine StabiliteTalus  (  &

  ProfilCourlis      ,  & ! Profils sedimentaires
  Talus              ,  & ! Parametres relatifs aux talus
  Dt                 ,  & ! Pas de temps
  Zsurf              ,  & ! Cote de la surface libre
  CoucheSed          ,  & ! Parametres sedimentaires des differentes couches
  DeltaH             ,  & ! Variation de hauteur sedimentaire en chaque point des profils
  Resini             ,  & ! Resistance initiale des blocs au mouvement
  SurPl              ,  & ! MS2020: a priori variable de sortie pour savoir si le point de profil a subi une evolution lie a la stabilite de talus
  Erreur             )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER
!
!  VERSION : 4.0       07/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Etablie la stabilite des berges et calcule la forme du lit
!  --------    apres glissement via une matrice de diffusion
!
!  Sous-programme appelant : Courlis
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
use M_TRAITER_ERREUR_I     ! Traitement de l'errreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE) :: Dt    ! PU2017 : Mise en commentaire de Temps

  real(DOUBLE)      , dimension(:)  , intent(in   )  :: Zsurf
  type(COUCHE_T)    , dimension(:)  , intent(in   )  :: CoucheSed
  type(TALUS_T)                     , intent(in   )  :: Talus
  real(DOUBLE)      , dimension(:,:), intent(in   )  :: Resini

! Variables de sortie
  type(PROFIL_COURLIS_T), dimension(:)  , intent(inout)  :: ProfilCourlis
  real(DOUBLE)          , dimension(:,:), intent(inout)  :: DeltaH
  integer               , dimension(:,:), intent(inout)  :: SurPl

! Variables locales
  integer :: i, j, k            ! Compteurs
  integer :: j1, jg, jd, jj, kk ! Compteurs
  integer :: NbProfil           ! Nombre de profils
  integer :: NInt               ! Nombre d'interfaces sedimentaires
  integer :: NPt                ! Nombre de points decrivant le profil en travers

  real(DOUBLE) :: ImG, ImD                ! Variable a 1 si le point gauche (resp. droit) est dans l'eau, 0 sinon
!  real(DOUBLE) :: HeG, HeD, HeM           ! Hauteurs de la colonne d'eau au dessus du bloc  ! PU2017 : Mise en commentaire
  real(DOUBLE) :: HsG, HsD                ! Hauteurs de la colonne sedimentaire (gauche, droite, moyenne)
                                          ! PU2017 : Mise en commentaire de HsM
  real(DOUBLE) :: HsGC, HsDC              ! Hauteurs de la couche en cours  ! Mise en commentaire de HsMC
  real(DOUBLE) :: HsC                     ! H auteur de sediment dans la couche traitee
  real(DOUBLE) :: Dx, DxG, DxD            ! Pas de discretisation horizontaux
  real(DOUBLE) :: DzS                     ! Pas de discretisation vertical, hypothenuse a la surface du bloc  ! PU2017 : Mise en commentaire DxzS
!  real(DOUBLE) :: DzF, DxzF              ! Pas de discretisation vertical, hypothenuse du fond du bloc  ! PU2017 : Mise en commentaire
!  real(DOUBLE) :: SinS, CosS, SinF, CosF  ! Angles du bloc  ! PU2017 : Mise en commentaire
  real(DOUBLE) :: Pente                   ! pente de la surface du bloc (= tanS)
!  real(DOUBLE) :: P, W, EG, ED, Mot      ! forces motrices exercees sur le bloc  ! PU2017 : Mise en commentaire
!  real(DOUBLE) :: Res                    ! forces de resistance au mouvement  ! PU2017 : Mise en commentaire
!  real(DOUBLE) :: Sec                    ! facteur de securite  ! PU2017 : Mise en commentaire
  real(DOUBLE) :: CG, CD                  ! facteur de la matrice "lumpee"
  real(DOUBLE) :: Ch                      ! facteur reducteur de Lam

  real(DOUBLE) :: z_ref                   ! point bas des interfaces sedimentaires

  real(DOUBLE), dimension(:,:), allocatable :: Z         ! Cotes des interfaces sedimentaires
  real(DOUBLE), dimension(:  ), allocatable :: Stab      ! Vecteur de stabilite des elements
  real(DOUBLE), dimension(:)  , allocatable :: A, C      ! Vecteur de la matrice
  real(DOUBLE), dimension(:)  , allocatable :: Lam       ! Parametre pour adapter la vitesse du glissement
  real(DOUBLE), dimension(:)  , allocatable :: Dz        ! Vecteur de correction de la cote des interfaces
  real(DOUBLE), dimension(:,:), allocatable :: DzPart    ! Deformation des couches sedimentaires

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  integer                       :: retour  ! Code de retour de la fonction read, allocate
!  character(132) :: arbredappel_old    ! Ancien arbre d'appel  ! PU2017 : Mise en commentaire

! Constantes
!  real(DOUBLE), parameter :: HsMin  = 5 * EPS3  ! Hauteur de couche minimale  ! PU2017 : Mise en commentaire
  real(DOUBLE), parameter :: EpsH   = EPS8

!=========================================================================

  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>StabiliteTalus'


!=========================================================================
! Initialisation
!=========================================================================

  NbProfil = size(ProfilCourlis)
  NInt     = size(CoucheSed) + 1

!=========================================================================
! Boucle sur les profils
!=========================================================================
  Do i = 1, NbProfil

    NPt = size(ProfilCourlis(i)%X)

!=========================================================================
! Allocations des tableaux locaux dependant de NPt
!=========================================================================
    Allocate(A(NPt),STAT=retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'A')
      return
    Endif

    Allocate(C(NPt),STAT=retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'A')
      return
    Endif

    Allocate(Z(NInt, NPt),STAT=retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'Z')
      return
    Endif

    Allocate(DzPart(NInt,NPt),STAT=retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'DzPart')
      return
    Endif

    Allocate(Stab(NPt),STAT=retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'Stab')
      return
    Endif

    Allocate(Lam(NPt),STAT=retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'Lam')
      return
    Endif

    Allocate(Dz(NPt),STAT=retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'Dz')
      return
    Endif


    ! Initialisation
    Z(:,:)      = ProfilCourlis(i)%Z(:,:)
    DzPart(:,:) = W0
    Stab(:)     = W1 !!! ajout mj


    Do j = 1, NPt-1

    ! Determination si le segment est immerge ou emerge
      ImG = W1
      ImD = W1
      If (Z(1,j)   > Zsurf(i)) ImG = W0
      If (Z(1,j+1) > Zsurf(i)) ImD = W0


    ! Calcul des distances et angles du bloc
      HsG = Z(1,j)   - Z(NInt,j)
      HsD = Z(1,j+1) - Z(NInt,j+1)
!      HsM = (HsG + HsD) / W2  ! PU2017 : Mise en commentaire

      Dx   = ProfilCourlis(i)%X(j+1) - ProfilCourlis(i)%X(j)
      DzS  = Z(1,j+1) - Z(1,j)

      Pente = DzS / Dx

    ! Si la pente est superieure a la pente limite de stabilite, il y a instabilite
    !MS2018: modif erreur test ==> ne prenait pas en compte la pente emmergee (cf. PN suspension 2018)
!     If (((ImD-W1) < EPS1) .and. ((ImG-W1) < EPS1)) Then
      If ((ImD > W0) .and. (ImG > W0)) Then
        If (Abs(Pente) > Talus%PstabI) Then
          Stab(j) = W0
        endif
      Else
        If (Abs(Pente) > Talus%PstabE) Then
          Stab(j) = W0
        endif

      Endif

      If ((dble(SurPl(j,i)) - Stab(j)) > 0.99_DOUBLE)  SurPl(j,i) = 0

      If (j == 1) Then
        DxG = W0
        DxD = ProfilCourlis(i)%X(j+2) - ProfilCourlis(i)%X(j+1)
      Elseif (j == NPt-1) Then
        DxG = ProfilCourlis(i)%X(j)   - ProfilCourlis(i)%X(j-1)
        DxD = W0
      Else
        DxG = ProfilCourlis(i)%X(j)   - ProfilCourlis(i)%X(j-1)
        DxD = ProfilCourlis(i)%X(j+2) - ProfilCourlis(i)%X(j+1)
      Endif

      CG = Dx / (W12 * (Dx + DxG))
      CD = Dx / (W12 * (Dx + DxD))

      C(j)   = (W1 - Stab(j)) * W16 * CG
      A(j+1) = (W1 - Stab(j)) * W16 * CD

    Enddo ! fin de la boucle sur les points du profil

    C(NPt) = W0
    A(1)   = W0

    ! Calcul de Dz, la deformation de la couche
    Do k = NInt-1, 1, -1

      Do j = 1, NPt-1

        HsGC = Z(k,j)   - Z(NInt,j)
        HsDC = Z(k,j+1) - Z(NInt,j+1)
        HsG  = Z(1,j)   - Z(NInt,j)
        HsD  = Z(1,j+1) - Z(NInt,j+1)

        If ((HsG + HsD) > EPS3) Then
          Ch = (HsGC + HsDC) / (HsG + HsD)
        Else
          Ch = W1
        Endif

        Lam(j) = 1 * Ch
        !Lam(j) = Talus%Lambda * Ch

        If (j == 1) Then ! Modif 1
          Dz(j) = C(j) * (Z(k,j+1) - Z(k,j)) * Lam(j)      !???
        Else
          Dz(j) = A(j) * (Z(k,j-1) - Z(k,j)) * Lam(j-1) + &
                  C(j) * (Z(k,j+1) - Z(k,j)) * Lam(j)
        Endif

      Enddo

      Ch = W1 !???
      !Lam(NPt) = Talus%Lambda * Ch !???
      Lam(NPt) = 1 * Ch
      Dz(NPt)  = A(NPt) * (Z(k,NPt-1) - Z(k,NPt)) * Lam(NPt-1) !???

      ! Le deplacement de la couche est limitee a HsC (hauteur de sediment dans la couche traitee)
      j1 = 1

10    Do j = j1, NPt

        HsC = Z(k,j) - Z(k+1,j)

        If (Abs(HsC + Dz(j)) < EPS8) Then

          Dz(j) = - HsC

        Elseif (((HsC + Dz(j)) < -EPSH) .and. (Dz(j) < -EPSH)) Then

          If (j == 1) Then
            jg = j
            jd = j
          Elseif (j == NPt) Then
            jg = j - 1
            jd = j - 1
          Else
            jg = j - 1
            jd = j
          Endif

          Lam(jg) = Lam(jg) * HsC / (-Dz(j))
          Lam(jd) = Lam(jg)
          If (j == NPt) Lam(NPt) = Lam(NPt-1)  !chb

          Do jj = jg, jd+1
            If (jj == 1) Then      ! Modif 2
              Dz(jj) = C(jj) * (Z(k,jj+1) - Z(k,jj)) * Lam(jj)    !???
            Elseif (jj == NPt) Then
              Dz(jj) = A(jj) * (Z(k,jj-1) - Z(k,jj)) * Lam(jj-1)  !???
            Else
              Dz(jj) = A(jj) * (Z(k,jj-1) - Z(k,jj)) * Lam(jj-1) + &
                       C(jj) * (Z(k,jj+1) - Z(k,jj)) * Lam(jj)
            Endif
          Enddo

          If ((HsC + Dz(j)) < W0)   Dz(j) = -HsC

          j1 = jg

          Goto 10

        Endif

      Enddo


  !   Calcul de Dz definitif
      Do j = 1, NPt
        Do kk = k, 1, -1  ! le deplacement de la couche k entraine une deformation des couches superieures
          If (j == 1) Then      ! Modif 3
            DzPart(kk,j) = C(j) * (Z(kk,j+1) - Z(kk,j)) * Lam(j)  !???
          Elseif (j == NPt) Then
            DzPart(kk,j) = A(j) * (Z(kk,j-1) - Z(kk,j)) * Lam(j-1)  !???
          Else
            DzPart(kk,j) = A(j) * (Z(kk,j-1) - Z(kk,j)) * Lam(j-1) + &
                           C(j) * (Z(kk,j+1) - Z(kk,j)) * Lam(j)
          Endif

        Enddo
      Enddo

      Do j = 1, NPt
        DeltaH(j,i) = DeltaH(j,i) + DzPart(1,j)
        Do kk = k, 1, -1
          Z(kk,j) = max(Z(kk+1,j), Z(kk,j) + DzPart(kk,j))
        Enddo
      Enddo


    Enddo  ! fin de la boucle sur les couches

    ! Mise a jour de la geometrie
      ProfilCourlis(i)%Z(:,:) = Z(:,:)

    Do k = 1, NInt
      z_ref = INFINI
      Do j = 1,NPt
      z_ref = min(z_ref,ProfilCourlis(i)%Z(k,j))
      End do
      ProfilCourlis(i)%Zref(k) = z_ref
    End do

!=========================================================================
!  Deallocation ds tableaux locaux
!=========================================================================
    Deallocate (A, C)
    Deallocate (Z, DzPart)
    Deallocate (Stab, Lam, Dz)
!
  Enddo ! fin de la boucle sur les profils

!  Erreur%arbredappel = arbredappel_old

  return

End Subroutine StabiliteTalus
