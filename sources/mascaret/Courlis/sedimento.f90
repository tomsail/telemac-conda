Subroutine Sedimento (  &

  QVaseCouche    ,  & ! Flux de depot des vases par couche (> 0 depot, < 0 erosion)
  QSableCouche   ,  & ! Flux de depot des sables par couche (> 0 depot, < 0 erosion)
  TauH           ,  & ! Contrainte hydraulique locale (depend du tirant d'eau local)
  TauHMoy        ,  & ! Contrainte hydraulique moyenne dans la section
  TauHMax        ,  & ! Contrainte hydraulique maximale dans la section
  TauE           ,  & ! Contrainte hydraulique effective (depend du rayon hydr.)
  TauEMoy        ,  & ! Contrainte hydraulique effective moyenne ds section
  TauEMax        ,  & ! Contrainte hydraulique effective maximale ds section
  Ceq            ,  & ! Concentration d'equilibre des sables locale
  CeqMoy         ,  & ! Concentration d'equilibre des sables moyenne dans la section
  DeltaH         ,  & ! Variation de hauteur sedimentaire en chaque point des profils
  ProfilCourlis  ,  & ! Profils sedimentaires
  NbProfil       ,  & ! Nombre de profils
  CoucheSed      ,  & ! Parametres sedimentaires des differentes couches
  NbCouche       ,  & ! Nombre de couches
  CVase          ,  & ! Concentration des vases en suspension
  CSable         ,  & ! Concentration des sables en suspension
  Dt             ,  & ! Pas de temps
  Zsurf          ,  & ! Cote de la surface libre
  Vitesse        ,  & ! Vitesse moyenne par section
  SurfMouil      ,  & ! Surface mouillee
  PerimMouil     ,  & ! Perimetre mouille
  LimiteDepotG   ,  & ! numero du point au dela duquel il peut y a voir depot ou erosion (1er pt immerge)
  LimiteDepotD   ,  & ! numero du dernier pt pour lequel il peut y a voir depot ou erosion (dernier pt immerge)
  LimiteSable    ,  & ! % de sable a partir duquel la couche est traitee suivant les lois du sable
  CalcSable      ,  & ! choix calcul avec sable
  Erreur         )

!*************************************************************************
!  PROGICIEL : COURLIS           P. UNG
!
!  VERSION : 8.2       2017  Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Calcul des flux sedimentaire a l'equilibre, resolution de
!  --------   l'equation d'Exner (avec un decentrement adaptatif selon
!             le regime de l'ecoulement amont-aval) et calcul d'evolution
!             des fonds
!
!  Sous-programme appelant :
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!  Commentaires :
!  ------------
!
!=========================================================================

use M_PRECISION         ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C       ! Definition des constante tq EPS*, W0, ...
use M_PROFIL_COURLIS_T  ! Definition du type PROFIL_COURLIS
use M_COUCHE_T          ! Definition du type COUCHE_T

use M_ERREUR_T          ! Type ERREUR_T
use M_MESSAGE_C         ! Messages d'erreur
use M_TRAITER_ERREUR_I  ! Traitement de l'erreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE)        :: Dt
  integer, intent(in) :: NbProfil
  integer, intent(in) :: NbCouche
  logical             :: CalcSable

  type(COUCHE_T),         dimension(:) , intent(in   )  :: CoucheSed
  real(DOUBLE),           dimension(:) , intent(in   )  :: CVase, CSable
  real(DOUBLE),           dimension(:) , intent(in   )  :: Zsurf, Vitesse
  real(DOUBLE),           dimension(:) , intent(in   )  :: SurfMouil
  real(DOUBLE),           dimension(:) , intent(in   )  :: PerimMouil
  real(DOUBLE)                         , intent(in   )  :: LimiteSable
  integer,                dimension(:) , intent(in   )  :: LimiteDepotG
  integer,                dimension(:) , intent(in   )  :: LimiteDepotD
  type(PROFIL_COURLIS_T), dimension(:) , intent(inout)  :: ProfilCourlis

! Variables de sortie
  real(DOUBLE), dimension(:,:), intent(inout) :: QVaseCouche, QSableCouche
  real(DOUBLE), dimension(:,:), intent(  out) :: TauH, TauE, Ceq
  real(DOUBLE), dimension(:)  , intent(  out) :: TauHMoy, TauHMax
  real(DOUBLE), dimension(:)  , intent(  out) :: TauEMoy, TauEMax
  real(DOUBLE), dimension(:)  , intent(  out) :: CeqMoy
  real(DOUBLE), dimension(:,:), intent(  out) :: DeltaH

! Variables locales
  integer                                 :: i, j, k, kk
  integer                                 :: NPt                                      ! Nombre de point de la section en travers i
  real(DOUBLE)                            :: Rho, RhoS, W52, W14                      ! constantes du calcul
  real(DOUBLE)                            :: V, Sm, Rh, Rh43, Rh13, Zsl, H            ! variables hydrauliques du profil
  real(DOUBLE)                            :: Cv, Cs                                   ! concentrations en suspension au profil i
  real(DOUBLE)                            :: dxj                                      ! pas d'abscisse angulaire du profil
  real(DOUBLE)                            :: Kp, Kt, Tce, M, d50, Cf, W, Tcd, Ps      ! parametres sedimentaires de la couche affleurante
  real(DOUBLE)                            :: TaH, TauHMa, TauHMo                      ! variables intermediaires de calcul
  real(DOUBLE)                            :: TaE, TauEMo, TauEMa                      ! variables intermediaires de calcul
  real(DOUBLE)                            :: Qv, Ce, CeqMo                            ! variables intermediaires de calcul
  real(DOUBLE)                            :: Flux                                     ! variables intermediaires de calcul
  real(DOUBLE)                            :: Depo, HDepo, VolDep                      ! variables intermediaires de calcul (flux, volume, hauteur de depot)
  real(DOUBLE)                            :: Eros, HEros                              ! variables intermediaires de calcul (flux, hauteur d'erosion)
  real(DOUBLE)                            :: VolPot                                   ! Volume erodable potentiel (si la couche n'est pas limitee en epaisseur)
  real(DOUBLE)                            :: VolDis                                   ! Volume de sediment disponible pour l'erosion
  real(DOUBLE)                            :: z_ref                                    ! point bas des interfaces sedimentaires
  logical                                 :: Vaseux                                   ! a vrai si la couche suit les lois des vases
  real(DOUBLE), dimension(:), allocatable :: Xj                                       ! abscisse transversale du profil en cours et nombre de Froude
  integer     , dimension(:), allocatable :: Couche                                   ! numero de la 1ere couche non vide en chaque point de profil
  real(DOUBLE), dimension(:), allocatable :: ErosPot                                  ! flux d'erosion potentiel en chaque pt du profil

! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  integer                       :: retour            ! Code de retour de la fonction read, allocate
  character(132)                :: arbredappel_old   ! Ancien arbre d'appel

!=========================================================================

!=========================================================================
! Initialisations
!=========================================================================

  Erreur%Numero = 0
  arbredappel_old    = trim(Erreur%arbredappel)
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>Sedimento'

! Initialisations des constantes
! ------------------------------
  Rho  = 1000._DOUBLE
  RhoS = 2600._DOUBLE
  W52  = 2.5_DOUBLE
  W14  = 0.25_DOUBLE
  Tcd  = CoucheSed(1)%TauCD

!=========================================================================
! Boucle sur les profils
!=========================================================================
  Do i = 2, NbProfil-1

    ! Initialisations
    TauHMa = W0
    TauHMo = W0
    TauEMa = W0
    TauEMo = W0
    CeqMo  = W0

    ! Allocation de memoire des tableaux locaux
    ! -----------------------------------------
    NPt = ProfilCourlis(i)%NbPoint

    allocate(Xj(NPt),STAT=retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'Xj')
!      return
    Endif

    allocate(Couche(NPt),STAT=retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'Couche')
!      return
!      stop 123
    Endif

    allocate(ErosPot(NPt),STAT=retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'ErosPot')
!      return
    Endif

    ! Conditions hydrauliques dans la section
    V = Vitesse(i)
    Sm = SurfMouil(i)
    If (Sm < W0) Then
      Erreur%Numero = 415
      Erreur%ft   = err_415
      Erreur%ft_c = err_415c
      call TRAITER_ERREUR (Erreur, i)
!      return
    Endif
    Rh = max(Sm/PerimMouil(i), EPS8)
    Rh43 = Rh**(4._DOUBLE/3._DOUBLE)
    Rh13 = Rh**(1._DOUBLE/3._DOUBLE)
    Zsl = Zsurf(i)

    Cv = CVase(i)
    Cs = CSable(i)

 ! Initialisation des variables aux points de la section en travers
    Do j = 1,NPt
      TauH(j,i)  = W0
      TauE(j,i)  = W0
      Ceq(j,i)   = W0
      ErosPot(j) = W0
      Couche(j)  =  0

      !Recherche de la premiere couche non vide MS2018
      Do k = 1, NbCouche
        If ((ProfilCourlis(i)%Z(k,j)-ProfilCourlis(i)%Z(k+1,j)) > EPS5) Then
          Couche(j) = k
          goto 10
        Endif
      Enddo
      10   continue
      Xj(j) = ProfilCourlis(i)%X(j)
    Enddo

    ! Calcul des contraintes de frottement
    ! ------------------------------------
    Do j = LimiteDepotG(i), LimiteDepotD(i)

      k = Couche(j)
      If (k /= 0) Then
        Kp  = CoucheSed(k)%Kp
        Kt  = CoucheSed(k)%Kt
        d50 = CoucheSed(k)%D50
      Else
        Kp  = CoucheSed(1)%Kp
        Kt  = CoucheSed(2)%Kt
        d50 = CoucheSed(2)%D50
      Endif

      ! Contrainte de frottement pour les vase
      ! MS2018 ==> contrainte efficace avec un alpha = 2 (? a confirmer)
      H = max(W0, Zsl - ProfilCourlis(i)%Z(1,j))
      !TaH = Rho * Gpes * H * V*V / (Kp*Kp * Rh43)
      TaH = Rho * Gpes  * V*V / (Kp*Kp * Rh13)

      ! Contrainte et concentration d'equilibre pour les sables
      !chb_27-02-2008   TaE = Rho * Gpes * V*V / (Kt*Kt * Rh13) * (Kt/Kp)
      !chb_27-02-2008 Engelund-Hansen prend en compte une contrainte totale et non efficace
      !MS2018 ==> alpha = 0 dans ce cas (? a confirmer)
      TaE = Rho * Gpes * V*V / (Kt*Kt * Rh13)
      !MS2019 glute pour le cas d50 = 0, probleme lecture d50 dans fichier cas ?
      If (d50.GT.0.0) Then
        Qv  = 0.05_DOUBLE * sqrt(d50 / Gpes / 1.6_DOUBLE) * &
              Kt * Kt * Rh13 / (Rho*Gpes) * &
              (TaE**W52) / (((RhoS-Rho) * Gpes * d50)**W32)
      Else
        !WRITE(*,*) j, d50, k, couche(j)
        Qv  = 0.0
      Endif

      If (ABS(Sm * V) .LT. EPS8) Then
        Ce = 0.0
      Else
        Ce = RhoS * Qv * (Xj(LimiteDepotD(i))-Xj(LimiteDepotG(i))) / (Sm*V)
      Endif

      TauH(j,i) = TaH
      !If (TaH > TauHMa) TauHMa = TaH
      !TauHMo = TauHMo + TaH

      TauE(j,i) = TaE
      Ceq(j,i) = Ce

      If (TaE > TauEMa) TauEMa = TaE

      TauEMo = TauEMo + TaH
      CeqMo = CeqMo + Ce
    ! fin boucle sur les points du profil - calcul des contraintes
    Enddo

    TauHMax(i) = TauHMa
    ! MS2020: a changer si developpement
    TauHMoy(i) = TauHMo / dble(LimiteDepotD(i)-LimiteDepotG(i)+1)
    ! Fin MS2020
    TauEMax(i) = TauEMa
    TauEMoy(i) = TauEMo / dble(LimiteDepotD(i)-LimiteDepotG(i)+1)
    CeqMoy(i)  = CeqMo  / dble(LimiteDepotD(i)-LimiteDepotG(i)+1)

    ! Traitement du depot
    ! -------------------

    ! Cas des sables
    if (CalcSable) then
      Do j = LimiteDepotG(i), LimiteDepotD(i)
        Depo  = W0 !chb
        HDepo = W0 !chb
        If (Cs > Ceq(j,i)) Then
          If (j == 1) Then
            dxj = (Xj(2) - Xj(1)) / W2
          Else If (j == NPt) Then
            dxj = (Xj(NPt) - Xj(NPt-1)) / W2
          Else
            dxj = (Xj(j+1) - Xj(j-1)) / W2
          Endif

          Cf = CoucheSed(2)%Cfond
          W  = CoucheSed(2)%Wc

          Flux   = W * (Cs - Ceq(j,i))
          Depo   = -Flux * dxj
          VolDep = -Depo * Dt / Cf
          HDepo  = VolDep / dxj

          ProfilCourlis(i)%Z(2,j) = ProfilCourlis(i)%Z(2,j) + HDepo
          ProfilCourlis(i)%Z(1,j) = ProfilCourlis(i)%Z(1,j) + HDepo
          DeltaH(j,i) = DeltaH(j,i) + HDepo
          QSableCouche(2,i) = QSableCouche(2,i) + Depo

        Endif
      ! fin boucle cas des sables
      Enddo
    Endif

    ! Cas des vases
    Do j = LimiteDepotG(i), LimiteDepotD(i)

      Depo  = W0 !chb
      HDepo = W0 !chb

      If (TauH(j,i) <= Tcd) Then

        If (j == 1) Then
          dxj = (Xj(2) - Xj(1)) / W2
        Else If (j == NPt) Then
          dxj = (Xj(NPt) - Xj(NPt-1)) / W2
        Else
          dxj = (Xj(j+1) - Xj(j-1)) / W2
        Endif

        Cf = CoucheSed(1)%Cfond
        W  = CoucheSed(1)%Wc

        Flux   = W * Cv * (W1 - (TauH(j,i) / Tcd))
        Depo   = - max(W0, Flux * dxj)
        VolDep = -Depo * Dt / Cf
        HDepo  = VolDep / dxj

        ProfilCourlis(i)%Z(1,j) = ProfilCourlis(i)%Z(1,j) + HDepo
        DeltaH(j,i) = DeltaH(j,i) + HDepo
        QVaseCouche(1,i) = QVaseCouche(1,i) + Depo

      Endif
    ! Fin boucle - cas des vases
    Enddo

    ! Traitement de l'erosion - determination de l'erosion potentielle
    ! ----------------------------------------------------------------
    Do j = LimiteDepotG(i), LimiteDepotD(i)

      If (j == 1) Then
        dxj = (Xj(2) - Xj(1)) / W2
      Else If (j == NPt) Then
        dxj = (Xj(NPt) - Xj(NPt-1)) / W2
      Else
        dxj = (Xj(j+1) - Xj(j-1)) / W2
      Endif

      k      = Couche(j)
      Vaseux = .true.
      If (k /= 0 ) Then
        If (CoucheSed(k)%Psable > LimiteSable) Vaseux = .false.
      Endif

      If (Vaseux) Then ! Erosion des vases
        If (k /= 0 ) Then
          Tce = CoucheSed(k)%TauCE
          M   = CoucheSed(k)%Mpart

        Else

          If ((j > 1 ) .and. (Couche(j-1) /= 0)) Then
            Tce = CoucheSed(Couche(j-1))%TauCE
            M   = CoucheSed(Couche(j-1))%Mpart
          Else If ((j < NPt) .and. (Couche(j+1) /= 0)) Then
            Tce = CoucheSed(Couche(j+1))%TauCE
            M   = CoucheSed(Couche(j+1))%Mpart
          Else
            Tce = 1.e+6_DOUBLE
            M   = W0
          Endif

        Endif

        If (TauH(j,i) >= Tce) Then
          Flux = M * (TauH(j,i) / Tce - W1)
          ErosPot(j) =  Flux * dxj
        Endif

      Else ! Erosion des sables

        W = CoucheSed(k)%Wc

        If (Cs < Ceq(j,i)) Then
          Flux = W * (Ceq(j,i) - Cs)
          ErosPot(j) = Flux * dxj
        Endif

      Endif
    ! fin boucle sur les points du profil - erosion potentielle
    Enddo

    ! Traitement de l'erosion - calcul de l'erosion reelle
    ! ----------------------------------------------------
    Do j = LimiteDepotG(i), LimiteDepotD(i)

      Eros  = W0 !chb
      VolPot = W0 !chb
      HEros  = W0 !chb

      k = Couche(j)

      If (k /= 0) Then

        If (j == 1) Then
          dxj  = (Xj(2) - Xj(1)) / W2
          Eros = W23 * ErosPot(1)   + W13 * ErosPot(2)
        Else If (j == NPt) Then
          dxj  = (Xj(NPt) - Xj(NPt-1)) / W2
          Eros = W23 * ErosPot(NPt) + W13 * ErosPot(NPt-1)
        Else
          dxj  = (Xj(j+1) - Xj(j-1)) / W2
          Eros = W12 * ErosPot(j)   + W14 * (ErosPot(j-1) + ErosPot(j+1))
        Endif

        Cf = CoucheSed(k)%Cfond
        Ps = CoucheSed(k)%Psable

        VolPot = Eros / Cf * Dt
        VolDis = (ProfilCourlis(i)%Z(k,j) - ProfilCourlis(i)%Z(k+1,j)) * dxj

        If (VolPot > VolDis)   VolPot = VolDis ! l'erosion est limitee par l'epaisseur de la couche

        HEros = VolPot / dxj
        QVaseCouche(k,i)  = QVaseCouche(k,i)  + VolPot * Cf * &
                            (W1 - Ps / 100._DOUBLE) / Dt
        QSableCouche(k,i) = QSableCouche(k,i) + VolPot * Cf * &
                            (Ps / 100._DOUBLE) / Dt

        Do kk = 1,k
          ProfilCourlis(i)%Z(kk,j) = ProfilCourlis(i)%Z(kk,j) - HEros
        Enddo

        DeltaH(j,i) = DeltaH(j,i) - HEros

      Endif
    !fin boucle sur les points du profil - calcul erosion reelle
    Enddo

    ! Mise a jour des points bas des interfaces sedimentaires
    Do k = 1, NbCouche+1
      z_ref = INFINI
      Do j = 1,NPt
        z_ref = min(z_ref,ProfilCourlis(i)%Z(k,j))
      End do
      ProfilCourlis(i)%Zref(k) = z_ref
    Enddo

    ! Deallocation de memoire des tableaux locaux
    ! -------------------------------------------
    deallocate (Xj, Couche, ErosPot)
    ! fin boucle sur les profils
  Enddo

    !=========================================================================
!  Erreur%arbredappel = arbredappel_old

  return

End Subroutine Sedimento
