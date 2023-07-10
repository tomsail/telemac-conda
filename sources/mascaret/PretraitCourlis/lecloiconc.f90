Subroutine  lecLoiConc  ( &

  UniteListing      , & ! Unite logique fichier listing
  ImpressionLoiConc , & ! Choix d'impression des lois de concentration
  FicLoiConc        , & ! Fichier de l'evolut temporelle de conc en vase
  NbLoiConc         , & ! Nombre de lois de concentration
  LoiConc           , & ! Structure de donnnees des lois de concentration
  CritereArret      , & ! Critere d'arret des calculs
  TempsMaximum      , & ! Temps maximum de calcul lu dans Pretrait_Mascaret
! Lecture des mots du dictionnaires
  MOTINT            , &
  MOTREA            , &
  MOTCAR            , &
  ADRESS            , &
! Traitement des erreurs
  Erreur            )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       03/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture des lois temporelles d'evolution de concentration
!  --------   (pour CL et apports)
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele : LecFicLoiConc
!  ---------------------
!
!=========================================================================

use M_FICHIER_T           ! Definition du type FICHIER_T
use M_LOI_CONC_T          ! Definition du type LOI_CONC_T
use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
use M_LecFicLoiConc_I     ! Interface de sous-programme

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none


! Variables d'entree
  type(FICHIER_T)          , intent(inout) :: FicLoiConc

  integer           , dimension(:)  , intent(in   ) :: MOTINT
  real(DOUBLE)      , dimension(:)  , intent(in   ) :: MOTREA
  character(LEN=144), dimension(:)  , intent(in   ) :: MOTCAR
  integer           , dimension(:,:), intent(in   ) :: ADRESS

  integer              , intent(in   ) :: UniteListing
  logical              , intent(in   ) :: ImpressionLoiConc
  integer              , intent(in   ) :: CritereArret
  real(DOUBLE)         , intent(in   ) :: TempsMaximum

! Variables de sortie
  integer                           , intent(  out) :: NbLoiConc
  type(LOI_CONC_T)  , dimension(:)  , pointer       :: LoiConc


! Variables locales
  integer        :: ModeLoiConc ! Mode de saisie des lois relatives aux vases
  integer        :: iLoi        ! Indice des lois
  integer        :: NbPts       ! Nombre de points decrivant la loi
  integer        :: iPts,i      ! Indice des points decrivant la loi
  integer        :: UniteTemps  ! unite de temps des lois
  character(72)  :: txt         ! Chaine de caractere temporaire


! Traitement des erreurs
  integer     :: retour      ! Code de retour de la fonction read
!  character(132) :: arbredappel_old  ! Arbre d'appel initial  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur


!=========================================================================
! INITIALISATION
!=========================================================================

  Erreur%Numero = 0
!  arbredappel_old = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LecLoiConc'

  UniteTemps = -1   ! pour tester l'attribution d'une valeur

!=========================================================================
! ENTREE DU NOMBRE DE LOIS ET ALLOCATION DE MEMOIRE DU CHAMP DES LOIS
!=========================================================================

  if (ImpressionLoiConc) write(UniteListing,2000)

  ! Nombre de lois
  !---------------
  NbLoiConc= MOTINT(ADRESS(1,616))
  If (NbLoiConc < 1) Then
    Erreur%Numero = 408
    Erreur%ft   = err_408
    Erreur%ft_c = err_408c
    call TRAITER_ERREUR  (Erreur, 'Le nombre de lois', 'au moins egal a 1')
    return
  Elseif (NbLoiConc > 15) Then
    Erreur%Numero = 408
    Erreur%ft   = err_408
    Erreur%ft_c = err_408c
    call TRAITER_ERREUR  (Erreur, 'Le nombre de lois', '< ou egal a 15')
    return
  Endif

  ! Allocation de memoire du tableau des lois de concentration
  !-----------------------------------------------------------
  allocate (LoiConc(NbLoiConc), STAT = retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR  (Erreur, 'LoiConc')
    return
  Endif

  If (ImpressionLoiConc) write(UniteListing,2001)  NbLoiConc

!=========================================================================
! BOUCLE SUR LES LOIS DE CONCENTRATIONS
!=========================================================================

  boucle_loi : Do iLoi = 1, NbLoiConc

    ! Nom de la loi
    !--------------
    LoiConc(iLoi)%Nom  = MOTCAR(ADRESS(4,607+iLoi-1))(1:30)

    ! Mode de saisie de la loi
    !-------------------------
    ModeLoiConc = MOTINT(ADRESS(1,617+iLoi-1))

    !=====================================================================
    ! TRAITEMENT DES DONNEES EN FONCTION DU MODE DE SAISIE
    !=====================================================================
    ModeSaisie: If (ModeLoiConc == SAISIE_PAR_FICHIER) Then
    ! SAISIE PAR FICHIER => APPEL DU SS-PROG LecFicLoiconc
    ! ------------------------------------------------------

      ! Nom du fichier contenant les donnees des lois de concentrations
      FicLoiConc%Nom = MOTCAR(ADRESS(4,622+iLoi-1))

      If (ImpressionLoiConc) Then
        write(UniteListing,2002) iLoi,                    &
                                 trim(LoiConc(iLoi)%Nom), &
                                 trim(FicLoiConc%Nom)
      Endif

      ! Appel du sous-programme traitant la lecture du fichier
      call LecFicLoiConc (FicLoiConc    , &
                          LoiConc(iLoi) , &
                          UniteTemps    , &
                          iLoi          , &
                          Erreur        )

      if (Erreur%numero /= 0) return

      NbPts = size(LoiConc(iLoi)%Temps)

    ElseIf (ModeLoiConc == SAISIE_PAR_CLAVIER) Then
    ! SAISIE AU CLAVIER => ENREGISTREMENT A PARTIR DES MOTS-CLES
    ! SAUVEGARDEES PAR L'IHM
    ! ----------------------------------------------------------

      If (ImpressionLoiConc) Then
        write(UniteListing,2003) iLoi,trim(LoiConc(iLoi)%Nom)
      Endif

      ! Unite de la variable Temps
      UniteTemps = MOTINT(ADRESS(1,647 + iLoi - 1))

      ! Nombre de points decrivant la loi
      NbPts = MOTINT(ADRESS(1,632+iLoi-1))

      ! Controle du nombre de points minimum
      !-------------------------------------
      write(txt,'(a,i3)') 'Nombre de Pts decrivant la loi ',iLoi
      If (NbPts < 2) Then
        Erreur%Numero = 408
        Erreur%ft   = err_408
        Erreur%ft_c = err_408c
        call TRAITER_ERREUR  (Erreur,trim(txt),'> ou egal a 2')
        return
      Endif

      ! Allocation de memoire du tableau des temps
      !-----------------------------------------------------------
      allocate (LoiConc(iLoi)%Temps(NbPts), STAT = retour)
      If (retour /= 0) Then
        Erreur%Numero = 5
        Erreur%ft   = err_5
        Erreur%ft_c = err_5c
        call TRAITER_ERREUR  (Erreur, 'LoiConc%Temps')
        return
      Endif

      ! Allocation de memoire du tableau des concentrations
      !-----------------------------------------------------------
      allocate (LoiConc(iLoi)%Conc(NbPts), STAT = retour)
      If (retour /= 0) Then
        Erreur%Numero = 5
        Erreur%ft   = err_5
        Erreur%ft_c = err_5c
        call TRAITER_ERREUR  (Erreur, 'LoiConc%Conc')
        return
      Endif

      ! Boucle sur le nombre de points decrivant la loi "iLoi"
      !-----------------------------------------------------------
      Do iPts = 1, NbPts
        ! Concentration en MES
        LoiConc(iLoi)%Conc(iPts)  = MOTREA(ADRESS(2,637+iLoi-1) + iPts-1)
        ! en fonction du temps
        LoiConc(iLoi)%Temps(iPts)  = MOTREA(ADRESS(2,622+iLoi-1) + iPts-1)
      Enddo

    Else
    ! TRAITEMENT DES ERREURS SUR LE MODE DE SAISIE
    ! --------------------------------------------

      Erreur%Numero = 410
      Erreur%ft   = err_410
      Erreur%ft_c = err_410c
      call TRAITER_ERREUR  (Erreur, 'les lois de concentrations')
      return

    Endif ModeSaisie    ! FIN CHOIX MODE DE SAISIE

    ! Verification sur l'unite de temps
    If (UniteTemps <= 0 .or. UniteTemps > LOI_UNITE_NB_MAX) Then
      Erreur%Numero = 316
      Erreur%ft   = err_316
      Erreur%ft_c = err_316c
      call TRAITER_ERREUR(Erreur,'Concentration', iLoi, LOI_UNITE_NB_MAX)
      return
    Endif

    ! Passage du temps en secondes
    If (UniteTemps /= LOI_UNITE_SECONDE) Then

      Select case (UniteTemps)
        Case (LOI_UNITE_MINUTE)
          Do i = 1, NbPts
            LoiConc(iLoi)%Temps(i) = LoiConc(iLoi)%Temps(i) * 60.
          End do
        Case (LOI_UNITE_HEURE)
          Do i = 1, NbPts
            LoiConc(iLoi)%Temps(i) = LoiConc(iLoi)%Temps(i) * 3600.
          End do
        Case (LOI_UNITE_JOUR)
          Do i = 1, NbPts
            LoiConc(iLoi)%Temps(i) = LoiConc(iLoi)%Temps(i) * 86400.
          End do
      End select

    Endif  ! de unite de temps

    ! Impression de l'unite de la variable Temps apres conversion
    If (ImpressionLoiConc) write(UniteListing,2006) 'SECONDE'

    ! Coherence avec le nombre de pas de temps de la simulation
    If ((CritereArret == TEMPS_MAXIMUM) .and. &
       (LoiConc(iLoi)%Temps(NbPts) < TempsMaximum)) Then
      Erreur%Numero = 416
      Erreur%ft   = err_416
      Erreur%ft_c = err_416c
      call TRAITER_ERREUR (Erreur, iLoi, trim(LoiConc(iLoi)%Nom))
      return
    Endif

    ! Impression de la loi
    If (ImpressionLoiConc) Then
      write(UniteListing,2004) iLoi,'Temps','Conc'
      Do iPts = 1, NbPts
        write(UniteListing,2005) iPts,                      &
                                 LoiConc(iLoi)%Temps(iPts), &
                                 LoiConc(iLoi)%Conc(iPts)
      End do
    Endif

  End do boucle_loi      ! FIN DE LA BOUCLE SUR LES LOIS

!  Erreur%arbredappel = arbredappel_old

  return

!=========================================================================
!  FORMAT D'ECRITURE
!=========================================================================

  2000  Format ('2',//,'LOIS DE CONCENTRATION',/,24('-'))
  2001  Format (/,'Nombre de lois : ',I3)
  2002  Format ('Loi No. : ', i3, ', Nom : ', A, /,     &
                'Mode de saisie par fichier. Nom du fichier : ', A)
  2003  Format ('Loi No. : ', i3, ', Nom : ', A, /,     &
                'Mode de saisie par clavier.')
  2006  Format ('Unite de temps     = ', A)
  2004  Format ('Loi No. ', i3, 2A12)
  2005  Format (i11,2F12.2)

!=========================================================================
! FIN SOUS-PROGRAMME
!=========================================================================
End Subroutine LecLoiConc
