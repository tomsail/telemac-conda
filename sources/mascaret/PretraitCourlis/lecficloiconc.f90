Subroutine  LecFicLoiConc  ( &

  FicLoiConc      , & ! Fic. contenant une evolution temporelle de conc
  LoiConc0        , & ! Concentration initiale des traceurs
  UniteTemps      , & ! unite de temps des chroniques temporelles
  NumLoi          , & ! Numero de loi
  Erreur          )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture du fichier contenant une loi de type temps-concentration
!  --------
!
!  Sous-programme appelant : LecLoiConc
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!=========================================================================
!
!  Commentaires : Description du fichier lu
!  ------------
!  Toute ligne de commentaire commence par le caractere "#"
!
!  Exemple :
!
!    #ESSAI
!    #18/12/1995
!    # Concentration en vase (g/l)
!    #    T        C
!    10.00     1.00
!    20.00     1.10
!    30.00     1.20
!    35.00     1.00
!    46.00     1.10
!    54.00     1.20
!
!  Exemple :
!
!    #ESSAI
!    #18/12/1995
!    # courbe de tarage
!    0.00     1.000
!    4.00     1.876
!    # 1er pic
!    20.00    5.500
!    ...
!=========================================================================


!============================ Declarations ===============================

use M_PRECISION        ! Definition de la precision DOUBLE ou SIMPLE
use M_LIRE_CHAINE_S    ! Lecture de lignes de commentaire du fichier
use M_FICHIER_T        ! Definition du type FICHIER_T
use M_LOI_CONC_T       ! Definition du type LOI_CONC_T

use M_ERREUR_T         ! Definition du type ERREUR_T
use M_MESSAGE_C        ! Messages d'erreur
use M_TRAITER_ERREUR_I ! Traitement de l'errreur

!.. Implicit Declarations ..
  implicit none

! Constantes
  integer     , parameter :: LEN_CHAINE         = 80
  character(1), parameter :: CHAINE_COMMENTAIRE = "#" ! Caractere commentaire qui
                                                      ! debute une ligne commentaire

! Variables d'entree
  type(FICHIER_T) , intent(in   ) :: FicLoiConc
  integer         , intent(in   ) :: NumLoi

! Variables de sortie
  type(LOI_CONC_T), intent(  out) :: LoiConc0
  integer         , intent(  out) :: UniteTemps

! Variables locales
  integer    :: iPts          ! Compteur de points
  integer    :: NbPts         ! Nombre de points decrivant la loi
  integer    :: UniteLoi      ! Unite logique du fichier des lois
  integer    :: rang          ! position du mot cle sur la ligne
  character(72)  :: txt       ! chaine de caractere temporaire

  character(LEN_CHAINE) :: chaine      ! Chaine contenant une ligne du fichier
!  character(LEN_CHAINE) :: chaine_opt  ! chaine au format opthyca  ! PU2017 : Mise en commentaire
!  character(LEN_CHAINE) :: chaine_fort ! chaine convertie au format fortran  ! PU2017 : Mise en commentaire
  character(1)          :: ChaineVar


! Traitement des erreurs
  integer        :: retour            ! code de retour des fonctions d'e/s
!  character(132) :: arbredappel_old   ! ancien arbre  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!============================ Instructions ===============================

  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LecFicLoiConc'

!=========================================================================
! PREMIERE LECTURE DU FICHIER AFIN DE CONNAITRE LE NOMBRE DE LOIS
! ET DE DECELER LES ERREURS
!=========================================================================

  ! Ouverture du fichier a lire
  ! ---------------------------
  UniteLoi = FicLoiConc%Unite

  open(unit=UniteLoi, file=FicLoiConc%Nom, access='SEQUENTIAL', &
       action='READ'    , form='FORMATTED', iostat=retour     , &
       position='rewind', status='OLD'     )

  If (retour /= 0) Then
    Erreur%Numero = 3
    Erreur%ft   = err_3
    Erreur%ft_c = err_3c
    call TRAITER_ERREUR (Erreur, trim(FicLoiConc%Nom))
    return
  Endif


  ! LECTURE DE LA PREMIERE LIGNE (LES LIGNES DE COMMENTAIRE NE SONT PAS LUES)
  !--------------------------------------------------------------
  call LIRE_CHAINE_S (chaine, FicLoiConc, CHAINE_COMMENTAIRE, retour)

  If (retour /= 0) Then
  Erreur%Numero = 50
    Erreur%ft   = err_50
    Erreur%ft_c = err_50c
    call TRAITER_ERREUR (Erreur, trim(FicLoiConc%Nom))
    return
  Endif

  rang = scan(chaine,'SsMmHhJj')

  ! Message d'erreur si erreur a la ligne d'unite de temps
  If (rang == 0) Then
    Erreur%Numero = 379
    Erreur%ft   = err_379
    Erreur%ft_c = err_379c
    call TRAITER_ERREUR (Erreur, 'Concentration',trim(FicLoiConc%Nom))
    return
  Endif


  ! ===============================================================
  ! Definition du nombre de points devrivant la loi
  ! ===============================================================
  NbPts = 0

  ! lecture jusqu'a la fin du fichier
  LabelDimLois: Do While (retour == 0)

    NbPts = NbPts + 1
    call LIRE_CHAINE_S(chaine,             &
                       FicLoiConc,         &
                       CHAINE_COMMENTAIRE, &
                       retour)

  End do LabelDimLois

  ! Si Erreur de lecture ==> message d'erreur
  !------------------------------------------
  If (retour > 0) Then
    Erreur%Numero = 11
    Erreur%ft   = err_11
    Erreur%ft_c = err_11c
    call TRAITER_ERREUR (Erreur, trim(FicLoiConc%Nom), NbPts)
    return
  Endif


  ! Stockage du nombre de points decrivant la loi
  !---------------------------------------------
  NbPts = NbPts - 1


  ! Controle du nombre de points minimum
  !-------------------------------------
  write(txt,'(a,i3)') 'Nombre de Pts decrivant la loi ',NumLoi
  If (NbPts < 2) Then
    Erreur%Numero = 408
    Erreur%ft   = err_408
    Erreur%ft_c = err_408c
    call TRAITER_ERREUR (Erreur,trim(txt),'> ou egal a 2')
    return
  Endif


  ! Allocations de memoire du tableau de la variable "Temps"
  !---------------------------------------------------------
  allocate(LoiConc0%Temps(NbPts), STAT = retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'LoiConc0%Temps')
    return
  Endif

  ! Allocations de memoire du tableau de la variable "Conc"
  !---------------------------------------------------------
  allocate(LoiConc0%Conc (NbPts), STAT = retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'LoiConc0%Conc')
    return
  Endif

!=========================================================================
! 2-EME LECTURE : LECTURE EFFECTIVE DES LOIS DE CONCENTRATION
!=========================================================================

  ! Retour au debut du fichier
  rewind (UniteLoi)

  ! Le cas echeant, lecture de l'unite de temps
  call LIRE_CHAINE_S (chaine, FicLoiConc, CHAINE_COMMENTAIRE, retour)
  read(chaine(rang:rang+1),*,IOSTAT = retour) ChaineVar

  ! Si erreur de 2eme lecture de la ligne d'unite de temps ==> message d'erreur
  !----------------------------------------------------------------------------
  If (retour /= 0) Then
  Erreur%Numero = 380
    Erreur%ft   = err_380
    Erreur%ft_c = err_380c
    call TRAITER_ERREUR (Erreur, 'Concentration', trim(FicLoiConc%Nom))
    return
  Endif

  ! Stockage de l'unite de la variable 'Temps' donnee en saisie
  !-------------------------------------------------------
  Select case (ChaineVar)
    Case ("S","s")
      UniteTemps = 1
    Case ("M","m")
      UniteTemps = 2
    Case ("H","h")
      UniteTemps = 3
    Case ("J","j")
      UniteTemps = 4
  End select

  !  Lecture des points decrivant la loi de concentration
  !-------------------------------------------------------
  Do iPts = 1, NbPts

    !Lecture
    call LIRE_CHAINE_S (chaine, FicLoiConc, CHAINE_COMMENTAIRE, retour)
    read(chaine,*,IOSTAT = retour) LoiConc0%Temps(iPts), LoiConc0%Conc(iPts)

    ! Traitement d'erreur
    If (retour /= 0) Then
      Erreur%Numero = 11
      Erreur%ft   = err_11
      Erreur%ft_c = err_11c
      call TRAITER_ERREUR (Erreur, trim(FicLoiConc%Nom), iPts)
      return
    Endif

  Enddo

!=========================================================================
! TRAITEMENT DE FIN DE SOUS-PROGRAMME
!=========================================================================

  close(UniteLoi)

!  Erreur%arbredappel = arbredappel_old

!=========================================================================
!  FIN DU SOUS-PROGRAMME
!=========================================================================
End Subroutine LecFicLoiConc
