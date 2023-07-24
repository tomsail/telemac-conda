Subroutine  LecFicConcIni( &

    FicCMESIni     , & ! Fichier des concentrations de sable et vase presentes initialement dans le bief
    OptionCourlis  , & ! commutateur : VRAI si calcul COURLIS de sediments
    AbscIni        , & ! Abscisse curviligne ou sont donnees les valeurs de concentration initiale
    Traceurs       , & ! Concentr. initiale des traceurs
    Erreur         )   ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture du fichier contenant les concentrations initiales
!  --------    des MES (format OPTHYCA)
!
!  Sous-programme appelant : LecConcIni
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================
!   Commentaires :
!   ------------
!   !!!!!!!  CE SOUS-PROGRAMME POURRA ETRE UTILISE ULTERIEUREMENT POUR
!   !!!!!!!                   LA LECTURE DES TRACEURS.
!   !!!!!!!
!   !!!!!!!             IL RESTE CEPENDANT A PROGRAMMER
!   !!!!!!!            LES PARTIES SPECIFIQUES AUX TRACEURS.
!
!=========================================================================


!=========================================================================
!   DECLARATIONS
!=========================================================================

use M_PRECISION            ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info
use M_LIRE_CHAINE_S        ! Lecture de lignes de commentaire du fichier
use M_OPT2FORT_I           ! Interface de sous-programme
use M_FICHIER_T            ! Definition du type FICHIER_T

use M_ERREUR_T             ! Definition du type ERREUR_T
use M_MESSAGE_C            ! Messages d'erreur
use M_TRAITER_ERREUR_I     ! Traitement de l'errreur

!.. Implicit Declarations ..
  implicit none

! Constantes
  integer     , parameter :: LEN_CHAINE         = 80
  character(1), parameter :: CHAINE_COMMENTAIRE = "#" ! Caractere commentaire qui debute une ligne commentaire

! Variables d'entree
  type(FICHIER_T)   , intent(in) :: FicCMESIni
  logical           , intent(in) :: OptionCourlis

! Variables de sortie
  real(DOUBLE), dimension(:  ), pointer :: AbscIni
  real(DOUBLE), dimension(:,:), pointer :: Traceurs

! Variables locales
!  integer  :: ModeMESIni  ! Mode d'entree de la concentration initiale de vase  ! PU2017 : Mise en commentaire
  integer  :: NbPtsConcIni ! Nombre de points devrivant les concentrations initiales
  integer  :: NbTraceurs   ! Nombre de traceurs
  integer  :: iPts,i       ! Compteur de points
  integer  :: UniteConc    ! Unite du fichier de sedimentation a lire
  integer  :: rang         ! position du mot cle sur la ligne
  integer  :: ivar         ! compteur
  integer  :: PosAbs       ! position de l'abscisse curviligne
  integer  :: PosVase      ! position de la colonne de qmin
  integer  :: PosSable     ! position de la colonne de qmin
  integer  :: NbVarStocke  ! nombre de variables stockees

  real(DOUBLE), dimension(:), allocatable :: var

  character(LEN_CHAINE) :: chaine      ! Chaine contenant une ligne du fichier
  character(LEN_CHAINE) :: chaine_opt  ! chaine au format opthyca
  character(LEN_CHAINE) :: chaine_fort ! chaine convertie au format fortran


! Traitement des erreurs
  integer                       :: retour ! code de retour des fonctions d'e/s
!  character(132) :: arbredappel_old       ! ancien arbre  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================
! COMMENTAIRES
!=========================================================================

! Description du fichier lu :
!----------------------------
! Toute ligne de commentaire commence par le caractere "#"
!
! Exemple :
!
!  #ESSAI
!  #03/03/2003
!  [variables]
!  "Abscisse curviligne"   ;"X"     ;"m"  ;1
!  "Concentration en vase" ;"CVASE" ;"g/l";3
!  "Concentration en sable";"CSABLE";"g/l";3
!  [resultats]
!  0;     0.5;    0.1
!  10;    0.5;    0.1
!  20;    0.5;    0.1
!  30;    0.5;    0.1
!  40;    0.5;    0.1
!  50;    0.5;    0.1
!

!=========================================================================
!  INITIALISATION
!=========================================================================

  Erreur%Numero      = 0
!  arbredappel_old   = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LecFicConcIni'

  UniteConc = FicCMESIni%Unite

!----------------------------
! ouverture du fichier a lire
!----------------------------
  open(unit=UniteConc   , file=FicCMESIni%Nom, access='SEQUENTIAL', &
       action='READ'    , form='FORMATTED'   , iostat=RETOUR      , &
       position='rewind', status='OLD'    )


  If (retour /= 0) Then
    Erreur%Numero = 3
    Erreur%ft   = err_3
    Erreur%ft_c = err_3c
    call TRAITER_ERREUR (Erreur, FicCMESIni%Nom)
    return
  Endif


!=========================================================================
!  LECTURE DE L'EN-TETE
!=========================================================================

! Lecture de la premiere ligne (commentaire)
!-----------------------------
  call LIRE_CHAINE_S (chaine, FicCMESIni, CHAINE_COMMENTAIRE, retour)

! si probleme en lecture de la ligne
!-----------------------------------
  If (retour /= 0) Then
    Erreur%Numero = 64
    Erreur%ft   = err_64
    Erreur%ft_c = err_64c
    call TRAITER_ERREUR (Erreur,'Concentrations Initiales')
    return
  Endif

  rang = 0
  rang = index(chaine, '[variables]')

!--------------------------------------------
! si le mot-cle '[variables]' n'est pas trouve
!--------------------------------------------
  If (rang == 0) Then
    Erreur%Numero = 65
    Erreur%ft   = err_65
    Erreur%ft_c = err_65c
    call TRAITER_ERREUR (Erreur,'Concentrations Initiales','[variables]')
    return
  Endif

!---------------------------
! initialisation des indices
!---------------------------
  ivar     = 0
  PosAbs   = 0
  PosVase  = 0
  PosSable = 0

!-----------------------------------------
! lecture de la premiere ligne de variable
!-----------------------------------------
  call LIRE_CHAINE_S (chaine, FicCMESIni, CHAINE_COMMENTAIRE, retour)

!----------------------------------------------
! Boucle de lecture sur les lignes de variables
!----------------------------------------------
  Do while (retour == 0)

    ! Si mot cle "resultat" present ==> sortir de la boucle
    ! -----------------------------------------------------
    rang = 0
    rang = index(chaine, '[resultats]')
    If (rang /= 0) exit

    ! on incremente la position de la variable lue
    ivar = ivar + 1

    ! on recherche la position des colonnes AbscIni et Traceurs
    !------------------------------------------------------
    If (index(chaine, 'X') /= 0) Then
      PosAbs = ivar
    Else If(OptionCourlis .and. index(chaine, 'CVASE') /= 0) Then
      PosVase = ivar
    Else If(OptionCourlis .and. index(chaine, 'CSABLE') /= 0) Then
      PosSable = ivar
    Else If (index(chaine, 'CTRAC') /= 0) then
    !!!!!!!!! PARTIE SPECIFIQUE AUX TRACEURS. A PROGRAMMER.
      Erreur%Numero = 66
      Erreur%ft   = err_66
      Erreur%ft_c = err_66c
      call TRAITER_ERREUR (Erreur,'Concentrations Initiales', ivar+1)
      return
    Endif

    call LIRE_CHAINE_S (chaine, FicCMESIni, CHAINE_COMMENTAIRE, retour)

  Enddo

!=========================================================================
! TRAITEMENT DES ERREURS
!=========================================================================

! Message d'erreur si probleme de lecture
!-----------------------
  If (retour /= 0) Then
    Erreur%Numero = 66
    Erreur%ft   = err_66
    Erreur%ft_c = err_66c
    call TRAITER_ERREUR (Erreur,'Concentrations Initiales', ivar + 1)
    return
  End if

! Message d'erreur si il n'y a pas de variables dans l'en-tete
!-------------------------------------------
  If (ivar == 0) Then
    Erreur%Numero = 67
    Erreur%ft   = err_67
    Erreur%ft_c = err_67c
    call TRAITER_ERREUR (Erreur,'Concentrations Initiales')
    return
  End if

! Message d'erreur si il n'y a pas de X
!----------------------------
  If (PosAbs == 0) Then
    Erreur%Numero = 412
    Erreur%ft   = err_412
    Erreur%ft_c = err_412c
    call TRAITER_ERREUR (Erreur,' des Concentrations Initiales', 'X')
    return
  End if

! Message d'erreur si il n'y a pas de CVASE
!----------------------------
  If (OptionCourlis .and. PosVase == 0) Then
    Erreur%Numero = 412
    Erreur%ft   = err_412
    Erreur%ft_c = err_412c
    call TRAITER_ERREUR (Erreur,'des Concentrations Initiales', 'CVASE')
    return
  End if

! Message d'erreur si il n'y a pas de CSABLE
!-----------------------------
  If (OptionCourlis .and. PosSable == 0) Then
    Erreur%Numero = 412
    Erreur%ft   = err_412
    Erreur%ft_c = err_412c
    call TRAITER_ERREUR (Erreur,'des Concentrations Initiales', 'CSABLE')
    return
  End if

! mise en memoire du nombre total de variables stockees en plus
  NbVarStocke = ivar

  !!!!!!!!!!!!
  !!!!!!!!!!!! ERREUR SPECIFIQUE AUX TRACEURS A PROGRAMMER ICI.
  !!!!!!!!!!!!


! Allocation du tableau 'var' en fonction de l'indice le plus grand
!-----------------------------
  allocate (var(NbVarStocke), STAT = retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'var')
    return
  End if

!==========================================================================
! PREMIERE LECTURE DES VALEURS POUR COMPTER LE NOMBRE DE SECTIONS DE CALCUL
!==========================================================================

  iPts = 1

!--------------------
! lecture d'une ligne
!--------------------
  call LIRE_CHAINE_S (chaine_opt, FicCMESIni, CHAINE_COMMENTAIRE, retour)

  ! Message d'erreur si probleme de lecture
  If (retour /= 0) Then
    Erreur%Numero = 69
    Erreur%ft   = err_69
    Erreur%ft_c = err_69c
    call TRAITER_ERREUR (Erreur, 'Concentrations Initiales', iPts)
    return
  End if

!--------------------------------------------------------
! conversion de la ligne (suppression des points-virgule)
!--------------------------------------------------------
  call OPT2FORT (chaine_fort, chaine_opt, Erreur)

  If (Erreur%Numero /= 0)  return

!--------------------------------------
! lecture des informations sur la ligne
!--------------------------------------
  read(chaine_fort, * , iostat = retour) (var(i),i=1,NbVarStocke)

  ! Message d'erreur si probleme de lecture
  If (retour /= 0) Then
    Erreur%Numero = 69
    Erreur%ft   = err_69
    Erreur%ft_c = err_69c
    call TRAITER_ERREUR (Erreur, 'Concentrations Initiales', iPts)
    return
  End if



! Boucle de lecture des valeurs
!==============================

  Do while (retour == 0)

    iPts = iPts + 1

    !-----------------------------
    ! lecture de la ligne suivante
    !-----------------------------
    call LIRE_CHAINE_S (chaine_opt, FicCMESIni, CHAINE_COMMENTAIRE, retour)
!        read(UniteConc,'(A)',iostat=RETOUR) chaine_opt

    If (retour < 0) Then
    ! si fin du fichier ==> Sortir de la boucle
      exit
    Else If(retour > 0) Then
    ! si probleme de lecture ==> Erreur
      Erreur%Numero = 69
      Erreur%ft   = err_69
      Erreur%ft_c = err_69c
      call TRAITER_ERREUR (Erreur, 'Concentrations Initiales', iPts)
      return
    Endif

    !--------------------------------------------------------
    ! conversion de la ligne (suppression des points-virgule)
    !--------------------------------------------------------
    call OPT2FORT (chaine_fort, chaine_opt, Erreur)

    If (Erreur%Numero /= 0)  return

    !--------------------------------------
    ! lecture des informations sur la ligne
    !--------------------------------------
    read(chaine_fort, * , iostat = retour) (var(i),i=1,NbVarStocke)

    ! si probleme de lecture ==> Erreur
    !----------------------------------
    If (retour /= 0) Then
      Erreur%Numero = 69
      Erreur%ft   = err_69
      Erreur%ft_c = err_69c
      call TRAITER_ERREUR (Erreur, 'Concentrations Initiales', iPts)
      return
    Endif

  Enddo   ! Fin Boucle sur les valeurs

  !-----------------------------------------------------
  ! allocation de memoire des tableaux AbscIni, Traceurs
  !-----------------------------------------------------
  NbPtsConcIni = iPts - 1
  NbTraceurs   = NbVarStocke - 1

  allocate (AbscIni(NbPtsConcIni), STAT = retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'AbscIni')
    return
  End if

  allocate (Traceurs(NbTraceurs,NbPtsConcIni), STAT = retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'Traceurs')
    return
  End if

!=====================================================================
! DEUXIEME PASSAGE : LECTURE PROPREMENT DITE DES VALEURS DES VARIABLES
!=====================================================================
! Repositionnement au debut des resultats (apres l'en-tete)
! qui prend NbVarStocke + 2 lignes
!----------------------------------------------------------
  rewind(UniteConc)

  Do ivar = 1, NbVarStocke + 2
    call LIRE_CHAINE_S (chaine, FicCMESIni, CHAINE_COMMENTAIRE, retour)
  End do

! Boucle de lecture des donnees
!==============================
  Do iPts = 1, NbPtsConcIni

    !--------------------
    ! lecture d'une ligne
    !--------------------
    call LIRE_CHAINE_S (chaine_opt, FicCMESIni, CHAINE_COMMENTAIRE, retour)
!    read(UniteConc,'(A)',iostat=retour) chaine_opt

    ! si probleme de lecture ==> Erreur
    If (retour /= 0) Then
      Erreur%Numero = 69
      Erreur%ft   = err_69
      Erreur%ft_c = err_69c
      call TRAITER_ERREUR (Erreur, 'Concentrations Initiales', iPts)
      return
    End if

    !-----------------------
    ! conversion de la ligne
    !-----------------------
    call OPT2FORT (chaine_fort, chaine_opt, Erreur)

    If (Erreur%Numero /= 0)  return

    !--------------------------------------
    ! lecture des informations sur la ligne
    !--------------------------------------
    read(chaine_fort, * , iostat = retour) (var(i),i=1,NbVarStocke)

    ! si probleme de lecture ==> Erreur
    If (retour /= 0) Then
      Erreur%Numero = 69
      Erreur%ft   = err_69
      Erreur%ft_c = err_69c
      call TRAITER_ERREUR (Erreur, 'Concentrations Initiales', iPts)
      return
    End if


    !--------------------------------------
    ! affectation du tableau Traceurs
    !--------------------------------------
    AbscIni(iPts) = var(PosAbs)

    If (OptionCourlis) Then
      Traceurs(1,iPts) = var(PosVase)
      Traceurs(2,iPts) = var(PosSable)
    Else
      !!!!!!!!
      !!!!!!!! PARTIE SPECIFIQUE AUX TRACEURS. A PROGRAMMER.
      !!!!!!!!
      Traceurs(:,iPts) = 0.
    Endif

  Enddo  ! Fin de la boucle sur les valeurs

!=========================================================================
! TRAITEMENT DE FIN DE SOUS-PROGRAMME
!=========================================================================

! de-allocation du tableau local var
!-----------------------------------
  deallocate(var)

  If (retour /= 0) Then
    Erreur%Numero = 6
    Erreur%ft   = err_6
    Erreur%ft_c = err_6c
    call TRAITER_ERREUR (Erreur, 'var')
    return
  Endif

  ! fermeture du fichier
  !---------------------
  close(UniteConc)

!  Erreur%arbredappel = arbredappel_old

  return

!=========================================================================
!  FIN DU SOUS-PROGRAMME
!=========================================================================
End Subroutine LecFicConcIni
