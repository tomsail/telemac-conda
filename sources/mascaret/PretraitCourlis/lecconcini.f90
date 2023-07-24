Subroutine  LecConcIni  ( &

  UniteListing       , & ! Unite du fichier d'impression des parametres
  ImpressionConcIni  , & ! Choix d'impression des concentrations initiales
  FicCMESIni         , & ! Fichier des concentrations de sable et vase
! presentes initialement dans la retenue
  Abscisse           , & ! Abscisse curviligne des sections de calcul
  NbProfCourlis      , & ! Nombre de sections de calcul
  OptionCourlis      , & ! commutateur : VRAI si calcul COURLIS de sediments
  Traceurs0          , & ! Concentr. initiale des traceurs
! Lecture des mots du dictionnaires
  MOTINT             , &
  MOTREA             , &
  MOTCAR             , &
  ADRESS             , &
! Traitement des erreurs
  Erreur             )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture des concentrations des matieres en suspension
!  --------    initialement presentes dans le bief
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele : LecFicConcIni
!  ---------------------
!
!=========================================================================

use M_PRECISION            ! Definition de la precision DOUBLE ou SIMPLE
use M_FICHIER_T            ! Definition du type FICHIER_T
use M_CONSTANTES_CALCUL_C  ! Constantes num, phys et info, mode de saisie

use M_ERREUR_T             ! Type ERREUR_T
use M_MESSAGE_C            ! Messages d'erreur
use M_TRAITER_ERREUR_I     ! Traitement de l'errreur

use M_INTERPOLATION_S      ! Sous-programme d'interpolation

use M_LecFicConcIni_I      ! Module d'interface

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(FICHIER_T)                   , intent(inout) :: FicCMESIni
  real(DOUBLE)      , dimension(:)  , pointer       :: Abscisse
  logical                           , intent(in   ) :: OptionCourlis
  integer                           , intent(in   ) :: NbProfCourlis

  logical                           , intent(in   ) :: ImpressionConcIni
  integer                           , intent(in   ) :: UniteListing

  integer           , dimension(:)  , intent(in   ) :: MOTINT
  real(DOUBLE)      , dimension(:)  , intent(in   ) :: MOTREA
  character(LEN=144), dimension(:)  , intent(in   ) :: MOTCAR
  integer           , dimension(:,:), intent(in   ) :: ADRESS

! Variables de sortie
  real(DOUBLE)    , dimension(:,:)  , pointer    :: Traceurs0

! Variables locales
  integer                :: ModeCMESIni   ! Mode d'entree de la concentration initiale de vase
  integer                :: NbPtsConcIni  ! Nb pts position des traceurs
  integer                :: iTraceur      ! Compteur de traceurs
  integer                :: iPts          ! Compteur de points
  integer                :: NbTraceurs

  real(DOUBLE), dimension(:)  , pointer :: AbscIni     ! Abscisse curviligne des valeurs de concentration initiale
  real(DOUBLE), dimension(:,:), pointer :: Traceurs    ! Nombre de points devrivant les concentrations initiales

! Traitement des erreurs
  integer                       :: retour      ! Code de retour Read
  type(ERREUR_T), intent(inout) :: Erreur
!  character(132)        :: arbredappel_old  ! Arbre d'appel initial  ! PU2017 : Mise en commentaire

!=========================================================================
! INITIALISATION
!=========================================================================

  Erreur%Numero      = 0
!  arbredappel_old = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LecConcIni'

  if(ImpressionConcIni) write(UniteListing,10300)

!=========================================================================
! CHOIX DU MODE DE SAISIE DES CONCENTRATIONS INITIALES
!=========================================================================

  ModeCMESIni = MOTINT(ADRESS(1,603))

!=========================================================================
! TRAITEMENT DES DONNEES EN FONCTION DU MODE DE SAISIE
!=========================================================================

  ModeSaisie: If (ModeCMESIni == SAISIE_PAR_FICHIER) Then
! SAISIE PAR FICHIER => APPEL DU SS-PROG LecFichierSedim
! ------------------------------------------------------

    FicCMESIni%Nom = MOTCAR(ADRESS(4,603))

    if (ImpressionConcIni) write(UniteListing,10310) trim(FicCMESIni%Nom)

    call LecFicConcIni ( &
      FicCMESIni       , &
      OptionCourlis    , &
      AbscIni          , &
      Traceurs         , &
      Erreur           )


    if (Erreur%numero /= 0)   return

      NbTraceurs   = size(Traceurs(:,1))
      NbPtsConcIni = size(Traceurs(1,:))

    Else If (ModeCMESIni == SAISIE_PAR_CLAVIER) Then
    ! SAISIE AU CLAVIER => ENREGISTREMENT A PARTIR DES MOTS-CLES SAISIS DANS L'IHM
    ! ----------------------------------------------------------------------------

      if(ImpressionConcIni)   write(UniteListing,10320)

      ! Nombre de traceurs
      !-------------------
      If (OptionCourlis) Then
        NbTraceurs = 2
      Else
        NbTraceurs = MOTINT(ADRESS(1,605))
        If (NbTraceurs < 0) Then
          Erreur%Numero = 408
          Erreur%ft   = err_408
          Erreur%ft_c = err_408c
          call TRAITER_ERREUR (Erreur, 'Le nombre de traceurs', '> ou egal a 0')
          return
        Endif
      Endif

      ! Nombre de points de position des traceurs
      !---------------------------------------------------
      NbPtsConcIni = MOTINT(ADRESS(1,604))
      If (NbPtsConcIni < 0) Then
        Erreur%Numero = 408
        Erreur%ft   = err_408
        Erreur%ft_c = err_408c
        call TRAITER_ERREUR (Erreur, 'Le nombre de points &
                                      Concentration Init.',  &
                                      '> ou egal a 0')
        return
      Endif


      ! Allocation de memoire du tableau des abscisses curvilignes
      !-----------------------------------------------------------
      allocate (AbscIni(NbPtsConcIni), STAT = retour)
      If (retour /= 0) Then
        Erreur%Numero = 5
        Erreur%ft   = err_5
        Erreur%ft_c = err_5c
        call TRAITER_ERREUR (Erreur, 'AbscIni')
        return
      Endif


      ! Lecture des abscisses curvilignes
      !----------------------------------
      Do iPts = 1, NbPtsConcIni
        AbscIni(iPts) = MOTREA(ADRESS(2,619)+iPts-1)
      Enddo


      ! Allocation de memoire du tableau des concentrations initiales
      !--------------------------------------------------------------
      allocate (Traceurs(NbTraceurs,NbPtsConcIni), STAT = retour)
      If (retour /= 0) Then
        Erreur%Numero = 5
        Erreur%ft   = err_5
        Erreur%ft_c = err_5c
        call TRAITER_ERREUR (Erreur, 'Traceurs')
        return
      Endif

      ! Lecture des concentrations initiales (traceurs ou vase ou sable)
      !-----------------------------------------------------------------
      If (OptionCourlis) Then
        !Si calcul Courlis, alors lire les mots-cle specifiques a Courlis

        Do iPts = 1, NbPtsConcIni
          ! Lecture de la concentration initiale en vase
          Traceurs(1,iPts) = MOTREA(ADRESS(2,620)+iPts-1)
          ! Lecture de la concentration initiale en sable
          Traceurs(2,iPts) = MOTREA(ADRESS(2,621)+iPts-1)
        Enddo

      Else
        !Ecrire la partie pour traceur si necessaire, sinon reecrire pour courlis uniquement
        !Sinon, lire de maniere generale tous les traceurs
!chb
!        do iTraceur = 1, NbTraceurs
!          do iPts = 1, NbPtsConcIni
!            Traceurs(iTraceur,iPts) = MOTREA(ADRESS(2,5XX+iTraceur-1)+iPts-1)
!          end do
!        end do

      Endif

    ! TRAITEMENT DES ERREURS SUR LE MODE DE SAISIE
    ! --------------------------------------------
    Else

      Erreur%Numero = 410
      Erreur%ft   = err_410
      Erreur%ft_c = err_410c
      call TRAITER_ERREUR (Erreur, 'les concentrations initiales')
      return

    ! FIN CHOIX MODE DE SAISIE
    ! ----------------------
    Endif ModeSaisie

!=========================================================================
! INTERPOLATION DE LA CONCENTRATION SUR LE MAILLAGE
!=========================================================================

    ! Allocation de memoire du tableau des concentrations initiales
    !--------------------------------------------------------------
    allocate (Traceurs0(NbTraceurs,NbProfCourlis), STAT = retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR  (Erreur, 'Traceurs0')
      return
    Endif

    ! Appel du sous-programme d'interpolation
    !----------------------------------------
    Do iTraceur = 1, NbTraceurs
      Do iPts = 1, NbProfCourlis

        call INTERPOLATION_S       ( &
          Traceurs0(iTraceur,iPts) , & ! Resultats
          Abscisse(iPts)           , & ! Abscisse pour laquelle on veut YT
          1                        , & ! Ordre d'interpolation
          AbscIni(:)               , & ! Tableau des abscisses
          Traceurs (iTraceur,:)    , & ! Tableau des ordonnees
          NbPtsConcIni             , & ! Dimension des tableaux X et Y
          Erreur                   )

        if (Erreur%Numero /= 0) return

      Enddo
    Enddo

!=========================================================================
! IMPRESSION DES RESULTATS DE LECTURE
!=========================================================================

    If (ImpressionConcIni) Then

      write(UniteListing,10330) NbProfCourlis

      If(OptionCourlis) Then
        write(UniteListing,10340) 'Absc','CVase','CSable'

        Do iPts = 1, NbProfCourlis
          write(UniteListing,10360) Abscisse(iPts)            , &
                                    (Traceurs0(iTraceur,iPts) , &
                                    iTraceur = 1              , &
                                    NbTraceurs)

        Enddo
      Endif

  Endif

!=========================================================================
!  DEALLOCATION DE MEMOIRE POUR LES TABLEAUX LOCAUX
!=========================================================================

  deallocate(AbscIni,Traceurs)

!  Erreur%arbredappel = arbredappel_old


  return

!=========================================================================
!  FORMATS D'ECRITURE
!=========================================================================

  10300 format (/,'CONCENTRATIONS INITIALES',/,     &
                &   '------------------------',/)
  10310 format ('Mode de saisie par fichier. Nom du fichier    : ',A )
  10320 format ('Mode de saisie par clavier                      '   )
  10330 format ('Nombre de profils d''interface : ',I3)
  10340 format (3A12)
  10360 format (16f12.3)

!=========================================================================
! FIN SOUS-PROGRAMME
!=========================================================================
End Subroutine LecConcIni
