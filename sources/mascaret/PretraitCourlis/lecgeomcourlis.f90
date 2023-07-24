Subroutine  LecGeomCourlis ( &

  UniteListing    , & ! Unite logique fichier listing
  FicGeomCourlis  , & ! Fichier des profils definissant les interfaces
  ImpressionGeom  , & ! Choix d'impression de la geometrie des interfaces sed.
  NbInterfaces    , & ! Nb d'interfaces
  NbProf          , & ! Nombre de profils du tableau "profil"
  Profil          , & ! profils de Mascaret
  ProfilCourlis   , & ! Profils de la geometrie des rivieres, lus dans COURLIS
  MOTCAR          , &
  ADRESS          , &
  Erreur          )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003  Copyright EDF-CETMEF
!
!*************************************************************************
!  Modifiee le 21/10/2003 par Ch. Bertier
!  Suppression du doublement des points extremes des profils
!
!=========================================================================
!  Fonction : Lecture du fichier de geometrie comprenant la description
!  --------    des couches sedimentaires sous forme de profils en travers
!             Verification de la coherence des donnees
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele : - TestGeom
!  ---------------------
!=========================================================================
!  Commentaires : Description du fichier lu
!  ------------
!  Toute ligne de commentaire est debutee par le caractere "#"
!  Ligne 1 : mot-cle MOT_CLE, NomBief, NomProfil, abscisse profil.
!            sans blanc dans les noms : le blanc est le caractere separateur
!            Ex : <MOT_CLE bief1 profil1 340.3>
!
!  Ligne 2 et jusqu'au prochain mot-cle MOT ou la fin de fichier :
!    (T, Z1, Z2, Z3, ...)
!    Exemple :
!      <  0. 2000.  2005. 2010. >
!      < 10. 3000.  3005. 3010. >
!      <100. 4000.  4005. 4010. >
!      etc.
!
!  ATTENTION : Contrairement a l'hydraulique, le premier et le dernier point
!              des profils en travers ne sont pas doubles. Les profils Courlis
!              ont donc 2 points de moins que les profils hydrauliques.
!
!=========================================================================


!=========================== Declarations ================================

use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
use M_FICHIER_T
use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS
use M_PROFIL_T            ! Definition du type PROFIL
use M_LIRE_CHAINE_S       ! lecture de lignes de commentaire
use M_DECODER_GEOM_I      ! Interface de sous-programme
use M_PARAMETRE_C         ! Parametres
use M_TestGeom_I          ! Interface de sous-programme

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'erreur

!.. Implicit Declarations ..
  implicit none


! Variables d'entree
  type(FICHIER_T)     , intent(inout) :: FicGeomCourlis

  integer       , intent(in   ) :: UniteListing
  logical       , intent(in   ) :: ImpressionGeom
  integer       , intent(in   ) :: NbInterfaces
  integer       , intent(in   ) :: NbProf

  type(PROFIL_T)    , dimension(:)  , pointer       :: Profil
  character(LEN=144), dimension(:)  , intent(in   ) :: MOTCAR
  integer           , dimension(:,:), intent(in   ) :: ADRESS


  ! Variables de sortie
  type(PROFIL_COURLIS_T), dimension(:), pointer   :: ProfilCourlis


  ! Constantes
  integer, parameter :: LEN_CHAINE = 80


  ! Constantes sur les mots-cles
  integer, parameter :: NB_MOT_CLE = 3
  character(*), dimension(NB_MOT_CLE), parameter :: MOT_CLE = (/"PROFIL", &
                                                                "Profil", &
                                                                "profil"  /)
  character(1), parameter :: CHAINE_COMMENTAIRE = "#" ! Caractere commentaire qui
                                                      ! debute une ligne commentaire

  logical  :: PresenceMotCle ! Test de presence du mot cle
  integer  :: UniteGeom      ! Unite logique du fichier de geometrie

  integer  :: iProf          ! Compteur sur les profils
  integer  :: NbProfCourlis  ! Nb de profils lus dans le fichier de geom.

  integer       :: iPts      ! Compteur sur les points d'un profil
  integer       :: iInterf   ! Compteur d'interface
  integer       :: NbPoints  ! Nombre de points d'un profil
  character(30) :: NomProf   ! Nom du profil
  character(30) :: NomBief   ! Nom du bief
  real(DOUBLE)  :: Abs       ! Abscisse relative du profil
!  real(DOUBLE)  :: CoteMax   ! Cote extreme maximum d'un profil  ! PU2017 : Mise en commentaire
  real(DOUBLE)  :: z_ref     ! Point bas du lit

  character(LEN_CHAINE) :: chaine ! Chaine contenant une ligne du fichier

! Traitement des erreurs
  integer        :: retour      ! Code de retour de la fonction read, allocate
  integer        :: Retour_L    ! Code de retour a la 2ieme lecture du fichier
!  character(132) :: arbredappel_old ! Arbre d'appel initial  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================

!  write(UniteListing,10000)
  Retour_L = 0
!=========================================================================
! LECTURE DU NOM DU FICHIER GEOMETRIE
!=========================================================================

  FicGeomCourlis%Nom = MOTCAR(ADRESS(4,602))

!  If (ImpressionGeom)  write(UniteListing,10010) FicGeomCourlis%Nom

!=========================================================================
! PREMIERE LECTURE DU FICHIER :
!
!  => DEFINITION DU NOMBRE DE PROFILS
!  => DETECTION DES ERREURS
!=========================================================================

  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LecGeomCourlis'

! Ouverture du fichier a lire
! ---------------------------
  UniteGeom = FicGeomCourlis%Unite

  open(unit=UniteGeom, file=FicGeomCourlis%Nom, access='SEQUENTIAL', &
       action='READ'    , form='FORMATTED', iostat=retour,    &
       position='rewind', status='OLD'    )

  If (retour /= 0) Then
    Erreur%Numero = 3
    Erreur%ft   = err_3
    Erreur%ft_c = err_3c
    call TRAITER_ERREUR (Erreur, trim(FicGeomCourlis%Nom))
    return
  End if

! Ligne 1 : mot-cle "MOT_CLE"
!----------------------------

  call LIRE_CHAINE_S (chaine, FicGeomCourlis, CHAINE_COMMENTAIRE, retour)

  If (retour /= 0) Then
    Erreur%Numero = 77
    Erreur%ft   = err_77
    Erreur%ft_c = err_77c
    call TRAITER_ERREUR ( &
      Erreur                   , &
      'de sediments (COURLIS)' , &
      trim(FicGeomCourlis%Nom) )
    return
  End if

  iProf = 0

! LIRE LES PARAMETRES D'UN PROFIL GEOMETRIQUE (SI PAS D'ERREUR DE LECTURE)
! ------------------------------------------------------------------------
  label_NbProf: Do while (retour == 0)

    call DECODER_GEOM ( &
      PresenceMotCle , &
      NomBief        , &
      NomProf        , &
      Abs            , &
      chaine         , &
      MOT_CLE        , &
      iProf+1        , &
      Erreur         )

    If (Erreur%Numero /=0)  return

    If (PresenceMotCle) Then
    ! SI MOT-CLE PRESENT :
    !  ==> INCREMENTATION DU NOMBRE DE PROFILS
    ! -----------------------------------------------------------
      iProf = iProf + 1

    Else
    ! SINON :
    !  ==> MESSAGE D'ERREUR
    ! -----------------------------------------------------------

    ! ERREUR si le mot-cle MOT_CLE n'est pas present sur la ligne
      If (iProf == 0) Then
        Erreur%Numero = 78
        Erreur%ft   = err_78
        Erreur%ft_c = err_78c
    !chb  call TRAITER_ERREUR (Erreur,'de sediments (COURLIS)',trim(FicGeomCourlis%Nom))
        call TRAITER_ERREUR (Erreur,trim(FicGeomCourlis%Nom))
        return
      Endif

    End if

    ! LECTURE DE LA LIGNE SUIVANTE
    !-----------------------------

    call LIRE_CHAINE_S (chaine, FicGeomCourlis, CHAINE_COMMENTAIRE, retour)

  End do label_NbProf


! SI LIRE_CHAINE_S ==> informe d'une erreur de lecture
!-------------------------------------------------
  If (retour > 0) Then
    Erreur%Numero = 79
    Erreur%ft   = err_79
    Erreur%ft_c = err_79c
    call TRAITER_ERREUR (Erreur,trim(FicGeomCourlis%Nom),iProf)
    return
  Endif

!=========================================================================
! ALLOCATION DE MEMOIRE DU TABLEAU ProfilCourlis
!=========================================================================

  NbProfCourlis = iProf

  allocate(ProfilCourlis(NbProfCourlis),STAT=retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'ProfilCourlis')
    return
  End if

!  If(ImpressionGeom) write(UniteListing,10020) NbProfCourlis

!=========================================================================
! 2-IEME LECTURE DU FICHIER DE GEOMETRIE :
!
!   => DEFINIR LA TAILLE DES PROFILS
!=========================================================================

! Retour au debut du fichier
!---------------------------
  rewind(UniteGeom)

! Initialisation
  iProf = 0
  iPts  = 0


! LIRE LES PARAMETRES D'UN PROFIL GEOMETRIQUE (SI PAS D'ERREUR DE LECTURE)
! ------------------------------------------------------------------------
  label_dim_prof: Do while (retour_L == 0)


! Lecture d'une ligne du fichier (autre que lignes de commentaires)
!------------------------------------------------------------------
    call LIRE_CHAINE_S (chaine, FicGeomCourlis, CHAINE_COMMENTAIRE, retour_L)


    ! Rechercher si le mot-cle est dans present dans la ligne
    !--------------------------------------------------------
    call DECODER_GEOM ( &
      PresenceMotCle , &
      NomBief        , &
      NomProf        , &
      Abs            , &
      chaine         , &
      MOT_CLE        , &
      iProf+1        , &
      Erreur         )

    If (Erreur%Numero /=0) return


    ! SI MOT-CLE PRESENT OU SI FIN DE FICHIER :
    !  ==> STOCKAGE DU NOMBRE DE POINTS DU PROFIL PRECEDENT
    !  ==> ALLOCATION DE MEMOIRE DU PROFIL PRECEDENT
    !
    ! -----------------------------------------------------------
    If (PresenceMotCle .or. Retour_L /= 0) Then

      ! Si pas 1ere ligne du fichier, allouer memoire Absc. angul.
      !-----------------------------------------------------------
      If (iPts > 0) Then
      !chb-21.10.03    allocate (ProfilCourlis(iProf)%X(iPts + 2), STAT=retour)
        allocate (ProfilCourlis(iProf)%X(iPts), STAT=retour)
        If (retour /= 0) Then
          Erreur%Numero = 5
          Erreur%ft   = err_5
          Erreur%ft_c = err_5c
          call TRAITER_ERREUR (Erreur, 'ProfilCourlis/Abscisse')
          return
        End if

        ! Allocation de memoire du tableau des cotes des interfaces
        !chb-21.10.03    allocate (ProfilCourlis(iProf)%Z(NbInterfaces,iPts + 2), STAT=retour)
        allocate (ProfilCourlis(iProf)%Z(NbInterfaces,iPts), STAT=retour)
        If (retour /= 0) Then
          Erreur%Numero = 5
          Erreur%ft   = err_5
          Erreur%ft_c = err_5c
          call TRAITER_ERREUR (Erreur, 'ProfilCourlis/Ordonnee')
          return
        End if

        ! Sauvegarde du nombre de points du profil
        !chb-21.10.03          ProfilCourlis(iProf)%NbPoint = iPts+2
        ProfilCourlis(iProf)%NbPoint = iPts

        ! on remet le compteur des abscisses a zero
        iPts = 0

      End if

      ! Si pas a la fin du fichier ==> passer a 1 nouveau profil
      !---------------------------------------------------------
      If (retour_L == 0) Then
        ! Incrementation de nombre de profils
        iProf = iProf + 1
        ! on stocke le nom de la branche, le nom du profil
        ! et l'abscisse utilisateur du profil
        ProfilCourlis(iProf)%Nom = NomProf
        ProfilCourlis(iProf)%Abs = Abs
      End if

    Else

      ! Si MOT-CLE ABSENT :
      !   ==> INCREMENTATION DE L'INDICE DE LECTURE DES ABSCISSES
      !------------------------------------------------------------------
      iPts = iPts + 1

    End if

  End do label_dim_prof


! La lecture du fichier est terminee
!====================================

  label_retour: If (retour > 0) Then
    ! SI ERREUR DE LECTURE (DANS BOUCLE PRECEDENTE) ==> MESSAGE D'ERREUR
    !-------------------------------------------------------------------
    Erreur%Numero = 413
    Erreur%ft   = err_413
    Erreur%ft_c = err_413c
    call TRAITER_ERREUR (Erreur, trim(FicGeomCourlis%Nom),iProf, iPts-1)
    return
  End if label_retour

!=========================================================================
! 3-IEME LECTURE DU FICHIER DE GEOMETRIE :
!
!   => LECTURE EFFECTIVE DES PROFILS
!=========================================================================

! Retour au debut du fichier
  rewind (UniteGeom)

! Ligne 1 : mot-cle "MOT_CLE"
!----------------------------
  call LIRE_CHAINE_S (chaine, FicGeomCourlis, CHAINE_COMMENTAIRE, retour)

  If (retour /= 0) Then
    Erreur%Numero = 77
    Erreur%ft   = err_77
    Erreur%ft_c = err_77c
    call TRAITER_ERREUR (Erreur,'de sediments (COURLIS)',trim(FicGeomCourlis%Nom))
    return
  End if

! BOUCLE SUR LES PROFILS D'INTERFACE : LECTURE DES COORDONNEES
!-------------------------------------------------------------
  boucle_profils: Do iProf = 1, NbProfCourlis

    NbPoints = ProfilCourlis(iProf)%NbPoint

!    If (ImpressionGeom) Then
!      write(UniteListing,10030) iProf
!      write(UniteListing,10040) ProfilCourlis(iProf)%Nom, ProfilCourlis(iProf)%Abs
!      write(UniteListing,10050) NbPoints
!    End if

    ! Si ce n'est pas le premier profil, lire la ligne d'en-tete
    !----------------------------------------------------------
    test_profil1: If (iProf /= 1) Then

      ! Lecture d'un mot-cle et d'un numero
      call LIRE_CHAINE_S (chaine, FicGeomCourlis, CHAINE_COMMENTAIRE, retour)

      If (retour /= 0) Then
        Erreur%Numero = 77
        Erreur%ft   = err_77
        Erreur%ft_c = err_77c
        call TRAITER_ERREUR (Erreur,'de sediments (COURLIS)',trim(FicGeomCourlis%Nom))
        return
      End if

    End if test_profil1

    ! If (ImpressionGeom) write(UniteListing,10060)

    ! Lecture des points du profil iProf
    !-----------------------------------
    !chb-21.10.03  boucle_points: do iPts = 2, NbPoints - 1
    boucle_points: Do iPts = 1, NbPoints

      ! Lecture d'une ligne de donnees
      call LIRE_CHAINE_S (chaine, FicGeomCourlis, CHAINE_COMMENTAIRE, retour)

      read(chaine,*, iostat=retour) ProfilCourlis(iProf)%X(iPts), &
          (ProfilCourlis(iProf)%Z(iInterf,iPts), iInterf=1,NbInterfaces)


      If (retour /= 0) Then
        Erreur%Numero = 413
        Erreur%ft   = err_413
        Erreur%ft_c = err_413c
        call TRAITER_ERREUR (Erreur,trim(FicGeomCourlis%Nom),iProf,iPts-1)
        return
      End if

    End do boucle_points


!chb-21.10.03  ! Definition des points extremes
!chb-21.10.03  !-------------------------------
!chb-21.10.03  ProfilCourlis(iProf)%X(1    )=ProfilCourlis(iProf)%X(2       )
!chb-21.10.03  ProfilCourlis(iProf)%X(NbPoints)=ProfilCourlis(iProf)%X(NbPoints-1)
!chb-21.10.03
!chb-21.10.03  do iInterf = 1, NbInterfaces
!chb-21.10.03    CoteMax = MAX( ProfilCourlis(iProf)%Z(iInterf,2   ), &
!chb-21.10.03    ProfilCourlis(iProf)%Z(iInterf,NbPoints-1 ))
!chb-21.10.03    ProfilCourlis(iProf)%Z(iInterf,1  ) = CoteMax + 10.
!chb-21.10.03    ProfilCourlis(iProf)%Z(iInterf,NbPoints) = CoteMax + 10.
!chb-21.10.03  end do

! Impression des points
! If (ImpressionGeom) Then
!   Do iPts = 1, NbPoints
!     write(UniteListing,10070) ProfilCourlis(iProf)%X(iPts), &
!     (ProfilCourlis(iProf)%Z(iInterf,iPts), iInterf=1,NbInterfaces)
!   End do
! End if


  End do boucle_profils


! ===============================================================
! CALCUL DES POINTS BAS DES INTERFACES POUR CHAQUE PROFIL
! ===============================================================

!  If (ImpressionGeom) Then
!    write(UniteListing,10100)
!    write(UniteListing,10080)
!  End if


  Do iProf = 1, NbProfCourlis

    ! Allocation de memoire du tableau des points bas des interfaces
    allocate (ProfilCourlis(iProf)%Zref(NbInterfaces), STAT=retour)

    !MS2018 memory allocation for sediment fraction
    allocate(ProfilCourlis(iprof)%Frac(2), STAT=retour)

    !MS2018 Initial conditions (dev in progress)
    ProfilCourlis(iprof)%Frac(1)=0.5
    ProfilCourlis(iprof)%Frac(2)=0.5

    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'ProfilCourlis/Zref')
      return
    End if

    z_ref = INFINI

    Do iInterf = 1, NbInterfaces
      Do iPts = 1, size(ProfilCourlis(iProf)%Z(iInterf,:))
        z_ref = min(z_ref,ProfilCourlis(iProf)%Z(iInterf,iPts))
      End do

      ProfilCourlis(iProf)%Zref(iInterf) = z_ref

    End do

!    If (ImpressionGeom) Then
!      write(UniteListing,10090) iProf, &
!      (ProfilCourlis(iProf)%Zref(iInterf), iInterf=1,NbInterfaces)
!    End if

  End do

! ===============================================================
! SOUS-PROGRAMME TestGeom de verification des valeurs
! ===============================================================

  call TestGeom ( &
    NbInterfaces   , &
    NbProf         , &
    NbProfCourlis  , &
    Profil         , &
    ProfilCourlis  , &
    Erreur         )


! ===============================================================
! TRAITEMENT DE FIN DE SOUS-PROGRAMME
! ===============================================================

  close(UniteGeom)

!  Erreur%arbredappel = arbredappel_old

  return

! ===============================================================
! FORMATS
! ===============================================================


!=========================================================================
! FIN SOUS-PROGRAMME
!=========================================================================
End Subroutine LecGeomCourlis
