Subroutine  LecFichierSedim    ( &

    FichierSedim               , & ! Fichier contenant les donnees sedim. rel. a ch. couche + donnees concernant la stabilite des berges
    NbCouche                   , & ! Nb de couches sedimentaires
    CoucheSed                  , & ! variable contenant ttes les donnees rel. a ch. couche
    Talus                      , & ! variable contenant ttes les donnees rel. aux talus
    LimiteSable                , & ! % de sable a part. dql la couche est traitee suivant les lois du sable
    CnuxV                      , & ! Coefficient de diffusion vases
    CnuxS                      , & ! Coefficient de diffusion sables
    !ConsConv                   , & ! parametres schema de convection
    Erreur                     )


!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL, M. JODEAU
!
!  VERSION : 5.1       08-2009        Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture du fichier contenant les parametres des couches
!  --------    sedimentaires constituant le lit du bief
!
!  Sous-programme appelant : LecParamSedim
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

!=========================== Declarations ================================

use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
use M_LIRE_CHAINE_S       ! lecture de lignes de commentaire du fichier
use M_FICHIER_T           ! Definition du type FICHIER_T
use M_COUCHE_T            ! Definition du type COUCHE_T
use M_TALUS_T             ! Definition du type TALUS_T
use M_CONSTANTES_TRACER_T ! parametres schema convection

use M_ERREUR_T            ! Definition du type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(FICHIER_T)        , intent(in   ) :: FichierSedim

! Variables de sortie
  integer                 , intent(  out)    :: NbCouche
  type(TALUS_T)           , intent(  out)    :: Talus
  real(DOUBLE)            , intent(  out)    :: LimiteSable
  real(DOUBLE)            , intent(  out)    :: CnuxV
  real(DOUBLE)            , intent(  out)    :: CnuxS
!  type(CONSTANTES_TRACER_T), intent(out) :: ConsConv

  type(COUCHE_T), dimension(:), pointer :: CoucheSed

! Constantes
  integer, parameter :: LEN_CHAINE = 80

! Variables locales
  integer        :: UniteSedim        ! Unite du fichier de sedimentation a lire
  character(72)  :: txt, DesignVar
  integer        :: iCouche           ! indice de couche
  integer        :: NumCouche         ! Numero de couche

  character(LEN_CHAINE) :: chaine     ! Chaine contenant une ligne du fichier

  character(1), parameter :: CHAINE_COMMENTAIRE = "#" ! Caractere commentaire qui debute une ligne commentaire

! Traitement des erreurs
  integer         :: retour(8)        ! Code de retour de la fonction read
  integer         :: retour0
!  character(132) :: arbredappel_old    ! ancien arbre  ! PU2017 : Mis en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================
!   INITIALISATION
!=========================================================================

  UniteSedim = FichierSedim%Unite
  RETOUR0    = 0
  RETOUR     = 0

! ===============================================================
! OUVERTURE DU FICHIER DE PARAMETRES
! ===============================================================

  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mis en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LecFichierSedim'

! Ouverture du fichier
! --------------------
  open(unit=UniteSedim, file=FichierSedim%Nom, access='SEQUENTIAL', &
       action='READ'    , form='FORMATTED' , iostat=retour0,        &
       position='rewind', status='OLD'     )

  If (retour0 /= 0) Then
    Erreur%Numero = 3
    Erreur%ft   = err_3
    Erreur%ft_c = err_3c
    call TRAITER_ERREUR (Erreur, trim(FichierSedim%Nom))
    return
  Endif


!=========================================================================
! COMMENTAIRES
!=========================================================================

! Description du fichier lu :
!----------------------------
! Toute ligne de commentaire est debutee par le caractere "#"
! Ligne 1 : Nombre de couches de sediments (texte + valeur)
!
! Ligne 2 : Numero de la couche de sediments dont les parametres sont a lire
! Ligne 3 a 11 : Parametres de la couche de sediment
!
! les lignes 2 a 11 sont repetees autant de fois qu'il y a de couches

!=========================================================================
!    LECTURE DES COUCHES DE SEDIMENTS
!=========================================================================

! Lecture du nombre de couches de sediments
!---------------------------------------------------------------
  call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour0)

  read(chaine,*, iostat = RETOUR0) DesignVar, NbCouche

  If (retour0 /= 0) Then
    Erreur%Numero = 411
    Erreur%ft   = err_411
    Erreur%ft_c = err_411c
    call TRAITER_ERREUR (Erreur,'NCOUCHES(nombre de couches)')
    return
  Endif

! Verification sur le nombre de couche (on impose 0 < NbCouche < 8 )
!-------------------------------------------------------------------
  If (NbCouche <= 2) Then
    Erreur%Numero = 408
    Erreur%ft   = err_408
    Erreur%ft_c = err_408c
    call TRAITER_ERREUR (Erreur, 'Le nombre de couches', '> ou egal a 3')
    return
  Else if(NbCouche > 7) Then
    Erreur%Numero = 408
    Erreur%ft   = err_408
    Erreur%ft_c = err_408c
    call TRAITER_ERREUR (Erreur, 'Le nombre de couches', 'au maximum de 7.')
    return
  Endif


! Allocation de memoire du tableau des couches de sediments
!----------------------------------------------------------
  allocate (CoucheSed(NbCouche), STAT = retour0)
  If (retour0 /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'CoucheSed')
    return
  Endif


! Boucle de lecture sur les couches de sediments
!-----------------------------------------------
  Do iCouche = 1, NbCouche

    ! Lecture du numero de couche
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour0)
    read(chaine,*, iostat = retour0) DesignVar, NumCouche

    If (retour0 /= 0) Then
      Erreur%Numero = 411
      Erreur%ft   = err_411
      Erreur%ft_c = err_411c
      call TRAITER_ERREUR (Erreur, 'Numero de couche')
      return
    Endif

    ! Verification NumCouche <= Nbcouche
    If (NumCouche > NbCouche) Then
      Erreur%Numero = 408
      Erreur%ft   = err_408
      Erreur%ft_c = err_408c
      call TRAITER_ERREUR (Erreur, 'Le numero de la couche', '<= au nb de couches.')
      return
    Endif

    ! Lecture du nom de couche
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(1))
    read(chaine,*, iostat = retour(1)) DesignVar, CoucheSed(NumCouche)%Nom

    ! Lecture de la concentration de la couche
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(2))
    read(chaine,*, iostat = retour(2)) DesignVar, CoucheSed(NumCouche)%Cfond

    ! Lecture du pourcentage de sable
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(3))
    read(chaine,*, iostat = retour(3)) DesignVar, CoucheSed(NumCouche)%Psable

    ! Lecture du diametre moyen du sable
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(4))
    read(chaine,*, iostat = retour(4)) DesignVar, CoucheSed(NumCouche)%D50

    ! Lecture de la vitesse de chute
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(5))
    read(chaine,*, iostat = retour(5)) DesignVar, CoucheSed(NumCouche)%Wc

    ! Lecture de la contrainte critique d'erosion
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(6))
    read(chaine,*, iostat = retour(6)) DesignVar, CoucheSed(NumCouche)%TauCE

    ! Lecture du coefficient des Partheniades
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(7))
    read(chaine,*, iostat = retour(7)) DesignVar, CoucheSed(NumCouche)%Mpart

    ! Lecture du Strickler de peau
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(8))
    read(chaine,*, iostat = retour(8)) DesignVar, CoucheSed(NumCouche)%Kp

    ! Lecture du Strickler de total
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(8))
    read(chaine,*, iostat = retour(8)) DesignVar, CoucheSed(NumCouche)%Kt


    If (sum(retour) /= 0) Then
      Erreur%Numero = 411
      Erreur%ft   = err_411
      Erreur%ft_c = err_411c
      write(txt,'(a,i3,a)') 'parametres de couche de sediments &
                            &(couche n0 ',NumCouche,')'
      call TRAITER_ERREUR (Erreur, trim(txt))
      return
    Endif

    ! Lecture de la vitesse de chute et de la Contrainte critique de depot des vases,
    ! pour la 1ere couche seulement
    If (NumCouche == 1) Then
      call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour0)
      read(chaine,*, iostat = retour(5)) DesignVar, CoucheSed(NumCouche)%Wc
      If (RETOUR0 /= 0) Then
        Erreur%Numero = 411
        Erreur%ft   = err_411
        Erreur%ft_c = err_411c
        call TRAITER_ERREUR (Erreur, 'Vitesse de chute des vases')
        return
      Endif

      call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour0)
      read(chaine,*, iostat = retour0) DesignVar, CoucheSed(NumCouche)%TauCD
      If (RETOUR0 /= 0) Then
        Erreur%Numero = 411
        Erreur%ft   = err_411
        Erreur%ft_c = err_411c
        call TRAITER_ERREUR (Erreur, 'Contrainte critique de depot des vases')
        return
      Endif
    Endif

  Enddo

!=========================================================================
! LECTURE DU POURCENTAGE CRITIQUE DE SABLE
! (pour utilisation des lois de sable)
!=========================================================================

  call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour0)

  read(chaine,*, iostat = retour0) DesignVar, LimiteSable
  LimiteSable = 60

  If (RETOUR0 /= 0) Then
    Erreur%Numero = 411
    Erreur%ft   = err_411
    Erreur%ft_c = err_411c
    call TRAITER_ERREUR (Erreur,'Pourcentage limite de sable')
    return
  Endif

!=========================================================================
! LECTURE DES PARAMETRES DES BERGES (ou TALUS)
!=========================================================================

! Lecture du Modele de berges
!----------------------------
  call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(1))
  read(chaine,*, iostat = retour(1)) DesignVar, Talus%Modele

! Lecture de la pente de stabilite des talus immerges
!----------------------------------------------------
  call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(2))
  read(chaine,*, iostat = retour(2)) DesignVar, Talus%PstabI

! Lecture de la pente de stabilite des talus emerges
!---------------------------------------------------
  call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(3))
  read(chaine,*, iostat = retour(3)) DesignVar, Talus%PstabE

  If (sum(retour) /= 0) Then
    Erreur%Numero = 411
    Erreur%ft   = err_411
    Erreur%ft_c = err_411c
    call TRAITER_ERREUR (Erreur,'Modele ou une des pentes de stabilite de talus')
    return
  Endif

! Allocation de memoire du tableau des talus
!-------------------------------------------
  allocate (Talus%Gamma(NbCouche),STAT = retour0)
  If (retour0 /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'CoucheSed')
    return
  Endif

! Coef de resistance residuelle et poids volumiques (eau et sediments)
!---------------------------------------------------------------------
  If (Talus%Modele == MODELE_TALUS_GLISSEMENT) then

    ! Coefficient d'homothetie
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(1))
    read(chaine,*, iostat = retour(1)) DesignVar, Talus%Lambda

    ! Coef de resistance residuelle
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(2))
    read(chaine,*, iostat = retour(2)) DesignVar, Talus%ResRed

    ! Poids volumique de l'eau
    call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour(3))
    read(chaine,*, iostat = retour(3)) DesignVar, Talus%GammaW

    ! Poids volumique des sediments
    Do iCouche = 1, NbCouche
      call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, &
                          retour(iCouche+3))
      read(chaine,*, iostat = retour(iCouche+3)) DesignVar, &
                                                 Talus%Gamma(iCouche)
    End do

    ! Message si erreur de lecture
    If (sum(retour) /= 0) Then
      Erreur%Numero = 411
      Erreur%ft   = err_411
      Erreur%ft_c = err_411c
      call TRAITER_ERREUR (Erreur,'Coefficient ou poids volumique de talus')
      return
    Endif

  Else
    ! Cas d'un modele de talus autre que le glissement
    ! ------------------------------------------------

    Erreur%Numero   = 409
    Erreur%ft   = err_409
    Erreur%ft_c = err_409c
    call TRAITER_ERREUR    (Erreur, 'Modele de talus = glissement ', &
                                        '=> Parametres de berge nuls')

    Talus%Lambda = 0.
    Talus%ResRed = 0.
    Talus%GammaW = 0.
    Do iCouche = 1, NbCouche
      Talus%Gamma(iCouche) = 0.
    Enddo

  Endif

!=========================================================================
! LECTURE DES COEFFICIENTs DE DIFFUSION
!=========================================================================

  call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour0)
  read(chaine,*, iostat = retour0) DesignVar, CnuxV

  If (retour0 /= 0) Then
    Erreur%Numero = 411
    Erreur%ft   = err_411
    Erreur%ft_c = err_411c
    call TRAITER_ERREUR (Erreur,'Coefficient de diffusion')
    return
  Endif

  call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour0)
  read(chaine,*, iostat = retour0) DesignVar, CnuxS

  If (retour0 /= 0) Then
    Erreur%Numero = 411
    Erreur%ft   = err_411
    Erreur%ft_c = err_411c
    call TRAITER_ERREUR (Erreur,'Coefficient de diffusion')
    return
  Endif

!=========================================================================
! LECTURE DES PARAMETRES DU SCHEMA DE CONVECTION
!=========================================================================
! call LIRE_CHAINE_S (chaine, FichierSedim, CHAINE_COMMENTAIRE, retour0)

!       allocate (ConsConv(1) , STAT = retour)
!
!  read(chaine,*, iostat = retour0) DesignVar, ConsConv(1)%Conv
!  read(chaine,*, iostat = retour0) DesignVar, ConsConv(1)%Scheconv
!  read(chaine,*, iostat = retour0) DesignVar, ConsConv(1)%OrdreVF
!  read(chaine,*, iostat = retour0) DesignVar, ConsConv(1)%ParamW
!  read(chaine,*, iostat = retour0) DesignVar, ConsConv(1)%LimiteurPente

!     if (ConsConv(1)%Scheconv > 4) Then
!        Erreur%Numero = 508
!        Erreur%ft   = err_508
!        Erreur%ft_c = err_508c
!        call TRAITER_ERREUR  (Erreur, 'Schema de convection', '< ou egal a 4')
!        return
 !    endif
!
!     if (ConsConv(1)%OrdreVF > 3) Then
!        Erreur%Numero = 508
!        Erreur%ft   = err_508
!        Erreur%ft_c = err_508c
!        call TRAITER_ERREUR  (Erreur, 'Ordre du schema VF', '< ou egal a 3')
!        return
!    endif

!     if ((ConsConv(1)%ParamW < -1).or.(ConsConv(1)%ParamW > 1)) Then
!        Erreur%Numero = 508
!        Erreur%ft   = err_508
!        Erreur%ft_c = err_508c
!        call TRAITER_ERREUR  (Erreur, 'Parametre W', 'compris entre -1 et 1')
!        return
!    endif


!=========================================================================
! FIN DU TRAITEMENT
!=========================================================================

  close (UniteSedim)

!  Erreur%arbredappel = arbredappel_old


!=========================================================================
! SERIE DE FORMATS
!=========================================================================

! FORMAT DE LECTURE
! -----------------

  return

!=========================================================================
!    FIN DU SOUS-PROGRAMME
!=========================================================================
End Subroutine LecFichierSedim
