Subroutine  StockPTransCourlis  ( &

  FicStockPTransCourlis   , & ! Unite logique fichier listing
  PhaseStockPTransCourlis , & ! Phase de la simulation (init, calcul, ??)
  Temps                   , & ! Temps courant
  NbProfil                , & ! Nombre de profils
  NbInterface             , & ! Nombre d'interfaces
  ProfilCourlis           , & ! Profils sedimentaires
  Zsurf                   , & ! Cote de la surface libre
  TauH                    , & ! Contrainte hydr. loc. (depend du tirant d'eau local)
  TauE                    , & ! Contrainte hydr. eff. (depend du rayon hydr.)
  Ceq                     , & ! Conc. d'equilibre des sables
  Erreur                  )

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       05/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :  Ecriture du fichier resultat de Courlis pour POSTCOURLIS
!  --------    - trace des profils en travers
!
!  Sous-programme appelant : Superviseur
!  -----------------------
!
!  Sous-programme appele : SurfaceLibre
!  ---------------------
!
!=========================================================================

use M_PRECISION           ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info

use M_FICHIER_T           ! Definition du type FICHIER_T
use M_PROFIL_COURLIS_T    ! Definition du type PROFIL_COURLIS

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

use M_SurfaceLibre_I      ! Interface du sous-programme SurfaceLibre

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

!
! Variables d'entree
!-------------------
  real(DOUBLE)        :: Temps
  integer, intent(in) :: NbInterface
  integer, intent(in) :: NbProfil
  integer, intent(in) :: PhaseStockPTransCourlis

  type(FICHIER_T)             , intent(in) :: FicStockPTransCourlis
  real(DOUBLE), dimension(:)  , intent(in) :: Zsurf
  real(DOUBLE), dimension(:,:), intent(in) :: TauH, TauE, Ceq

  type(PROFIL_COURLIS_T), dimension(:), intent(in)  :: ProfilCourlis

! Variables locales
!------------------
  integer :: NbCouche  ! Nb de couches de sediments
  integer :: i, j, k    ! indices
  integer :: NbVar
  integer :: Unite      ! Unite logique de fichier ouvert en ecriture
  real(DOUBLE), dimension(:), pointer  :: PtRiveG, PtRiveD

  ! Noms de variables
  character(10) :: C_ProfAbs , C_Zdur , C_TauH , C_TauE , C_Ceq
  character(10) :: Cu_ProfAbs, Cu_Zref, Cu_TauH, Cu_TauE, Cu_Ceq
  character(5), dimension(7) :: C_Zref

! Traitement des erreurs
!-----------------------
  integer        :: retour
!  character(132) :: arbredappel_old  ! PU2017 : Mise en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================


!=========================================================================
! INITIALISATIONS
!=========================================================================

  Erreur%Numero      = 0
!  arbredappel_old    = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>StockPTransCourlis'

  NbVar  = min(NbInterface+4,10)

  NbCouche = NbInterface - 1

  Unite = FicStockPTransCourlis%Unite

  C_Zref(1) = 'ZREF1'
  C_Zref(2) = 'ZREF2'
  C_Zref(3) = 'ZREF3'
  C_Zref(4) = 'ZREF4'
  C_Zref(5) = 'ZREF5'
  C_Zref(6) = 'ZREF6'
  C_Zref(7) = 'ZREF7'

  C_ProfAbs  = 'DXSC '
  C_Zdur     = 'ZDUR '
  C_TauH     = 'TauH '
  C_TauE     = 'TauE '
  C_Ceq      = 'Ceq  '

  Cu_ProfAbs = 'm    '
  Cu_Zref    = 'm    '
  Cu_TauH    = 'N/m2 '
  Cu_TauE    = 'N/m2 '
  Cu_Ceq     = 'g/l  '

!=========================================================================
! DEFINITION DE LA SURFACE LIBRE
!=========================================================================

  If ((PhaseStockPTransCourlis == PHASE_INITIALISATION) .AND. &
     (NbVar < NbInterface+1)) Then
    Erreur%Numero = 409
    Erreur%ft   = err_409
    Erreur%ft_c = err_409c
    call TRAITER_ERREUR (Erreur, 'Visualisation des profils : ', &
                         'Seules les 7 premieres couches seront &
                         prises en compte.')
  EndIf

  call SurfaceLibre ( &
    NbProfil        , &
    PtRiveG         , &
    PtRiveD         , &
    ProfilCourlis   , &
    Zsurf           , &
    Erreur          )

!=========================================================================
! ECRITURE DE L'EN TETE
!=========================================================================

  If (PhaseStockPTransCourlis == PHASE_INITIALISATION) Then

    open(unit=Unite   , file=FicStockPTransCourlis%Nom, access='SEQUENTIAL', &
         action='WRITE'    , form='FORMATTED'     , iostat=RETOUR      , &
         position='rewind', status='OLD'    )

    If (RETOUR /= 0) Then
      Erreur%Numero = 4
      Erreur%ft   = err_4
      Erreur%ft_c = err_4c
      call TRAITER_ERREUR (Erreur, FicStockPTransCourlis%Nom)
      return
    Endif

    write(Unite,1000)
    write(Unite,1010) NbProfil
    Do i = 1, NbProfil
      write(Unite,1020) i, ProfilCourlis(i)%NbPoint !!!!!!!!!!!!!!!cette fonction devra etre adaptee pour POSTCOURLIS
    Enddo
    write(Unite,1030) NbVar

    write(Unite,1040) C_ProfAbs, Cu_ProfAbs

    Do k = 1, NbCouche
      write(Unite,1040) C_Zref(k), Cu_Zref
    Enddo

    write(Unite,1040) C_Zdur, Cu_Zref
    write(Unite,1040) C_TauH, Cu_TauH
    write(Unite,1040) C_TauE, Cu_TauE
    write(Unite,1040) C_Ceq , Cu_Ceq

  Endif

!=========================================================================
! ECRITURE DES DONNEES
!=========================================================================

  write(Unite,1050) real(Temps)

  Do i = 1, NbProfil
    write(Unite,1060) real(ProfilCourlis(i)%Abs)
    write(Unite,1070) real(Zsurf(i))
    write(Unite,1080) PtRiveG(i), PtRiveD(i)

    Do j = 1, ProfilCourlis(i)%NbPoint
      write(Unite,1090) ProfilCourlis(i)%X(j)                      , &
                        (ProfilCourlis(i)%Z(k,j), k=1,NbInterface) , &
                        TauH(j,i), TauE(j,i), Ceq(j,i)
    Enddo
  Enddo

!=========================================================================
! FERMETURE DU FICHIER
!=========================================================================
  If (PhaseStockPTransCourlis == PHASE_TERMINAISON)    close(Unite)

!=========================================================================
! FORMATS
!=========================================================================

  1000 format('Programme COURLIS : Fichier resultat pour POSTCOUR', /, &
              '--------------------------------------------------')
  1010 format('''NbProfil ===>''', I5)
  1020 format('''NbPoint Profil ', i4, ' ===>''',I5)
  1030 format('''Nombre de variables stockees ===>''' ,I5)
  1040 format(2A10)
  1050 format('''Temps''', F18.3)
  1060 format('''Abscisse''', F14.2)
  1070 format('''Surface libre''', F9.2)
  1080 format('''PT RG ET RD a la cote surface libre''', 2F8.3)
  1090 format(11F10.3)

!=========================================================================
! FIN DU SOUS-PROGRAMME
!=========================================================================

!  Erreur%arbredappel = arbredappel_old

End Subroutine StockPTransCourlis

