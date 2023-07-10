Subroutine  LecApportCourlis( &

    UniteList         , & ! Unite du fichier d'impression des parametres
    ImpressionApport  , & ! Choix d'impression des apports en vase et sable
    Apport            , & ! Variable definissant les donnees d'apport de l'hydro
    NbLoiConc         , & ! Nombre de Lois de concentration
    ApportVase        , & ! Var. contenant les donnees sedim. des apports de vase
    ApportSable       , & ! Var. contenant les donnees sedim. des apports de sable
    CL_Vase           , & ! CL amont de la concentration en Vase
    CL_Sable          , & ! CL amont de la concentration en Sable
! Lecture des mots du dictionnaires
    MOTINT            , &
    ADRESS            , &
! Traitement des erreurs
    Erreur             ) ! Erreur

!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL
!
!  VERSION : 4.0       02/2003    Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture des donnees sedimentaires relatives aux apports
!  --------
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele :
!  ---------------------
!
!=========================================================================

use M_MY_GLOBAL_VAR_SED
use M_SOURCE_TRACER_T     ! Definition du type TRACEUR_T
use M_APPORT_T            ! Definition du type APPORT_T
use M_CL_COURLIS_T        ! Definition du type CL_COURLIS_T

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  type(APPORT_T), dimension(:)  , pointer       :: Apport
  integer       , dimension(:)  , intent(in   ) :: MOTINT
  integer       , dimension(:,:), intent(in   ) :: ADRESS

  logical            , intent(in   ) :: ImpressionApport
  integer            , intent(in   ) :: UniteList
  integer            , intent(in   ) :: NbLoiConc

! Variables de sortie
  type(CL_COURLIS_T),                  intent(  out) :: CL_Vase, CL_Sable
  type(SOURCE_TRACER_T), dimension(:), pointer       :: ApportVase
  type(SOURCE_TRACER_T), dimension(:), pointer       :: ApportSable


! Variables locales
  integer :: NbApports     ! Nombre d'apports
  integer :: iApport       ! Numero ou indice de l'apport
  integer :: LoiVaseAmont  ! Numero de la loi pour la CL amont en vase
  integer :: LoiSableAmont ! Numero de la loi pour la CL amont en sable
  character(72) :: txt     ! Chaine de caractere temporaire

! Traitement des erreurs
  integer                       :: retour    ! Code de retour Read
  type(ERREUR_T), intent(inout) :: Erreur
!  character(132)        :: arbredappel_old  ! PU2017 : Mise en commentaire

!=========================================================================

  If (ImpressionApport) write(UniteList,1000)

!=========================================================================
! INITIALISATION
!=========================================================================

  Erreur%Numero      = 0
!  arbredappel_old   = trim(Erreur%arbredappel)  ! PU2017 : Mise en commentaire
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LecApportCourlis'


!=========================================================================
! LECTURE DU NOMBRE D'APPORTS
!=========================================================================
  NbApports = size(Apport)
  If (NbApports < 0) Then
    Erreur%Numero = 408
    Erreur%ft   = err_408
    Erreur%ft_c = err_408c
    call TRAITER_ERREUR (Erreur, 'Le nombre d''apports', '> ou egal a 0')
    return
  End if

  ! Allocation de memoire du tableau des apports en vase
  !-----------------------------------------------------
  allocate (ApportVase(NbApports), STAT = retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'ApportVase')
    return
  End if

  ! Allocation de memoire du tableau des apports en sable
  !------------------------------------------------------
  allocate (ApportSable(NbApports), STAT = retour)
  If (retour /= 0) Then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR (Erreur, 'ApportSable')
    return
  End if

  If(ImpressionApport) Then
    write(UniteList,1010) NbApports
    write(UniteList,1020) 'TypeVar','Vase','Sable'
  End if

!=========================================================================
! BOUCLE DE DEFINITION SUR LES APPORTS
!=========================================================================

  Do iApport = 1, NbApports

    ! Nom de la source
    ApportVase(iApport)%Nom     = Apport(iApport)%Nom
    ApportSable(iApport)%Nom    = Apport(iApport)%Nom

    ! Numero de la loi associee
    ApportVase(iApport)%NumeroLoi  = MOTINT(ADRESS(1,614)+iApport-1)
    ApportSable(iApport)%NumeroLoi = MOTINT(ADRESS(1,615)+iApport-1)

    ! Test sur les numeros de loi ==> a adapter pour le bedload (glute pour l'instant)
    If (ApportVase(iApport)%NumeroLoi  <= 0 .and. &
        suspension_option .eqv. .true. )    Then
      write(txt,'(a,i3,a)') 'Numero de loi de l''apport ', &
                            iApport                      , &
                            ' de Vase ou Sable'
      Erreur%Numero = 408
      Erreur%ft   = err_408
      Erreur%ft_c = err_408c
      call TRAITER_ERREUR  (Erreur, trim(txt), '> a 0')
      return
    End if

    If (ApportVase(iApport)%NumeroLoi  > NbLoiConc)    Then
      write(txt,'(a,i3,a)') 'Numero de loi de l''apport ', &
                            iApport                      , &
                            ' de Vase ou Sable'
      Erreur%Numero = 408
      Erreur%ft   = err_408
      Erreur%ft_c = err_408c
      call TRAITER_ERREUR  (Erreur, txt, '<= a Nb de lois de concentration.')
      return
    End if

    ! Type de la source
    ApportVase(iApport)%Type       = 1
    ApportSable(iApport)%Type      = 1

    ! Numero de la section debut
    ApportVase(iApport)%SectionAm     = Apport(iApport)%SectionAm
    ApportSable(iApport)%SectionAm    = Apport(iApport)%SectionAm

    ! Numero de la section fin
    ApportVase(iApport)%SectionAv     = Apport(iApport)%SectionAv
    ApportSable(iApport)%SectionAv    = Apport(iApport)%SectionAv

    ! quantite de source
    ApportVase(iApport)%debit_source   = 0.
    ApportSable(iApport)%debit_source  = 0.

    ! quantite de source
    ApportVase(iApport)%Numero_traceur   = 1
    ApportSable(iApport)%Numero_traceur  = 2

    ! Impression des valeurs
    If (ImpressionApport) Then
      write(UniteList,1030) 'Nom      '             , &
                            ApportVase(iApport)%Nom , &
                            ApportSable(iApport)%Nom
      write(UniteList,1040) 'NumeroLoi'                   , &
                            ApportVase(iApport)%NumeroLoi , &
                            ApportSable(iApport)%NumeroLoi
      write(UniteList,1040) 'Type     '              , &
                            ApportVase(iApport)%Type , &
                            ApportSable(iApport)%Type
      write(UniteList,1040) 'SectionAm'                   , &
                            ApportVase(iApport)%SectionAm , &
                            ApportSable(iApport)%SectionAm
      write(UniteList,1040) 'SectionAv'                   , &
                            ApportVase(iApport)%SectionAv , &
                            ApportSable(iApport)%SectionAv
      write(UniteList,1050) 'Q_source '                      , &
                            ApportVase(iApport)%debit_source , &
                            ApportSable(iApport)%debit_source
      write(UniteList,1040) 'No. Trac.'                        , &
                            ApportVase(iApport)%Numero_traceur , &
                            ApportSable(iApport)%Numero_traceur
    Endif

  Enddo

!=========================================================================
! NUMERO DE LOI DE CONCENTRATION POUR LA CONDITION LIMITE AMONT
!=========================================================================

  LoiVaseAmont  = MOTINT(ADRESS(1,612))
  LoiSableAmont  = MOTINT(ADRESS(1,613))

  ! Test sur les numeros de loi ==> a adapter pour le bedload (glute pour l'instant)
  If ((LoiVaseAmont <= 0) .and. suspension_option .eqv. .true.) Then
    Erreur%Numero = 408
    Erreur%ft   = err_408
    Erreur%ft_c = err_408c
    call TRAITER_ERREUR (Erreur, 'Le numero de loi amont (VASE)','> a 0')
    return
  Endif

  If ((LoiVaseAmont > NbLoiConc)) Then
    Erreur%Numero = 408
    Erreur%ft   = err_408
    Erreur%ft_c = err_408c
    call TRAITER_ERREUR (Erreur, 'Numero de loi amont VASE',  &
                   '<= a Nb de lois de concentration.')
    return
  Endif

  CL_Vase%Nom = "Condition limite amont en vase"
  CL_Vase%NumeroLoi = LoiVaseAmont

  CL_Sable%Nom = "Condition limite amont en sabl"
  CL_Sable%NumeroLoi = LoiSableAmont


  If (ImpressionApport) Then
    write(UniteList,*)
    write(UniteList,1040) 'Loi amont', LoiVaseAmont, LoiSableAmont
  Endif

!  Erreur%arbredappel = arbredappel_old

!=========================================================================
! FORMATS
!=========================================================================

  1000 format (/,'APPORT EN VASE',/, &
             &   '--------------',/)
  1010 format ('Nombre d''apports en vase      : ',i3)
  1020 format (/,A10,2x,A35)
  1030 format (/,A10,2x,A35  )
  1040 format (A10,2x,I35  )
  1050 format (A10,2x,F35.3)

!=========================================================================
! FIN DU SOUS-PROGRAMME
!=========================================================================
End Subroutine LecApportCourlis
