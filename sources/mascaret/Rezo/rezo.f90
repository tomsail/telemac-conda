!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

subroutine  REZO    ( &
! Donnees / Resultats
    Z               , & ! Cote de la surface libre
    Q1, Q2          , & ! Debits mineur et majeur
    P1, P2          , & ! Perimetres mouilles mineur et majeur
    B1, B2, BS      , & ! Largeurs au miroir mineur, majeur et de stockage
    RH1, RH2        , & ! Rayons hydrauliques mineur et majeur
    S1, S2          , & ! Sections mouillee mineur et majeur
    DTLEVY          , & ! Pas de temps optimal
    Beta            , & ! Coefficient Beta de repartition des lits
    Froude          , & ! Nombre de Froude
    Extremite       , & ! Extremites libres
    Apport          , & ! Debits d'apport
    Qinjec          , & ! Debit injecte
    Qdeverse        , & ! debit total deverse par un deversoir lateral ponctuel ou lineique
    Temps           , & ! Temps
    PhaseSimulation , & ! Phase de la simulation
! Profils
    Profil          , & ! Profils geometriques
    ProfilPlan      , & ! Profils planimetrees
! Modele
    X                , & ! Maillage
    CF1, CF2         , & ! Coefficients de frottement mineur et majeur
    ZREF             , & ! Cote de reference
    XDT              , & ! Position section/profil amont
    IDT              , & ! Numero du profil amont d'une section
    Connect          , & ! Table de connectivite
    Singularite      , & ! Singularites
    PCSing           , & ! Pertes de charges singulieres
    Deversoir        , & ! Deversoirs
    ModeleLit        , & ! Modele du lit
    Confluent        , & ! Caracteristiques des confluences
    Abaque           , & ! Abaques des pertes de  charges aux confluences
! Parametres
    DTImpression     , & ! Pas de temps d'impression
    Impression       , & ! Flag d'autorisation d'impression
    UniteListing     , & ! Unite logique du fichier listing
    LoiFrottement    , & ! Loi de frottement
    PerteChargeConfluent,& ! Perte de charge automatique aux confluents
! Etat
    TempsPrecedent   , & ! Temps precedent
    Tinitial         , & ! Temps de debut de simulation
    NumeroPas        , & ! Numero du pas de temps
    DPDZ1, DPDZ2     , & ! Derivee de P1 et de P2 / Z
    OptionCasier     , & ! Flag de presence de casiers
    Liaison          , & ! Caracteristiques des liaisons RIVIERE-CASIER et CASIER-CASIER
    Casier           , & ! Caracteristiques des casiers
    ApportPluie      , & ! Apport de pluie des casiers
    NoConvection     , & ! Attenuation de la convection
    CQMV             , & ! qmv des debits d'apport
    Erreur             & ! Erreur
                       )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE      S. PERON
!                             E. BEN SLAMA    S. MANDELKERN
!                             F. ZAOUI
!                             S. DELMAS       C. COULET
!
! VERSION : V8P4R0               EDF-CEREMA-ARTELIA
! *********************************************************************
! FONCTION :
!
! TRAITEMENT D'UN PAS DE TEMPS A L'AIDE DU CODE REZO
!
! .....................................................................
! DANS LES COMMENTAIRES :
! DQ DESIGNE LA VARIATION DE DEBIT D'UN PAS DE TEMPS AU SUIVANT
! DANS UNE SECTION DE CALCUL,
! ET DE MEME DZ DESIGNE LA VARIATION DE COTE
! .....................................................................
!
! .....................................................................
! . FICHIERS EN SORTIE                                                .
! .....................................................................
! . NUMERO       . SUPPORT PHYSIQUE . ROLE
! .....................................................................
! . UniteListing . FICHIER LISTING  . IMPRESSION DES PRINCIPAUX RESULTATS
! .....................................................................
! .....................................................................
! . PROGRAMME APPELANT                : Superviseur
! .....................................................................

   !============================= Declarations ===========================
   use M_PRECISION           ! Type DOUBLE
   use M_PARAMETRE_C         ! Parametres de calcul
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes parametres du calcul
   use M_APPORT_T            ! Definition du type APPORT_T
   use M_CONFLUENT_T         ! Type confluent
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_DEVERSOIR_T         ! Definition du type DEVERSOIR_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_EXTREMITE_T         ! Definition du type EXTREMITE_T
   use M_PROFIL_T            ! Definition du type PROFIL_T
   use M_PROFIL_PLAN_T       ! Definition du type PROFIL_PLAN_T
   use M_SINGULARITE_T       ! Definition du type SINGULARITE_T
   use M_REZOMAT_T           ! Definition du type REZOMAT_T
   use M_INTERPOLATION_S     ! Interpolation
   use M_RHSBP_S             ! Calcul de RH, S, B et P
   use M_NUM_BIEF_S          ! Calcul du numero du bief d'une section
   use M_FROUDE_S            ! Calcul et controle du nombre de Froude
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs
   use M_CALCUL_I            ! Calcul
   use M_CHAINAGE_REZO_I     ! Chainage de la matrice
   use M_SINGULARITE_REZO_I  ! Lien direct section -> no. singularite
   use M_CONTQ_I             ! Controle de la conservation du debit aux noeuds
   use M_CCL_I               ! Transformation pour calcul des conditions aux limites
   use M_CQINJ_I             ! Transformation pour calcul des de bits d'apport
   use M_KSING_I             ! Traitement des singularites
   use M_CALC_PC_CONFLU_I    ! Calcul des pertes de charge aux conf
   use M_DPDZ_I              ! Calcul du derive du perimetre mouille / cote
   use M_REPAR_I             ! Calcul de la repartition lit mineur / majeur
   use M_CONSTANTES_CASIER_C ! Modules pour l'implicitation des casiers
   use M_CASIER_T
   use M_LIAISON_T
   use M_APPORT_PLUIE_T
   use M_KLIAISON_I

   !.. Implicit Declarations
   implicit none

   !.. Arguments ..
   real(DOUBLE), dimension(:), intent(inout) :: Z, Q1, Q2
   real(DOUBLE), dimension(:), intent(inout) :: P1, P2
   real(DOUBLE), dimension(:), intent(inout) :: B1, B2, BS
   real(DOUBLE), dimension(:), intent(inout) :: RH1, RH2
   real(DOUBLE), dimension(:), intent(inout) :: S1, S2
   real(DOUBLE)              , intent(  out) :: DTLEVY
   real(DOUBLE), dimension(:), intent(  out) :: Beta
   real(DOUBLE), dimension(:), intent(  out) :: Froude
   ! Maillage
   real(DOUBLE), dimension(:), intent(in   ) :: X, CF1, CF2
   real(DOUBLE), dimension(:), intent(in   ) :: ZREF
   real(DOUBLE), dimension(:), intent(in   ) :: XDT
   integer     , dimension(:), intent(in   ) :: IDT
   ! Conditions aux limites
   type(EXTREMITE_T)   , dimension(:), intent(inout) :: Extremite
   ! Variables planimetrees
   type (PROFIL_T)     , dimension(:), intent(in   ) :: Profil
   type (PROFIL_PLAN_T)              , intent(in   ) :: ProfilPlan
   ! Debits d apports
   type(APPORT_T)      , dimension(:), intent(in   ) :: Apport
   ! Temps
   real(DOUBLE)                      , intent(in   ) :: Temps
   integer                           , intent(in   ) :: PhaseSimulation
   real(DOUBLE)                      , intent(in   ) :: DTImpression
   ! Pertes de charge singulieres
   real(DOUBLE)        , dimension(:), intent(inout) :: PCSing
   ! Confluences
   type(CONFLUENT_T)   , dimension(:) , intent(in   ) :: Confluent
   real(DOUBLE)    , dimension(:,:,:) , intent(in   ) :: Abaque
   ! Table de connectivite du reseau
   type(CONNECT_T)                   , intent(in   ) :: Connect
   ! Singularites
   type (SINGULARITE_T), dimension(:), pointer , intent(inout) :: Singularite
   ! Parametres
   integer                           , intent(in   ) :: ModeleLit
   logical                           , intent(in   ) :: Impression
   integer                           , intent(in   ) :: UniteListing
   integer                           , intent(in   ) :: LoiFrottement
   logical                           , intent(in   ) :: PerteChargeConfluent
   ! Deversoirs
   type (DEVERSOIR_T)  , dimension(:), intent(in   ) :: Deversoir
   real(DOUBLE)        , dimension(:), intent(inout) :: Qdeverse
   ! Etats
   real(DOUBLE)                      , intent(inout) :: Tinitial
   real(DOUBLE)                      , intent(inout) :: TempsPrecedent
   integer                           , intent(inout) :: NumeroPas
   real(DOUBLE)        , dimension(:), pointer       :: DPDZ1, DPDZ2
   ! Casier
   logical                           , intent(in   ) :: OptionCasier
   type(CASIER_T)       , dimension(:)  , pointer, intent(inout) :: Casier
   type(LIAISON_T)      , dimension(:)  , pointer , intent(inout) :: Liaison
   type(APPORT_PLUIE_T ), dimension(:)  , pointer, intent(inout   ) :: ApportPluie
   ! Flag calcul particulier
   logical                           , intent(in   ) :: NoConvection
   integer                           , intent(in   ) :: CQMV
   ! Erreur
   type(ERREUR_T)                    , intent(inout) :: Erreur
   ! Tableaux de travail
   ! Matrice du reseau
   type(REZOMAT_T)                                   :: Matrice
   ! Debits d'apports generalise (avec deversoirs)
   real(DOUBLE), dimension(:)    ,intent (inout)     :: QINJEC
   real(DOUBLE), dimension(size(X))                  :: Q
   ! Coefficients de l'equation discretisee d'une singularites
   real(DOUBLE), dimension(:), allocatable :: ASING, BSING, CSING, DSING
   ! Debit passant sur cette singularite
   real(DOUBLE), dimension(:), allocatable :: QSING
   ! Coefficients de l'equation discretisee d'une cond a la limite
   ! RDLIM*DQ + SDLIM*DZ = TDLIM
   real(DOUBLE), dimension(size(Extremite))   :: RDLIM, SDLIM, TDLIM
   !.. Scalairs locaux ..
   logical        :: impression_sing   ! Impression info sur singularites
   integer        :: I                 ! Compteur
   integer        :: ibief             ! Compteur sur les biefs
   integer        :: ising             ! Compteur sur les singularites
   integer        :: iliai             ! Compteur sur les liaisons
   integer        :: iext              ! Compteur sur les extremites libres
   integer        :: NumSeuil          ! Numero de seuil
   integer        :: NS                ! Numero de section de calcul
   integer        :: appel_KSING       ! phase d'appel de KSING (1 ou 2)
   integer        :: II1, II2
   real(DOUBLE)   :: Celerite
   real(DOUBLE)   :: DT, dt_lim
   real(DOUBLE)   :: HI, VI, v_amont_sing
   real(DOUBLE)   :: v_moyen           ! Vitesse moyenne
   real(DOUBLE)   :: debitance         ! Debitance
   integer        :: num_bief          ! Numero de bief
   real(DOUBLE)   :: abs_rel           ! abscisse relative sur un bief
   integer        :: retour            ! Code de retour des fonctions intrinseques
   integer        :: dim               ! Dimension des tableaux ASING...
   ! Integration des casiers :
   integer      :: dimLiaison
   integer      :: NumLiai, Appel_Kliaison
   real(DOUBLE) :: ZAM, ZAV, ZfAM, ZfAV
   real(DOUBLE)         , dimension(:)  , allocatable   :: Aliai, Bliai, Cliai, Dliai, Qliai
   ! Save
   Save DT
   Save Matrice

   ! Les Constantes sont declares dans le module M_PARAMETRES

   !.. Fonctions intrinseques ..
   intrinsic DMIN1, MOD, DSQRT

   !============================ Instructions ==============================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   retour        = 0
   ! arbredappel_old    = trim(Erreur%arbredappel)
   ! Erreur%arbredappel = trim(Erreur%arbredappel)//'=>REZO'

   ! Calcul du debit total
   !----------------------
   Q(:) = Q1(:) + Q2(:)

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      ! Initialisation du pas de temps et du temps initial
      !---------------------------------------------------
      DT       = 0._DOUBLE
      Tinitial = Temps

      ! Quel solveur prendre?
      !----------------------
      if(size(Connect%ORIGINEBIEF).gt.1) then
         Matrice%SOLV = 2
      elseif(OptionCasier.eqv..true.) then
         Matrice%SOLV = 2
      else
         Matrice%SOLV = 1
      endif

      ! Allocations
      !------------
      allocate( DPDZ1(size(X(:))) , stat = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'DPDZ1' )
         return
      end if

      allocate( DPDZ2(size(X(:))) , stat = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'DPDZ2' )
         return
      end if

      ! calcul du pattern de la matrice au format coordonnees du pb a resoudre
      call CHAINAGE_REZO( Connect , Extremite , Matrice , OptionCasier , Liaison , Casier , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      ! table de correspondance section -> singularite
      call SINGULARITE_REZO( Matrice , Singularite , X , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

   elseif( PhaseSimulation.eq.PHASE_TERMINAISON ) then

      ! desallocation necessaire en fin de calcul
      deallocate( DPDZ1 )
      deallocate( DPDZ2 )
      deallocate( Matrice%rowA )
      deallocate( Matrice%valA )
      deallocate( Matrice%colA )
      deallocate( Matrice%noVarDQ )
      deallocate( Matrice%noVarDZ )
      if( OptionCasier ) then
          deallocate( Matrice%noVarDQl )
          deallocate( Matrice%noVarDZc )
          deallocate( Matrice%LiaiSec )
      endif
      deallocate( Matrice%SecLiai )
      deallocate( Matrice%typSec )
      deallocate( Matrice%SecSin )
      deallocate( Matrice%b )
      if(Matrice%SOLV.eq.2) then
         deallocate( Matrice%IFLAG )
         deallocate( Matrice%AFLAG )
         deallocate( Matrice%RNR )
         deallocate( Matrice%SNR )
         deallocate( Matrice%Pivot )
         deallocate( Matrice%ha )
      else
         deallocate( Matrice%ipiv )
         deallocate( Matrice%AB )
      endif
      if( associated( Matrice%headConflu ) ) deallocate( Matrice%headConflu )
      if( associated( Matrice%nextSecConflu ) ) deallocate( Matrice%nextSecConflu )

      return
   else

      ! Calcul du pas de temps
      !-----------------------
      DT = Temps - TempsPrecedent

      ! calcul de la perte de charge aux confluences
      !---------------------------------------------
      if( PerteChargeConfluent ) then

         call CALC_PC_CONFLU       ( &
             PCSing                , &
             Z                     , &
             Q                     , &
             X                     , &
             ZREF                  , &
             Profil                , &
             ProfilPlan            , &
             Confluent             , &
             Abaque                , &
             IDT                   , &
             XDT                   , &
             Connect               , &
             UniteListing          , &
             Erreur                  &
                                   )
         if( Erreur%Numero /= 0 ) then
            return
         endif
      endif
   end if

   !---------------------------------------------------------------
   ! Determination de l'impression des information aux singularites
   !---------------------------------------------------------------
   impression_sing = .false.
   if( Impression .and. DTImpression >= 1._DOUBLE ) then
      impression_sing = MOD((Temps-Tinitial),DTImpression) < 1._DOUBLE
   end if

   !--------------------------------
   ! Initialisations du pas de temps
   !--------------------------------
   DTLEVY    = 0._DOUBLE
   QINJEC(:) = 0._DOUBLE

   if( associated(Singularite).eqv..false. ) then
      dim = 1
   else
      dim = size(Singularite)
   endif

   ! Allocations de tableaux locaux
   !-------------------------------
   allocate( ASING(dim) , stat = retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ASING' )
      return
   end if

   allocate( BSING(dim) , stat = retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'BSING' )
      return
   end if

   allocate( CSING(dim) , stat = retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'CSING' )
      return
   end if

   allocate( DSING(dim) , stat = retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'DSING' )
      return
   end if

   allocate( QSING(dim) , stat = retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'QSING' )
      return
   end if

   if ( associated(liaison).eqv..false. ) then
        dimLiaison = 1
    else
        dimLiaison = size(liaison)
    endif

    allocate( Aliai(dimLiaison) , stat = retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ALIAI' )
      return
   end if

   allocate( Bliai(dimLiaison) , stat = retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'BLIAI' )
      return
   end if

   allocate( Cliai(dimLiaison) , stat = retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'CLIAI' )
      return
   end if

   allocate( Dliai(dimLiaison) , stat = retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'DLIAI' )
      return
   end if

   allocate( Qliai(dimLiaison) , stat = retour )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'QLIAI' )
      return
   end if

   ! CALCUL DES CONDITIONS AUX LIMITES
   ! ---------------------------------
   call CCL                    ( &
       RDLIM, SDLIM, TDLIM     , & ! Conditions aux limites calcul
       Z, Q1, Q2               , & ! Cote, debit mineur et majeur
       ZREF                    , & ! Cote de reference
       CF1                     , & ! Coeff de frottement mineur
       IDT                     , & ! Positionnement des section/profils
       XDT                     , & ! Positionnement des section/profils
       Profil                  , & ! Profils geometriques
       ProfilPlan              , & ! Profils planimetres
       Extremite               , & ! Extremites libres
       Connect                 , & ! table de connectivite
       Temps                   , & ! Temps
       LoiFrottement           , & ! Loi de frottement
       Erreur                    & ! Erreur
                              )

   if( Erreur%Numero /= 0 ) then
      return
   end if

   ! CALCUL DE LA SOMME DES DEBITS D'APPORT DANS LA RIVIERE
   ! ------------------------------------------------------
   call CQINJ                 ( &
        QInjec                , & ! Resultats
        X, Z                  , & ! Donnees non modifiees
        Apport                , &
        Deversoir             , &
        Qdeverse              , &
        Erreur                  ) ! Erreur

   if( Erreur%Numero /= 0 ) then
      return
   end if

   !--------------------------
   ! CALCUL D'UN PAS DE TEMPS
   !--------------------------

   ! EFFECTUE SEULEMENT SI NOUS NE SOMMES PAS A L'ETAPE D'INITIALISATION
   if( PhaseSimulation == PHASE_CALCUL ) then

      ! CARACTERISTIQUES DES SEUILS
      ! ---------------------------
      boucle1 : do ising = 1 , size(Singularite)

         NumSeuil     = ising
         NS           = Singularite(ising)%Section
         appel_KSING  = 1
         v_amont_sing = Q1(NS) / S1(NS)

         call KSING                                          ( &
                 ASING(ising),BSING(ising)                   , &
                 CSING(ising),DSING(ising),QSING(ising)      , &
                 NumSeuil                                    , &
                 Singularite                                 , &
                 appel_KSING                                 , &
                 Z (NS), Z(NS+1)                             , &
                 Q (NS), Q(NS+1)                             , &
                 B1(NS)                                      , &
                 v_amont_sing                                , &
                 Connect                                     , &
                 X                                           , &
                 ZREF                                        , &
                 IDT                                         , &
                 XDT                                         , &
                 Profil                                      , &
                 ProfilPlan%B1                               , &
                 Impression_sing                             , &
                 UniteListing                                , &
                 Erreur                                        &
                                                            )

         if( Erreur%Numero /= 0 ) then
            return
         end if

      end do boucle1

      ! CARACTERISTIQUES DES LIAISONS
      !------------------------------
      if( OptionCasier ) then
          boucleLiaison : do iLiai = 1 , size(Liaison)

             NumLiai        = iLiai
             appel_KLIAISON = 1

             if ( Liaison(iLiai)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
                NS      = Liaison(iLiai)%CaracRC%Section
                ZAM     = Z(NS)
                ZfAM    = ZREF(NS)
                ZAV     = Casier(Liaison(iLiai)%CaracRC%NumCasier)%Cote
                ZfAV    = Casier(Liaison(iLiai)%CaracRC%NumCasier)%CoteFond

             else
                ZAM     = Casier( Liaison(iLiai)%CaracCC%CasierOrigine )%Cote
                ZfAM    = Casier( Liaison(iLiai)%CaracCC%CasierOrigine )%CoteFond
                ZAV     = Casier( Liaison(iLiai)%CaracCC%CasierFin )%Cote
                ZfAV    = Casier( Liaison(iLiai)%CaracCC%CasierFin )%CoteFond

             endif

             call KLIAISON                                       ( &
                     Aliai(iLiai),Bliai(iLiai)                   , &
                     Cliai(iLiai),Dliai(iLiai)                   , &
                     Qliai(iLiai)                                , &
                     Liaison(NumLiai)                            , &
                     appel_KLIAISON                              , &
                     ZAM, ZAV, ZfAM, ZfAV                        , &
                     Connect                                     , &
                     Impression_sing                             , &
                     UniteListing                                , &
                     Erreur                                        &
                                                                )

             if( Erreur%Numero /= 0 ) then
                return
             end if

          end do boucleLiaison
      endif

      ! CALCUL DES DERIVEES DES PERIMETRES MOUILLES
      ! --------------------------------------------------------
      do ibief = 1 , size(Connect%OrigineBief)
         II1 = Connect%OrigineBief(ibief)
         II2 = Connect%FinBief(ibief)
         do I = II1 , II2
            call DPDZ            ( &
                     DPDZ1(I)    , &
                     DPDZ2(I)    , &
                     I           , &
                     Z(I)        , &
                     ZREF(I)     , &
                     IDT         , &
                     XDT         , &
                     Profil      , &
                     ProfilPlan  , &
                     Connect     , &
                     X           , &
                     Impression  , &
                     UniteListing, &
                     Erreur        &
                                 )
            if( Erreur%Numero /= 0 ) then
               return
            end if
         enddo
      enddo

      ! RESOLUTION DU SYSTEME
      ! ---------------------
      call CALCUL                                     ( &
            Z, Q                                      , &
            Matrice                                   , &
            P1, P2, B1, B2, BS, S1, S2, RH1, RH2      , &
            DPDZ1, DPDZ2                              , &
            X                                         , &
            CF1, CF2                                  , &
            QINJEC                                    , &
            Connect                                   , &
            RDLIM,SDLIM,TDLIM                         , &
            PCSing                                    , &
            ModeleLit                                 , &
            DT, NumeroPas                             , &
            Impression                                , &
            UniteListing                              , &
            LoiFrottement                             , &
            ASING,BSING,CSING,DSING                   , &
            OptionCasier                              , &
            Aliai,Bliai,Cliai,Dliai,Qliai             , &
            ApportPluie                               , &
            Liaison, Casier                           , &
            NoConvection                              , &
            CQMV                                      , &
            Erreur                                      &
                                                      )

      if( Erreur%Numero /= 0 ) then
         return
      end if

      ! CORRECTION DES COTES AUX SORTIES LIBRES REGIES PAR UNE LOI Z(Q)
      ! ---------------------------------------------------------------
      do iext = 1 , size(Connect%NumSectionExtLibre)
         NS = Connect%NumSectionExtLibre(iext)
         if( Extremite(iext)%Type == CONDITION_TYPE_COTE_DEBIT ) then
            if( Z(NS) >= Extremite(iext)%PtZ(1) ) then
               call INTERPOLATION_S             ( &
                     Z(NS)                      , &
                     Q(NS)                      , &
                     1                          , &
                     Extremite(iext)%PtQ        , &
                     Extremite(iext)%PtZ        , &
                     size(Extremite(iext)%PtZ)  , &
                     Erreur                       &
                                          )
               if( Erreur%Numero /= 0 ) then
                  return
               end if
            end if
         elseif( Extremite(iext)%Type == CONDITION_TYPE_DEBIT_COTE ) then

            if( Q(NS) >= Extremite(iext)%PtQ(1) ) then
               call INTERPOLATION_S              ( &
                      Q(NS)                      , &
                      Z(NS)                      , &
                      1                          , &
                      Extremite(iext)%PtZ        , &
                      Extremite(iext)%PtQ        , &
                      size(Extremite(iext)%PtQ)  , &
                      Erreur                       &
                                                 )
               if( Erreur%Numero /= 0 ) then
                  return
               end if
            end if
         end if
      end do

      ! CORRECTION DES DEBITS PASSANT SUR CHAQUE SINGULARITE
      ! ----------------------------------------------------
      if( size(Singularite) > 0 ) then

         do ising = 1,size(Singularite)

            NS           = Singularite(ising)%Section
            NumSeuil     = ising
            appel_KSING  = 2
            v_amont_sing = Q1(NS) / S1(NS)

            call KSING                                      ( &
                ASING(ising),BSING(ising)                   , &
                CSING(ising),DSING(ising),QSING(ising)      , &
                NumSeuil                                    , &
                Singularite                                 , &
                appel_KSING                                 , &
                Z (NS), Z(NS+1)                             , &
                Q (NS), Q(NS+1)                             , &
                B1(NS)                                      , &
                v_amont_sing                                , &
                Connect                                     , &
                X                                           , &
                ZREF                                        , &
                IDT                                         , &
                XDT                                         , &
                Profil                                      , &
                ProfilPlan%B1                               , &
                Impression_sing                             , &
                UniteListing                                , &
                Erreur                                        &
                                                            )
            if( Erreur%Numero /= 0 ) then
               return
            end if
         end do
      end if
   end if    ! de if ipas<>0

   !--------------------------------
   ! FIN DU CALCUL D'UN PAS DE TEMPS
   !--------------------------------

   ! Post-traitement des variables hydrauliques
   do ibief = 1 , size(Connect%OrigineBief)
      II1 = Connect%OrigineBief(ibief)
      II2 = Connect%FinBief(ibief)
      do I = II1 , II2

         call RHSBP_S           ( &
                B1(I)           , &
                B2(I)           , &
                BS(I)           , &
                P1(I)           , &
                P2(I)           , &
                S1(I)           , &
                S2(I)           , &
                RH1(I)          , &
                RH2(I)          , &
                I               , &
                Z(I)            , &
                ZREF(I)         , &
                IDT             , &
                XDT             , &
                Profil          , &
                ProfilPlan      , &
                UniteListing    , &
                Erreur            &
                                )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         ! CALCUL DE LA REPARTITION LIT MINEUR / LIT MAJEUR
         ! ------------------------------------------------

         call REPAR         ( &
                debitance   , &
                v_moyen     , &
                Beta(I)     , &
                Q1(I)       , &
                Q2(I)       , &
                S1(I)       , &
                S2(I)       , &
                RH1(I)      , &
                RH2(I)      , &
                P1(I)       , &
                P2(I)       , &
                Q(I)        , &
                CF1(I)      , &
                CF2(I)      , &
                ModeleLit   , &
                LoiFrottement,&
                Profil(IDT(I))%Nom, &
                Erreur        &
                            )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         ! CALCUL ET CONTROLE DU NOMBRE DE FROUDE
         !---------------------------------------
         VI = Q(I) / ( S1(I) + S2(I) )
         if( B1(I).GT.EPS6 ) then
            HI = ( S1(I) + S2(I) ) / ( B1(I) + B2(I) )
         else
            HI = EPS3
         end if
         call FROUDE_S        ( &
                 Froude(i)    , &
                 Beta(i)      , &
                 VI           , &
                 HI           , &
                 i            , &
                 Connect      , &
                 X            , &
                 Erreur         &
                                )

         ! CALCUL DE DTLEVY
         ! ----------------
         VI = Q(I) / ( S1(I) + S2(I) )
         if( B1(I).GT.EPS6 ) then
            HI = ( S1(I) + S2(I) ) / ( B1(I) + B2(I) )
         else
            HI = EPS3
         end if

         Celerite = Beta(I) * VI + DSQRT( Beta(I) * ( Beta(I) - 1 ) * VI**2 + GPES * HI )
         if( I < II2 ) then
            dt_lim = ( X(I+1) - X(I) ) / Celerite
            DTLEVY = INT( DMIN1( dt_lim , DTLEVY ) )
         end if

         ! VERIFICATION DE LA COTE EN TOUT POINT
         ! -------------------------------------
         if( ( Z(I) - ZREF(I) ) < 0._DOUBLE ) then
            Erreur%Numero = 600
            Erreur%ft     = err_600
            Erreur%ft_c   = err_600c
            num_bief      = NUM_BIEF_S( Connect , i , Erreur )
            abs_rel       = X(i) - X(Connect%OrigineBief(num_bief))
            call TRAITER_ERREUR( Erreur , num_bief , abs_rel )
            return
         end if

      end do ! Boucle sur les sections de calcul du bief
   end do ! Boucle sur les biefs

   ! Desallocations de tableaux locaux
   !----------------------------------
   deallocate(ASING, stat = retour)
   if( RETOUR /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'ASING' )
      return
   end if

   deallocate(BSING, stat = retour)
   if( RETOUR /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'BSING' )
      return
   end if

   deallocate(CSING, stat = retour)
   if( RETOUR /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'CSING' )
      return
   end if

   deallocate(DSING, stat = retour)
   if( RETOUR /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'DSING' )
      return
   end if

   deallocate(QSING, stat = retour)
   if( RETOUR /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'QSING' )
      return
   end if

    deallocate(Aliai, stat = retour)
   if( RETOUR /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'ALIAI' )
      return
   end if

   deallocate(Bliai, stat = retour)
   if( RETOUR /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'BLIAI' )
      return
   end if

   deallocate(Cliai, stat = retour)
   if( RETOUR /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'CLIAI' )
      return
   end if

   deallocate(Dliai, stat = retour)
   if( RETOUR /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'DLIAI' )
      return
   end if

   deallocate(Qliai, stat = retour)
   if( RETOUR /= 0 ) then
      Erreur%Numero = 6
      Erreur%ft     = err_6
      Erreur%ft_c   = err_6c
      call TRAITER_ERREUR( Erreur , 'QLIAI' )
      return
   end if


   ! Fin des traitements
   ! -------------------

   ! Erreur%arbredappel = arbredappel_old

   return

end subroutine REZO
