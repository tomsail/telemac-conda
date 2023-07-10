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

subroutine PERMAT ( &
     Z            , & ! Cotes de la surface libre
     Q            , & ! Debit
     ZINIT        , & ! Cote initiale
     X            , & ! Maillage
     ZREF         , & ! Cote de reference
     CF1          , & ! Coeff de frottement mineur
     CF2          , & ! Coeff de frottement majeur
     PCSing       , & ! Pertes de charge singuliere
     IDT          , & ! Positionnnement des sections/profils
     XDT          , & ! idem
     Profil       , & ! Profils geometriques
     ProfilPlan   , & ! Tableaux de planimetrage
     F1           , & ! Fonction impulsion
     Connect      , & ! Table de connectivite
     NumBief      , & ! Numero du bief
     Nbsect       , & ! Nombre de sections
     Singularite  , & ! Singularites
     ModeleLit    , & ! Modele du lit Debord/Fond/Berge
     Impression   , & ! Flag d'impression
     UniteListing , & ! Unite logique du fichier listing
     Temps        , & ! Temps
     LoiFrottement, & ! Loi de frottement
     CQMV         , & ! apport de debit dans la quantite de mvt
     decentrement , & ! option decentrement
     Erreur         ) !/ERREUR/)

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P.CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!   FONCTION :
!   --------
!
!   CALCUL DE LIGNE D'EAU DANS UN BIEF EN REGIME PERMANENT
!   MODELISATION UNIDIMENSIONNELLE
!   LITS MINEUR ET MAJEUR - HYPOTHESES DEBORD OU PRADO
!   SINGULARITES
!
! ----------------------------------------------------------------------
!
! VARIABLES LOCALES
! ._________________.____.____.______________________________________________
! !  JS             ! R  ! -- ! ) PERTES DE CHARGE
! !  JAV            ! R  ! -- ! )      "
! !  JAVAM          ! R  ! -- ! )      "
! !  JAVC           ! R  ! -- ! )      "
! !  ZAM1,ZAM2      ! R  ! -- ! ITERES DE LA COTE AMONT DU BIEF DE CALCUL
! !  ZAV            ! R  ! -- ! COTE AVAL DU BIEF DE CALCUL
! !  ZCRIT          ! R  ! -- ! COTE CRITIQUE AU POINT AMONT DU BIEF DE CALCUL
! !  FRAV           ! R  ! -- ! VALEUR DE FROUDE
! !  FRAM           ! R  ! -- !         "
! !  BETAAM         ! R  ! -- ! REPARTITION DU DEBIT LIT MINEUR - LIT MAJEUR
! !  BETAAV         ! R  ! -- !         "
! !  BETAC          ! R  ! -- !         "
! !  DEBAM          ! R  ! -- ! DEBITANCE
! !  DEBAV          ! R  ! -- !         "
! !  DEBC           ! R  ! -- !         "
! !                 !    !    !
! !  CPCS           ! R  ! -- ! COEFFICIENT DE PERTE DE CHARGE SINGULIERE
! !                 !    !    ! POUR LES ELARGISSEMENTS
! !  FROUD1         ! R  ! -- ! VALEUR TEST DU FROUDE POUR LE PASSAGE EN C.
! !  FROUD2         ! R  ! -- ! VALEUR TEST DU FROUDE POUR LE PASSAGE EN T.
! !  DFROUD         ! R  ! -- ! VALEUR TEST DE VAR. MAX. DU FROUDE
! !  ITMAX1         ! I  ! -- ! NOMBRE MAXIMUM D'ITERATIONS POUR LE CALCUL DE
! !                 !    !    ! LA LIGNE D'EAU
! !_________________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
! ----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :
!   -----------------------
!
!   UniteListing   : IMPRESSION DES RESULTATS GLOBAUX
!
!   SOUS PROGRAMMES APPELANTS :  PERSAR
!   ---------------------------
!   SOUS PROGRAMMES APPELES :
!   -------------------------
!
!   CRITIQ : CALCUL DE LA COTE CRITIQUE
!   FROUDE : CALCUL DU NOMBRE DE FROUDE
!   PSING  : TRAITEMENT DES SINGULARITES EN PERMANENT
!   REPAR  : CALCUL DE LA REPARTITION DES DEBITS ENTRE LE LIT MINEUR
!            ET LE LIT MAJEUR ACTIF, HYPOTHESES 'DEBORD'
!   RHSBP_S: CALCUL DES PARAMETRES DE LA GEOMETRIE D'UNE SECTION
!
!   COMMENTAIRES :
!   ------------
!
!   LE TERME REPRESENTANT LES PERTES DE CHARGE REGULIERES EST PRIS
!   SELON LA MOYENNE HARMONIQUE DES
!   COEFFICIENTS CALCULES DANS CHACUNE DES SECTIONS AMONT ET AVAL:
!                  2      1     1
!                ----- = --- + ---
!                JAVAM   JAV   JAM
!
!   LES PERTES DE CHARGE SINGULIERES SONT :
!      - RALENTISSEMENT : JS=CPCS*(BETAAM*VAM-BETAAV*VAV)**2/2./G
!      - OBSTACLE  EN A : JS=PCSing(A)*BETAAM*VAM**2/2./G
!
!   LE CALCUL EST SUPPOSE SE FAIRE EN REGIME FLUVIAL.
!   TOUTEFOIS SI AU COURS DES ITERATIONS ON DETECTE UN REGIME FLUVIAL
!   A L'AVAL ET TORRENTIEL A L'AMONT , ON IMPOSE ALORS Z=ZCRITIQUE A
!   LA SECTION AMONT , CECI TANT QU'ON N'A PAS RETROUVE UN ECOULEMENT
!   FLUVIAL. DE PLUS UN TEST EST REALISE POUR VERIFIER QUE CE PASSAGE
!   FLUVIAL AMONT - CRITIQUE AVAL N'EST PAS TROP BRUTAL
!
! ----------------------------------------------------------------------

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   ! Constantes nommees
   use M_PARAMETRE_C       ! GPES
   use M_MESSAGE_C         ! Messages d'erreur
   ! Types derives
   use M_PROFIL_T          ! Type PROFIL_T
   use M_PROFIL_PLAN_T     ! Type PROFIL_PLAN_T
   use M_SINGULARITE_T     ! Type SINGULARITE_T
   use M_ERREUR_T          ! Type EREUR_T
   use M_CONNECT_T         ! Type CONNECT_T
   ! Procedures-module
   use M_RHSBP_S           ! Sous-programme RHSBP_S
   use M_RHSB1_S          ! Sous programme RHSBP1
   use M_NUM_BIEF_S        ! Numero de bief d'une section
   use M_FROUDE_S          ! Calcul du nombre de Froude
   use M_TRAITER_ERREUR_I  ! Interface generique de traitement des erreurs
   ! Interfaces
   use M_REPAR_I
   use M_PSING_I
   use M_CRITIQ_I
   !.. Declarations explicites ..
   !-----------------------------
   implicit none
   !.. Arguments ..
   !---------------
   ! TABLEAU  DIMENSIONNE  A NbSect
   real(DOUBLE)       , dimension(:)  , intent(out)   :: Z
   ! TABLEAU  DIMENSIONNE  A NbSect
   real(DOUBLE)       , dimension(:)  , intent(in)    :: Q
   real(DOUBLE)       ,                 intent(in)    :: ZINIT
   ! TABLEAUX DIMENSIONNES A NMSCAL
   real(DOUBLE)       , dimension(:)  , intent(in)    :: X
   real(DOUBLE)       , dimension(:)  , intent(in)    :: ZREF
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF1
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF2
   real(DOUBLE)       , dimension(:)  , intent(in)    :: PCSing
   integer            , dimension(:)  , intent(in)    :: IDT
   real(DOUBLE)       , dimension(:)  , intent(in)    :: XDT
   type(PROFIL_T)     , dimension(:)  , intent(in)    :: Profil
   type(PROFIL_PLAN_T),                 intent(in)    :: ProfilPlan
   real(double)       , dimension(:,:), intent(in)    :: F1
   integer            ,                 intent(in)    :: NumBief
   type(CONNECT_T)    ,                 intent(in)    :: Connect
   type(SINGULARITE_T), dimension(:)  , intent(in)    :: Singularite
   integer            ,                 intent(in)    :: ModeleLit
   logical                            , intent(in)    :: Impression
   integer            ,                 intent(in)    :: UniteListing
   real(DOUBLE)       ,                 intent(in)    :: Temps
   integer            ,                 intent(in)    :: LoiFrottement
   integer            ,                 intent(in)    :: Nbsect
   integer             ,                intent(in)    :: CQMV
   logical            ,                 intent(in)    :: decentrement
   type(ERREUR_T)     ,                 intent(inout) :: Erreur
   !.. Constantes ..
   !----------------
   real(DOUBLE), parameter :: CPCS   = 0.3_DOUBLE
!   real(DOUBLE), parameter :: CQMV   = 0.0_DOUBLE
   real(DOUBLE), parameter :: FROUD1 = 0.95_DOUBLE
   real(DOUBLE), parameter :: FROUD2 = 1.05_DOUBLE
   real(DOUBLE), parameter :: DFROUD = 0.5_DOUBLE
   integer     , parameter :: ITMAX1 = 100
   !.. Variables locales ..
   !-----------------------
   integer :: IDEBTOR(Nbsect),Kpass(Nbsect)
   real (DOUBLE) :: ZC(Nbsect),Fimp(Nbsect)
   integer               :: Izone,nb_zone
   real(DOUBLE) :: JAV , JS , JAVAM , JAVC,JAM
   real(DOUBLE) :: ZAM1, ZAM2, ZAV ,ZAM,YP2,ZAM3,ZAM22,ZAM21,ZMIL
   real(DOUBLE) :: DX, DQ,YPMIL,YP22,YP21,DZ
   real(DOUBLE) :: CQMVJ
   real(DOUBLE) :: Y
   real(DOUBLE) :: DEBAV
   real(DOUBLE) :: HAV
   real(DOUBLE) :: BETAAV
   real(DOUBLE) :: VAV
   real(DOUBLE) :: VAV2
   real(DOUBLE) :: H
   real(DOUBLE) :: SAV, SM1, SM2
   real(DOUBLE) :: B1 , B2
   real(DOUBLE) :: BSTOCK
   real(DOUBLE) :: SS1 , SS2
   real(DOUBLE) :: RH1  , RH2
   real(DOUBLE) :: FRAV , FRAM
   real(DOUBLE) :: EPSIL
   real(DOUBLE) :: SC
   real(DOUBLE) :: DEBC
   real(DOUBLE) :: HC
   real(DOUBLE) :: ZCRIT
   real(DOUBLE) :: BETAC
   real(DOUBLE) :: VC
   real(DOUBLE) :: VC2
   real(DOUBLE) :: DXBETA
   real(DOUBLE) :: DXQ
   real(DOUBLE) :: HAM
   real(DOUBLE) :: SAM
   real(DOUBLE) :: DEBAM
   real(DOUBLE) :: BETAAM
   real(DOUBLE) :: VAM
   real(DOUBLE) :: VAM2
   real(DOUBLE) :: DXV
   real(DOUBLE) :: XP   , YP
   real(DOUBLE) :: XQ   , YQ
   real(DOUBLE) :: YMIN , YMAX
   real(DOUBLE) :: XMIN , XMAX
   real(DOUBLE) :: XM   , YM
   real(DOUBLE) :: Q1 , Q2
   real(DOUBLE) :: P1   , P2
   integer        :: J, IECRIT, ITER1
   integer        :: debut_bief
   integer        :: fin_bief
   integer        :: ising             ! Compteur sur les singularites
   integer        :: num_bief
   real(DOUBLE)   :: absc_rel
   character(132) :: arbredappel_old

   !============================= Instructions ===========================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero      = 0
   arbredappel_old    = trim(Erreur%arbredappel)
   Erreur%Arbredappel = trim(Erreur%arbredappel) // '=>PERMAT'

   IECRIT     = 0
   debut_bief = Connect%OrigineBief(NumBief)
   fin_bief   = Connect%FinBief(NumBief)

   !
   !  Prise compte des apports de debit dans la qte de mvt
   !
   if( CQMV.EQ.0 ) then
      CQMVJ = 0.D0
   else
      CQMVJ = 1.D0
   endif

   !
   ! CONDITION INITIALE A L'AVAL
   ! ------------ Balayage d'AMONT EN AVAL DES ZONES FLUVIALES ---------------
   !
   J     = fin_bief
   izone = 0
   Z(J)  = ZINIT
   ZAM1  = ZINIT
   !
   ! INITIALISATION DE LA VALEUR CRITIQUE AU POINT J
   ! C. Coulet (Artelia) : Lors du calcul entre J-1 et J, 
   !    on a potentiellement un appel à ZC(J) qui peut ne pas être calculé
   ! ------------------------------------
   call CRITIQ (      &
       ZCRIT        , & ! /RESULTATS/
       J            , & ! /DONNEES NON MODIFIEES/
       ZREF         , &
       Q(J)         , & ! /DONNEES NON MODIFIEES
       CF1(J)       , & !  (ARGUMENTS DE S.P APPELES)/
       CF2(J)       , &
       IDT          , &
       XDT          , &
       Profil       , &
       ProfilPlan   , &
       ModeleLit    , &
       LoiFrottement, &
       UniteListing , &
       Erreur         & ! Erreur
       )
   ZC(J) = ZCRIT
   !
   ! 1ER  TEST SUR LA CONDITION AVAL DU BIEF :
   ! COHERENCE AVEC LA COTE DES FONDS A L'AVAL
   ! -----------------------------------------
   if( Z(J) < ZREF(J) )then
      Erreur%Numero = 37
      Erreur%ft     = err_37
      Erreur%ft_c   = err_37c
      call TRAITER_ERREUR( Erreur , Temps , NumBief , Z(J) , ZREF(J) )
      return
   endif

   ! ELEMENT DE LA PREMIERE SECTION DE CALCUL AVAL = AV
   ! --------------------------------------------------
   100 ITER1 = -1

   ZAV   = ZAM1
   DX    = X(J)-X(J-1)
   DQ    = Q(J)-Q(J-1)
!   CQMVJ = CQMV

   call RHSBP_S (                                           &
       B1 , B2 , BSTOCK , P1 , P2 , SM1 , SM2 , RH1 , RH2 , &
       J , ZAV , ZREF(J) , IDT , XDT ,                      &
       Profil , ProfilPlan ,                                &
       UniteListing, Erreur                                 &
        )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   Y = ZAV - ZREF(J)
   if( Y < EPS6 ) then
      Erreur%Numero = 31
      Erreur%ft     = err_31
      Erreur%ft_c   = err_31c
      num_bief      = NUM_BIEF_S( Connect , J , Erreur )
      absc_rel      = X(J) - X(Connect%OrigineBief(num_bief))
      call TRAITER_ERREUR( Erreur , Temps , J , num_bief , absc_rel , ZAV , ZREF(J) )
      return
   endif

   call REPAR (      &
       DEBAV       , & ! Resultats
       VAV         , &
       BETAAV      , &
       Q1          , &
       Q2          , &
       SM1         , & ! Donnees modifiees
       SM2         , &
       RH1         , &
       RH2         , &
       P1          , & ! Donnees non modifiees
       P2          , &
       Q(J)        , &
       CF1(J)      , &
       CF2(J)      , &
       ModeleLit    , &
       LoiFrottement, &
       Profil(IDT(J))%Nom, &
       Erreur        & ! Erreur
       )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   SAV  = SM1 + SM2
   JAV  = ( Q(J) / DEBAV )**2
   JAV  = sign( JAV , Q(J) )
   VAV2 = VAV**2
   HAV  = ZAV + 0.5_DOUBLE * BETAAV * VAV2 / GPES
   H    = ( SM1 + SM2 ) / ( B1 + B2 )

   call FROUDE_S (    &
       FRAV         , &
       BETAAV       , &
       VAV          , &
       H            , &
       J            , &
       Connect      , &
       X            , &
       Erreur         &
       )

   if( Erreur%Numero /= 0 ) then
      return
   endif

   ! PASSAGE A LA SECTION AMONT = AM
   ! -------------------------------
   J = J - 1

   EPSIL = EPS3 * Profil(IDT(J))%Pas

   ! TEST DE PRESENCE D'UNE SINGULARITE
   ! ----------------------------------
   if( size(Singularite) /= 0 ) then

      do ising = 1 , size(Singularite)

         if( Singularite(ising)%Section == J ) then

            call PSING           ( &
             ZAM2                , & !/RESULTATS/
             Singularite(ising)  , & !/DONNEES NON MODIFIEES/
             ZREF(J)             , &
             ZAV                 , &
             Q(J)                , &
             Profil              , &
             ProfilPlan%B1       , &
             IDT                 , &
             XDT                 , &
             J                   , &
             Temps               , &
             Erreur                & !/Erreur/
             )

            if( Erreur%Numero /= 0 ) then
               return
            endif

            goto 300

         endif

      end do

   endif

   ! TEST DU PASSAGE EN REGIME TORRENTIEL
   ! ------------------------------------

   call CRITIQ (      &
       ZCRIT        , & ! /RESULTATS/
       J            , & ! /DONNEES NON MODIFIEES/
       ZREF         , &
       Q(J)         , & ! /DONNEES NON MODIFIEES
       CF1(J)       , & !  (ARGUMENTS DE S.P APPELES)/
       CF2(J)       , &
       IDT          , &
       XDT          , &
       Profil       , &
       ProfilPlan   , &
       ModeleLit    , &
       LoiFrottement, &
       UniteListing , &
       Erreur         & ! Erreur
       )

   ZC(J) = ZCRIT

   if( Erreur%Numero /= 0 ) then
      return
   endif

   call RHSBP_S (                                           &
       B1 , B2 , BSTOCK , P1 , P2 , SS1 , SS2 , RH1 , RH2 , &
       J , ZCRIT , ZREF(J) , IDT , XDT ,                    &
       Profil , ProfilPlan ,                                &
       UniteListing, Erreur                                 &
       )

   if( Erreur%Numero /= 0 ) then
      return
   endif

   Y = ZCRIT - ZREF(J)

   if( Y < EPS6 ) then
      Erreur%Numero = 35
      Erreur%ft     = err_35
      Erreur%ft_c   = err_35c
      num_bief      = NUM_BIEF_S( Connect , J , Erreur )
      absc_rel      = X(J) - X(Connect%OrigineBief(num_bief))
      call TRAITER_ERREUR( Erreur , Temps , J , num_bief , absc_rel , ZCRIT , ZREF(J) )
      return
   endif

   call REPAR (      &
       DEBC        , & ! Resultats
       VC          , &
       BETAC       , &
       Q1          , &
       Q2          , &
       SS1         , & ! Donnees modifiees
       SS2         , &
       RH1         , &
       RH2         , &
       P1          , & ! Donnees non modifiees
       P2          , &
       Q(J)        , &
       CF1(J)      , &
       CF2(J)      , &
       ModeleLit    , &
       LoiFrottement, &
       Profil(IDT(J))%Nom, &
       Erreur        &
       )

   if( Erreur%Numero /= 0 ) then
      return
   endif

   SC     = SS1 + SS2
   if (decentrement) then
     JAVC   = Q(J)**2 / (DEBC**2 )
   else
     JAVC   = Q(J)**2 / ( 0.5_DOUBLE * ( DEBAV**2 + DEBC**2 ) )
   endif
   JAVC   = sign(JAVC, Q(J))
   VC2    = VC**2
   HC     = ZCRIT + 0.5_DOUBLE * BETAC * VC2 / GPES
   DXBETA = 0.25_DOUBLE * ( BETAAV - BETAC ) * ( VAV2 + VC2 ) / GPES
   DXQ    = 0.5_DOUBLE * DQ / GPES * ( ( BETAAV - CQMVJ ) * VAV / SAV + ( BETAC - CQMVJ ) * VC / SC )

   !----------------------------------------------------
   ! PERTE DE CHARGE SINGULIERE LIEE A UN RALENTISSEMENT
   !----------------------------------------------------
   if( BETAC * VC > BETAAV * VAV .and. PCSing(J+1) < EPS2 ) then
      JS = 0.5_DOUBLE * CPCS * ( BETAC * VC - BETAAV * VAV )**2 / GPES
   else
      JS = 0._DOUBLE
   endif

   !-----------------------------------------------
   ! PERTE DE CHARGE SINGULIERE LIEE A UN EVENEMENT
   !-----------------------------------------------
   if( VC > 0._DOUBLE ) then
      JS = JS + 0.5_DOUBLE * PCSing(J+1) * BETAC  * VC2  / GPES
   else
      JS = JS - 0.5_DOUBLE * PCSing(J+1) * BETAAV * VAV2 / GPES
   endif

   HAM = HAV + JAVC * DX + JS + DXBETA + DXQ

   label_HC : if( HC >= HAM ) then
      if( izone == 0 ) then
         izone          = 1
         Kpass(izone)   = 1
         IDEBTOR(izone) = J
      else
         if( J.LT.( - 1 + Idebtor(izone) ) ) then
            izone           = izone + 1
            IDEBTOR (izone) = J
            Kpass(izone)    = 1
         else
            Kpass(izone)    = Kpass (izone) + 1
            IDEBTOR(Izone)  = J
         endif
      endif

      ZC(J) = ZCRIT

      !
      ! DETECTION D'UN RESSAUT HYDRAULIQUE OU D'UNE CHUTE
      !
      ZAM2 = ZCRIT

      if( Impression .and. IECRIT == 0 ) then
         write(UniteListing ,2000) debut_bief , Q(debut_bief) , fin_bief , Z(fin_bief)
      endif

      IECRIT = 1

      if( Impression ) then
         num_bief = NUM_BIEF_S( Connect , J , Erreur )
         absc_rel = X(j) - X(Connect%OrigineBief(num_bief))
         write (UniteListing ,2030) J , num_bief , absc_rel , ZAM2
      endif

   else label_HC

      ! COTE CHOISIE EN AMONT A PRIORI : ZAM1

      ZAM1 = ZAV + JAV * DX

      ! DEMARRAGE DE L'ALGORITHME ITERATIF VISANT A OBTENIR ZAM2 = ZAM1
      ! ---------------------------------------------------------------

      200  ITER1 = ITER1 + 1

      !-------------------------------------------------------
      ! Calcul des grandeurs hydrauliques correspondant a ZAM1
      !-------------------------------------------------------
      call RHSBP_S (                                           &
          B1 , B2 , BSTOCK , P1 , P2 , SS1 , SS2 , RH1 , RH2 , &
          J , ZAM1 , ZREF(J) , IDT , XDT ,                     &
          Profil , ProfilPlan ,                                &
          UniteListing, Erreur                                 &
          )

      if( Erreur%Numero /= 0 ) then
         return
      endif

      !----------------------------
      ! Test de hauteur d'eau nulle
      !----------------------------
      Y = ZAM1 - ZREF(J)

      if( Y < EPS6 ) then
         Erreur%Numero = 31
         Erreur%ft     = err_31
         Erreur%ft_c   = err_31c
         num_bief      = NUM_BIEF_S( Connect , J , Erreur )
         absc_rel      = X(J) - X(Connect%OrigineBief(num_bief))
         call TRAITER_ERREUR( Erreur , Temps , J , num_bief , absc_rel , ZAV , ZREF(J) )
         return
      endif

      !---------------------------------
      ! Calcul de la debitance pour ZAM1
      !---------------------------------
      call REPAR (      &
          DEBAM       , & ! Resultats
          VAM         , &
          BETAAM      , &
          Q1          , &
          Q2          , &
          SS1         , & ! Donnees modifiees
          SS2         , &
          RH1         , &
          RH2         , &
          P1          , & ! Donnees non modifiees
          P2          , &
          Q(J)        , &
          CF1(J)      , &
          CF2(J)      , &
          ModeleLit    , &
          LoiFrottement, &
          Profil(IDT(J))%Nom, &
          Erreur        &
          )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      SAM   = SS1 + SS2
      if (decentrement) then
        JAVAM = sign(Q(J)**2, Q(J)) / ( DEBAM**2 )
      else
        JAVAM = sign(Q(J)**2, Q(J)) / ( 0.5_DOUBLE * ( DEBAV**2 + DEBAM**2 ) )
      endif
      VAM2  = VAM**2

      !---------------------
      ! termes de l'equation
      !---------------------
      DXBETA = 0.25_DOUBLE * ( BETAAV - BETAAM ) * ( VAV2 + VAM2 ) / GPES
      DXQ    = 0.5_DOUBLE * DQ / GPES * ( ( BETAAV - CQMVJ ) * VAV / SAV + ( BETAAM - CQMVJ ) * VAM / SAM )
      DXV    = 0.5_DOUBLE * ( BETAAV * VAV2 - BETAAM * VAM2 ) / GPES

      !----------------------------------------------------
      ! PERTE DE CHARGE SINGULIERE LIEE A UN RALENTISSEMENT
      !----------------------------------------------------
      if( BETAAM*VAM > BETAAV*VAV .and. PCSing(J+1) < EPS2 ) then
         JS = 0.5_DOUBLE * CPCS * ( BETAAM * VAM - BETAAV * VAV )**2 / GPES
      else
         JS = 0._DOUBLE
      endif

      !-----------------------------------------------
      ! PERTE DE CHARGE SINGULIERE LIEE A UN EVENEMENT
      !-----------------------------------------------
      if( VAM > 0._DOUBLE ) then
         JS = JS + 0.5_DOUBLE * PCSing(J+1) * BETAAM * VAM2 / GPES
      else
         JS = JS - 0.5_DOUBLE * PCSing(J+1) * BETAAV * VAV2 / GPES
      endif

      H = ( SS1 + SS2 ) / ( B1 + B2 )

      call FROUDE_S (    &
          FRAM         , &
          BETAAM       , &
          VAM          , &
          H            , &
          J            , &
          Connect      , &
          X            , &
          Erreur         &
          )

      if( Erreur%Numero /= 0 ) then
         return
      endif

      !------------------------------------------
      ! COTE OBTENUE EN AMONT A POSTERIORI : ZAM2
      !------------------------------------------
      ZAM2 = ZAV + JAVAM * DX + JS + DXBETA + DXQ + DXV

      !----------------------------------
      ! PASSAGE EN TORRENTIEL NON DETECTE
      !----------------------------------
      if( FRAM >= 1.D0 ) then
         if( Impression .and. IECRIT == 0 ) then
            write (UniteListing ,2000) debut_bief,Q(debut_bief),fin_bief,Z(fin_bief)
         endif
         if( izone == 0 ) then
            izone          = 1
            Kpass(izone)   = 1
            IDEBTOR(izone) = J
         else
            if( J.LT.(- 1 + Idebtor(izone)) ) then
               izone           = izone + 1
               IDEBTOR (izone) = J
               Kpass(izone)    = 1
            else
               Kpass(izone)   = Kpass (izone) + 1
               IDEBTOR(Izone) = J
            endif
         endif

         ZC(J) = ZAM2

         !
         ! DETECTION D'UN RESSAUT HYDRAULIQUE OU D'UNE CHUTE
         !

         IECRIT = 1

         goto 300

      endif

      ! CHOIX DES POINTS P,Q A LA BASE DE L'INTERPOLATION
      label_ITER1 : if( ITER1 == 0 ) then

         XP = ZAM1
         YP = ZAM2 - ZAM1
         XQ = ZAM2

         if( dabs(YP) <= EPSIL ) then
            goto 300
         else
            ZAM1=ZAM2
            goto 200
         endif

      else if (ITER1 == 1) then label_ITER1

         YQ = ZAM2 - ZAM1

         if( dabs(YQ) <= EPSIL ) then
            goto 300
         endif

         ! PASSAGE DES POINTS (P,Q) AUX POINTS (MIN,MAX) SELON LEUR POSITION
         YMIN = DMIN1(YP,YQ)
         YMAX = DMAX1(YP,YQ)

         if( abs(YMAX-YP).LT.EPS6 ) then
            XMIN = XQ
            XMAX = XP
         else
            XMIN = XP
            XMAX = XQ
         endif

      else label_ITER1

         YM = ZAM2 - ZAM1

         label_YM : if( dabs(YM) <= EPSIL ) then

            goto 300

         else label_YM

            if( YMIN * YMAX <= 0._DOUBLE ) then

               if( YM <= 0._DOUBLE ) then
                  XMIN = XM
                  YMIN = YM
               else
                  XMAX = XM
                  YMAX = YM
               endif

            else

               if( YMAX < 0._DOUBLE ) then
                  XMIN = XMAX
                  YMIN = YMAX
                  XMAX = XM
                  YMAX = YM
               else
                  XMAX = XMIN
                  YMAX = YMIN
                  XMIN = XM
                  YMIN = YM
               endif

            endif

         endif label_YM

      endif label_ITER1

      ! INTERPOLATION DE LAGRANGE : NOUVEAU POINT DE COORDONNEE XM

      if( YMIN * YMAX > 0._DOUBLE ) then

         if( dabs( YMAX - YMIN ) <= EPS6 ) goto 300

            XM = XMIN - ( XMAX - XMIN ) * ( YMIN / ( YMAX - YMIN ) )
            XM = DMAX1( XM , ZREF(J) )

            if( YMIN <= 0._DOUBLE ) then
               XM = 0.5_DOUBLE * ( XM + XMAX )
            else
               XM = 0.5_DOUBLE * ( XM + XMIN )
            endif

      else

         XM = ( XMIN + XMAX ) / 2._DOUBLE

      endif

      !------------------------
      ! TEST DE NON CONVERGENCE
      !------------------------
      if( ITER1 >= ITMAX1 ) then
         Erreur%Numero = 38
         Erreur%ft     = err_38
         Erreur%ft_c   = err_38c
         call TRAITER_ERREUR( Erreur , Temps , NumBief , J )
         return
      endif

      ! FIN DE L'ALGORITHME ITERATIF

      ZAM1 = XM
      goto 200

   endif label_HC

   !
   ! RESULTAT DEFINITIF POUR LA SECTION AMONT
   ! ----------------------------------------

   300 Z(J) = ZAM2

   ! TESTS : PERTE DE CHARGE SINGULIERE OU PASSAGE FLUVIAL AMONT
   ! - CRITIQUE AVAL TROP BRUTAL
   ! -----------------------------------------------------------
   if( dabs(JS) > EPS2 ) then
      if( Impression .and. IECRIT == 0 ) then
         write (UniteListing ,2000) debut_bief,Q(debut_bief),fin_bief,Z(fin_bief)
      endif

      IECRIT=1

      if( Impression ) then
         num_bief = NUM_BIEF_S (Connect, J+1, Erreur)
         absc_rel = X(j) - X(Connect%OrigineBief(num_bief))
         write(UniteListing,2080) J+1 , num_bief , absc_rel , JS
      endif

   endif

   if( ( FRAV - FRAM ) > DFROUD ) then
      if( Impression .and. IECRIT == 0 ) then
         write(UniteListing ,2000) debut_bief , Q(debut_bief) , fin_bief , Z(fin_bief)
      endif

      IECRIT = 1

      if(Impression) then
         num_bief = NUM_BIEF_S (Connect, J, Erreur)
         absc_rel = X(j) - X(Connect%OrigineBief(num_bief))
         write (UniteListing ,2090) J, num_bief, absc_rel
      endif

   endif

   !------------------------------
   ! SORTIE DE LA BOUCLE DE CALCUL
   !------------------------------
   ZAM1 = ZAM2

   if( J > debut_bief ) then
      goto 100
   else
      nb_zone = izone

      !-------------------------------------
      !  BOUCLE SUR LES ZONES TORRENTIELLES
      !  BALAYAGE AMONT -AVAL
      !-------------------------------------

      DO  izone = 1 , NB_ZONE
         J = IDEBTOR(IZONE) - 1
         If( J==0 ) then
            J = IDEBTOR(IZONE)
         endif

         101  ZAM = Z(J)
         ZAM2     = ZREF(J+1)
         ITER1    = -1

         ZAM1  = ZAM
         DX    = X(J+1)-X(J)
         DQ    = Q(J+1)-Q(J)
  !       CQMVJ = CQMV

         call RHSBP_S (                                               &
                 B1 , B2 , BSTOCK , P1 , P2 , SM1 , SM2 , RH1 , RH2 , &
                 J , ZAM1 , ZREF(J) , IDT , XDT ,                     &
                 Profil , ProfilPlan ,                                &
                 UniteListing, Erreur                                 &
         )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         call REPAR (       &
              DEBAM       , & ! Resultats
              VAM         , &
              BETAAM      , &
              Q1          , &
              Q2          , &
              SM1         , & ! Donnees modifiees
              SM2         , &
              RH1         , &
              RH2         , &
              P1          , & ! Donnees non modifiees
              P2          , &
              Q(J)        , &
              CF1(J)      , &
              CF2(J)      , &
              ModeleLit    , &
              LoiFrottement, &
              Profil(IDT(J))%Nom, &
              Erreur        & ! Erreur
              )

         if( Erreur%Numero /= 0 ) then
            return
         endif

         SAM  = SM1 + SM2
         JAM  = ( Q(J) / DEBAM )**2
         JAM  = sign( JAM , Q(J) )
         VAM2 = VAM**2

         !
         ! DICHOTOMIE  : Z est compris entre ZRZF et ZC avec les cas analytiques la dichotomoie commence a 0.001 + convergence a 0.001
         ITER1 = 0
         DZ = 0.001001_double
         IF( ITER1 == 0 ) then
            YP2   = 0._DOUBLE
            Zam21 = ZREF(J+1) + DZ
            ZAM22 = ZC(J+1)
         endif

         !
         ! Initialisation des valeurs de la fonctions aux bornes
         !
         ZAM2 = ZAM21
         call RHSBP_S (                                         &
           B1 , B2 , BSTOCK , P1 , P2 , SM1 , SM2 , RH1 , RH2 , &
           J+1 , ZAM2 , ZREF(J+1) , IDT , XDT ,                 &
           Profil , ProfilPlan ,                                &
           UniteListing, Erreur                                )

         call REPAR (       &
              DEBAV       , & ! Resultats
              VAV         , &
              BETAAV      , &
              Q1          , &
              Q2          , &
              SM1         , & ! Donnees modifiees
              SM2         , &
              RH1         , &
              RH2         , &
              P1          , & ! Donnees non modifiees
              P2          , &
              Q(J+1)      , &
              CF1(J+1)    , &
              CF2(J+1)    , &
              ModeleLit   , &
              LoiFrottement, &
              Profil(IDT(J+1))%Nom, &
              Erreur        & ! Erreur
              )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         SAV   = SM1 + SM2
         JAV   = ( Q(J+1) / DEBAV )**2
         JAV   = sign( JAV , Q(J+1) )
         VAV2  = VAV**2
         JAVAM = sign( Q(J+1)**2 , Q(J+1) ) / ( 0.5_DOUBLE * ( DEBAV**2 + DEBAM**2 ) )

         !---------------------
         ! termes de l'equation
         !---------------------
         DXBETA = 0.25_DOUBLE * ( BETAAV - BETAAM ) * ( VAV2 + VAM2 ) / GPES
         DXQ    = 0.5_DOUBLE * DQ / GPES * ( ( BETAAV - CQMVJ ) * VAV / SAV + ( BETAAM - CQMVJ ) * VAM / SAM )
         DXV    = 0.5_DOUBLE * ( BETAAV * VAV2 - BETAAM * VAM2 ) / GPES

         H = ( SAV ) / ( B1 + B2 )

         call FROUDE_S (       &
                FRAM         , &
                BETAAM       , &
                VAM          , &
                H            , &
                J            , &
                Connect      , &
                X            , &
                Erreur         &
                )

         if( Erreur%Numero /= 0 ) then
            return
         endif

         !---------------------
         ! COTE OBTENUE  F (Z)
         !---------------------

         ZAM3 = ZAM1 - JAV * DX - DXBETA - DXQ - DXV
         YP21 = ZAM3 - ZAM2
         ZAM2 = ZAM22

         call RHSBP_S (                                         &
           B1 , B2 , BSTOCK , P1 , P2 , SM1 , SM2 , RH1 , RH2 , &
           J+1 , ZAM2 , ZREF(J+1) , IDT , XDT ,                 &
           Profil , ProfilPlan ,                                &
           UniteListing, Erreur                                 &
             )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         Y = ZAM2 - ZREF(J+1)
         call REPAR (       &
              DEBAV       , & ! Resultats
              VAV         , &
              BETAAV      , &
              Q1          , &
              Q2          , &
              SM1         , & ! Donnees modifiees
              SM2         , &
              RH1         , &
              RH2         , &
              P1          , & ! Donnees non modifiees
              P2          , &
              Q(J+1)      , &
              CF1(J+1)    , &
              CF2(J+1)    , &
              ModeleLit   , &
              LoiFrottement, &
              Profil(IDT(J+1))%Nom, &
              Erreur        & ! Erreur
              )

      if( Erreur%Numero /= 0 ) then
         return
      endif

      SAV   = SM1 + SM2
      JAV   = ( Q(J+1) / DEBAV )**2
      JAV   = sign( JAV , Q(J+1) )
      VAV2  = VAV**2
      JAVAM = sign( Q(J+1)**2 , Q(J+1) ) / ( 0.5_DOUBLE * ( DEBAV**2 + DEBAM**2 ) )

      !---------------------
      ! termes de l'equation
      !---------------------
      DXBETA = 0.25_DOUBLE * ( BETAAV - BETAAM ) * ( VAV2 + VAM2 ) / GPES
      DXQ    = 0.5_DOUBLE * DQ / GPES * ( ( BETAAV - CQMVJ ) * VAV / SAV + ( BETAAM - CQMVJ ) * VAM / SAM )
      DXV    = 0.5_DOUBLE * ( BETAAV * VAV2 - BETAAM * VAM2 ) / GPES

      H = ( SAV ) / ( B1 + B2 )

      call FROUDE_S (    &
          FRAM         , &
          BETAAM       , &
          VAM          , &
          H            , &
          J            , &
          Connect      , &
          X            , &
          Erreur         &
          )

      if( Erreur%Numero /= 0 ) then
         return
      endif

      !---------------------
      ! COTE OBTENUE  F (Z)
      !---------------------
      !
      !  Recherche des zeros de F par dichotomie
      !
      ZAM3 = ZAM1 - JAV * DX  - DXBETA - DXQ - DXV
      YP22 = ZAM3 - ZAM2
      ZMIL  = ( ZAM22 + ZAM21 ) / 2.D0
      !
      !   Calcul de F(Z) pour ces 3 valeurs
      !
      201 ZAM2 = ZMIL

      !
      ! Calcul de la fonction non lineaire a annuler aux pts ZAM21 ZAM22 ZMIL
      ! ---------------------------------------------------------------------
      !
      call RHSBP_S (                                        &
       B1 , B2 , BSTOCK , P1 , P2 , SM1 , SM2 , RH1 , RH2 , &
       J+1 , ZAM2 , ZREF(J+1) , IDT , XDT ,                 &
       Profil , ProfilPlan ,                                &
       UniteListing, Erreur                                 &
        )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      Y = ZAM2 - ZREF(J+1)

      if( Y < EPS6 ) then
         Erreur%Numero = 31
         Erreur%ft     = err_31
         Erreur%ft_c   = err_31c
         num_bief      = NUM_BIEF_S( Connect , J , Erreur )
         absc_rel      = X(J) - X(Connect%OrigineBief(num_bief))
         call TRAITER_ERREUR( Erreur , Temps , J , num_bief , absc_rel , ZAV , ZREF(J) )
         return
      endif

      call REPAR (   &
       DEBAV       , & ! Resultats
       VAV         , &
       BETAAV      , &
       Q1          , &
       Q2          , &
       SM1         , & ! Donnees modifiees
       SM2         , &
       RH1         , &
       RH2         , &
       P1          , & ! Donnees non modifiees
       P2          , &
       Q(J+1)      , &
       CF1(J+1)    , &
       CF2(J+1)    , &
       ModeleLit   , &
       LoiFrottement, &
       Profil(IDT(J+1))%Nom, &
       Erreur        & ! Erreur
       )

      if( Erreur%Numero /= 0 ) then
         return
      endif

      SAV   = SM1 + SM2
      JAV   = ( Q(J+1) / DEBAV )**2
      JAV   = sign( JAV , Q(J+1) )
      VAV2  = VAV**2
      JAVAM = sign( Q(J+1)**2 , Q(J+1) ) / ( 0.5_DOUBLE * ( DEBAV**2 + DEBAM**2 ) )

      !---------------------
      ! termes de l'equation
      !---------------------
      DXBETA = 0.25_DOUBLE * ( BETAAV - BETAAM ) * ( VAV2 + VAM2 ) / GPES
      DXQ    = 0.5_DOUBLE * DQ / GPES * ( ( BETAAV - CQMVJ ) * VAV / SAV + ( BETAAM - CQMVJ ) * VAM / SAM )
      DXV    = 0.5_DOUBLE * ( BETAAV * VAV2 - BETAAM * VAM2 ) / GPES

      !----------------------------------------------------
      ! PERTE DE CHARGE SINGULIERE LIEE A UN RALENTISSEMENT
      !----------------------------------------------------
      if( BETAAM * VAM > BETAAV * VAV .and. PCSing(J+1) < EPS2 ) then
         JS = 0.5_DOUBLE * CPCS * ( BETAAM * VAM - BETAAV * VAV )**2 / GPES
      else
         JS = 0._DOUBLE
      endif

      !-----------------------------------------------
      ! PERTE DE CHARGE SINGULIERE LIEE A UN EVENEMENT
      !-----------------------------------------------
      if( VAM > 0._DOUBLE ) then
         JS = JS + 0.5_DOUBLE * PCSing(J+1) * BETAAM * VAM2 / GPES
      else
         JS = JS - 0.5_DOUBLE * PCSing(J+1) * BETAAV * VAV2 / GPES
      endif

      H = (SAV) / ( B1 + B2 )

      call FROUDE_S (    &
          FRAM         , &
          BETAAM       , &
          VAM          , &
          H            , &
          J            , &
          Connect      , &
          X            , &
          Erreur         &
          )

      if( Erreur%Numero /= 0 ) then
         return
      endif

      !----------------------
      ! COTE OBTENUE  F (Z)
      !----------------------
      ZAM3  = ZAM1 - JAV * DX - JS - DXBETA - DXQ - DXV
      YPMIL = ZAM3 - ZAM2

      if( ( ZAM22 - ZAM21 ).LE.0.0001_DOUBLE ) GOTO 301

      if( Yp21 * ypmil.LE.0.0_DOUBLE ) then
         ZAM22 = ZMIL
         ZMIL  = (ZAM21+ZMIL) / 2.0_DOUBLE
         YP22  = YPMIL
         GO TO 201
      else
         ZAM21 = ZMIL
         ZMIL  = ( ZAM22 + ZMIL ) / 2.0_DOUBLE
         YP21  = YPMIL
         GO TO 201
      endif

      301 Z(J+1) = ZMIL
      J = J + 1

      IF( J < ( idebtor(izone) + Kpass(izone) ) ) go to 101
         !
         ! Verification de la position du ressaut - fonction impulsion
         !
         if( FRAM.GE.1.1_DOUBLE ) then
            if( (J+1).EQ.(fin_bief) ) then
               Z(J+1) = Z(J)
               go to 102
            endif

            call RHSB1_S (                         &
                 Fimp (J)                        , &
                 J , Z(J) , ZREF(J) , IDT , XDT  , &
                 Profil ,  F1                    , &
                 UniteListing, Erreur              &
                   )

            call RHSB1_S (                               &
                 Fimp (J+1)                            , &
                 J+1, Z(J+1) , ZREF(J+1) , IDT , XDT   , &
                 Profil ,  F1 ,                          &
                 UniteListing, Erreur                    &
                       )

            call RHSBP_S (                                            &
                 B1 , B2 , BSTOCK , P1 , P2 , SM1 , SM2 , RH1 , RH2 , &
                 J+1, Z(J+1) , ZREF(J+1) , IDT , XDT                , &
                 Profil , ProfilPlan                                , &
                 UniteListing, Erreur                             )

            Fimp(J+1) = Z(J+1) - ZREF(J+1) - Fimp(J+1) / ( SM1 + SM2 )
            Fimp(J+1) = Q(J+1)**2 / ( SM1 + SM2 ) + GPES * ( SM1 + SM2 ) * FIMP(J+1)

            call RHSBP_S (                                            &
                 B1 , B2 , BSTOCK , P1 , P2 , SM1 , SM2 , RH1 , RH2 , &
                 J , Z(J) , ZREF(J) , IDT , XDT ,                     &
                 Profil , ProfilPlan ,                                &
                 UniteListing, Erreur   )

            FIMP(J) = Z(J) - ZREF(J) - FIMP(J) / ( SM1 + SM2 )
            FIMP(J) = Q(J)**2 / ( SM1 + SM2 ) + GPES * ( SM1 + SM2 ) * FIMP(J)

            if((Fimp(J).GT.Fimp(J+1) ).AND.( J.LT.(fin_bief) ) ) go to 101

         endif

      enddo

   Endif

   102 continue

   !--------------------
   ! Fin des traitements
   !--------------------

   !  Erreur%arbredappel = arbredappel_old

   return

   ! FORMATS POUR LES WARNINGS
   ! -------------------------
2000 format('<<ATTENTION>>',/,                                          &
     'Pour Q(',I4,') amont = ',f10.2,' et Z (',i4,') aval = ',f10.2)
2030 format('Passage en torrentiel DETECTE a la section : ',i5,/,       &
     'Bief n0 ',i3,', Abscisse relative : ',g12.3,/,                    &
     'cote critique Zcrit = ',f8.3)
2080 format('A la section : ',I5,', bief n0 ',i3,', abscisse relative = ',g12.3,/, &
     'perte de charge singuliere JS = ',f8.3)
2090 format('<<CONSEIL>>',/,                                    &
     'A la section : ',I5,', bief n0 ',i3,', abscisse relative : ',g12.3, &
     'passage amont fluvial - aval critique trop brutal.',/,    &
     'Raffiner le maillage dans cette zone.')

end subroutine PERMAT
