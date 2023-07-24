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

subroutine SARAP ( &
! Donnees/Resultats
    Z            , & ! Cote de la surface libre
    Q1           , & ! Debit mineur
    Q2           , & ! Debit majeur
    P1           , & ! Perimetre mouille mineur
    P2           , & ! Perimetre mouille majeur
    B1           , & ! Largeur au miroir mineur
    B2           , & ! Largeur au miroir majeur
    BS           , & ! Largeur au miroir zone de stockage
    RH1          , & ! Rayon hydraulique mineur
    RH2          , & ! Rayon hydraulique majeur
    S1           , & ! Section mouillee mineur
    S2           , & ! Section mouillee majeur
    BETA         , & ! Coefficient du modele Debord
    Froude       , & ! Nombre de Froude
    Extremite    , & ! Conditions aux limite
    Apport       , & ! Apports
    Qinjec       , & ! Debit injecte
    Qdeverse     , & ! Debit total deverse par un dev lateral ponctuel ou lineique
    Temps        , & ! Temps
! Profils
    Profil       , & ! Profils geometriques
    ProfilPlan   , & ! Profils planimetrees
    F1           , & ! Fonction impulsion
! Modele
    X            , & ! Maillage
    CF1          , & ! Strickler mineur
    CF2          , & ! Strickler majeur
    ZREF         , & ! Cote de reference
    XDT          , & ! Position section/profil amont
    IDT          , & ! Numero du profil amont
    Connect      , & ! Table de connectivite
    Singularite  , & ! Singularites (seuils)
    PCSing       , & ! Pertes de charge singulieres
    Deversoir    , & ! Deversoirs
    ModeleLit    , & ! Modelisation lit
    Confluent    , & ! Caracteristiques des confluences
    Abaque       , & ! Abaques des pertes de  charges aux confluences
    Algorithme   , & ! Algorithme de parcours des biefs
! Parametres
     Impression   , & ! Flag d'impression
     UniteListing , & ! Unite logique du fichier listing
     LoiFrottement,& ! Loi de frottement
     PerteChargeConfluent,& ! Perte de charge automatique aux confluents
     CQMV         , &
     decentrement , &
     Erreur        & ! Erreur
                 )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
! FONCTION :                                                          .
! .          CALCUL EN REGIME PERMANENT A L'AIDE DU CODE SARA         .
! .          ADAPTE A LA RESOLUTION D'UN RESEAU RAMIFIE               .
! .....................................................................
! . ADAPTE A LA RESOLUTION D'UN RESEAU RAMIFIE                        .
! . LIT MINEUR / MAJEUR                                               .
! .                                                                   .
! . LE SENS DE PARCOURS DU RESEAU EST DETERMINE DANS LE SOUS-PROGRAMME.
! . ALGOP, APPELE LORS DE LA PHASE DE LECTURE DES DONNEES             .
! .....................................................................

   !============================= Declarations ===========================
   use M_PRECISION
   ! Constantes nommees
   use M_CONSTANTES_CALCUL_C
   use M_MESSAGE_C
   use M_PARAMETRE_C
   ! Types derives
   use M_APPORT_T
   use M_CONFLUENT_T       ! Type confluent
   use M_CONNECT_T
   use M_DEVERSOIR_T
   use M_ERREUR_T
   use M_EXTREMITE_T
   use M_PROFIL_T
   use M_PROFIL_PLAN_T
   use M_SINGULARITE_T
   ! Procedures-module
   use M_NUM_BIEF_S        ! Calcul du numero du bief d'une section
   use M_RHSBP_S           ! Sous-programme RHSBP_GENERIQUE_S
   use M_FROUDE_S          ! calcul du nombre de Froude
   use M_TRAITER_ERREUR_I  ! Interface generique d'appel aux
                           ! procedures de traitement des erreurs
   ! Interfaces
   use M_CQINJ_I
   use M_PERSAR_I
   use M_REPAR_I

   !.. Implicit Declarations ..
   implicit none

   !.. Donnees/Resultats ..
   real(DOUBLE), dimension(:), intent(inout) :: Z, Q1, Q2
   real(DOUBLE), dimension(:), intent(inout) :: P1, P2
   real(DOUBLE), dimension(:), intent(inout) :: B1, B2, BS
   real(DOUBLE), dimension(:), intent(inout) :: RH1, RH2
   real(DOUBLE), dimension(:), intent(inout) :: S1, S2
   real(DOUBLE), dimension(:), intent(inout) :: BETA
   real(DOUBLE), dimension(:), intent(  out) :: Froude
   ! Conditions aux limites des extremites libres
   type(EXTREMITE_T),dimension(:), intent(in   ) :: Extremite
   type(ERREUR_T)                , intent(inout) :: Erreur
   ! Maillage
   real(DOUBLE), dimension(:), intent(in) :: X, CF1, CF2
   real(DOUBLE), dimension(:), intent(in) :: ZREF
   real(DOUBLE), dimension(:), intent(in) :: XDT
   integer     , dimension(:), intent(in) :: IDT
   ! Variables planimetrees
   type (PROFIL_T), dimension(:)    , intent(in   ) :: Profil
   type (PROFIL_PLAN_T)             , intent(in   ) :: ProfilPlan
   real(DOUBLE) ,dimension (:,:)    , intent(in   ) :: F1
   ! Debits d apports
   type(APPORT_T), dimension(:)     , intent(in   ) :: Apport
   real(DOUBLE) , dimension(:)     , intent(inout) :: Qdeverse
   real(DOUBLE)     , dimension(:) , intent(inout):: QInjec
   ! Pertes de charge singulieres
   real(DOUBLE), dimension(:)       , intent(inout) :: PCSing
   ! Table du connectivite du reseau
   type(CONNECT_T)                  , intent(in   ) :: Connect
   ! Algorithme de resolution
   integer, dimension(:)            , intent(in   ) :: Algorithme
   ! Singularites
   type(SINGULARITE_T), dimension(:), intent(in   ) :: Singularite
   ! Modelisation du lit
   integer                          , intent(in   ) :: ModeleLit
   ! Confluences
   type(CONFLUENT_T)   , dimension(:) , intent(in   ) :: Confluent
   real(DOUBLE)    , dimension(6,6,5) , intent(in   ) :: Abaque
   ! Parametres
   logical                          , intent(in   ) :: Impression
   integer                          , intent(in   ) :: UniteListing
   integer                          , intent(in   ) :: LoiFrottement,CQMV
   logical                          , intent(in   ) :: PerteChargeConfluent
   logical                          , intent(in   ) :: decentrement
   ! Temps
   real(DOUBLE)                     , intent(in   ) :: Temps
   ! Deversoirs
   type (DEVERSOIR_T), dimension(:) , intent(in   ) :: Deversoir
   !.. Local Scalars ..
   real(DOUBLE) :: VMOY
   real(DOUBLE) :: DEB
   integer      :: isec      ! Compteur sur les sections
   integer      :: num_bief  ! Numero du bief
   real(DOUBLE) :: absc_rel  ! abscisse relative d'une section
   !character(132) :: !arbredappel_old  ! arbre dappel precedent
   real(DOUBLE)   :: HI, VI
   !.. Local Arrays ..
   real(DOUBLE), dimension(size(X)) :: Q

   !============================= Instructions ===========================
   ! INITIALISATIONS ET ALLOCATIONS
   ! ------------------------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>SARAP'

   ! REMPLISSAGE DU TABLEAU DES APPORTS
   ! ----------------------------------
   if(( size(Apport) > 0 ).OR.( size(Deversoir) > 0 )) then
!cct      do iinj = 1 , size(Apport)
         ! Calcul de QInjec, tableau dimensionne a NbSect
         ! representant les apports
         ! ----------------------------------------------
         call CQINJ (                 &
              QInjec                , &
              X, Z                  , &
              Apport                , &
              Deversoir             , &
              Qdeverse              , &
              Erreur                  )
         if( Erreur%Numero /= 0 ) then
            return
         end if
!cct      end do
   else
      QInjec(:) = 0._DOUBLE
   end if

   ! CALCUL DE BASE DE LA LIGNE D'EAU
   ! --------------------------------
   call PERSAR        ( &
       Z              , & !/RESULTATS/
       Q              , &
       X              , & !/DONNEES NON MODIFIEES/
       ZREF           , &
       CF1            , &
       CF2            , &
       PCSing         , &
       IDT            , &
       XDT            , &
       Profil         , &
       ProfilPlan     , &
       F1             , &
       QInjec         , &
       Connect        , &
       Singularite    , &
       Extremite      , &
       ModeleLit      , &
       Confluent      , & ! Caracteristiques des confluences
       Abaque         , & ! Abaques des pertes de  charges aux confluences
       Impression     , &
       UniteListing   , &
       Temps          , &
       Algorithme     , &
       LoiFrottement  , &
       PerteChargeConfluent, &
       CQMV           , &
       decentrement   , &
       Erreur           &
       )

   if( Erreur%Numero /= 0 ) then
      return
   end if

   do isec = 1 , size(X)
      ! CALCUL DES GRANDEURS HYDRAULIQUES
      ! ---------------------------------
      call RHSBP_S                                                    &
           ( B1(isec) , B2(isec) , BS(isec) , P1(isec) , P2(isec) ,   &
           S1(isec) , S2(isec) , RH1(isec) , RH2(isec) ,              &
           isec , Z(isec) , ZREF(isec) , IDT , XDT ,                  &
           Profil, ProfilPlan,                                        &
           Unitelisting, Erreur                                       &
          )

      if( Erreur%Numero /= 0 ) then
         return
      end if

      ! CALCUL DE LA REPARTITION LIT MINEUR / LIT MAJEUR
      ! ------------------------------------------------
      call REPAR (      &
          DEB         , & ! Resultats
          VMOY        , &
          BETA(isec)  , &
          Q1(isec)    , &
          Q2(isec)    , &
          S1(isec)    , & ! Donnees modifiees
          S2(isec)    , &
          RH1(isec)   , &
          RH2(isec)   , &
          P1(isec)    , & ! Donnees non modifiees
          P2(isec)    , &
          Q(isec)     , &
          CF1(isec)   , &
          CF2(isec)   , &
          ModeleLit    , &
          LoiFrottement, &
          Profil(IDT(isec))%Nom, &
          Erreur        &
          )

      if( Erreur%Numero /= 0 ) then
         return
      end if

      ! CALCUL ET CONTROLE DU NOMBRE DE FROUDE
      !---------------------------------------
      VI = Q(isec) / ( S1(isec) + S2(isec) )
      if( ABS(B1(isec)).GT.EPS15 ) then
         HI = ( S1(isec) + S2(isec) ) / ( B1(isec) + B2(isec) )
      else
         HI = EPS3
      end if
      call FROUDE_S             ( &
                Froude(isec)    , &
                Beta(isec)      , &
                VI              , &
                HI              , &
                isec            , &
                Connect         , &
                X               , &
                Erreur            &
                                  )
   end do

   ! VERIFICATION DE LA COTE EN TOUT POINT
   ! -------------------------------------
   do isec = 1 , size(X)
      if( ( Z(isec) - ZREF(isec) ) < W0 ) then
         Erreur%Numero = 31
         Erreur%ft     = err_31
         Erreur%ft_c   = err_31c
         num_bief      = NUM_BIEF_S (Connect, isec, Erreur)
         absc_rel      = X(isec) - X(Connect%OrigineBief(num_bief))
         call TRAITER_ERREUR( Erreur , isec , num_bief , absc_rel )
         return
      end if
   end do

   ! FIN DU CALCUL
   ! -------------

   !Erreur%arbredappel = !arbredappel_old

   return

   ! ... Format Declarations ...

end subroutine SARAP
