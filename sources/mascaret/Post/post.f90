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

subroutine POST( TAUF                      , &
                 Y           , HMOY        , &
                 Q2G         , Q2D         , &
                 VOL         , VOLS        , &
                 CHARG                     , &
                 SS                        , &
                 V1          , V2          , &
                 ZMAX        , TZMAX       , &
                 VZMAX                     , &
                 ZMIN        , TZMIN       , &
                 V1MIN       , V1MAX       , &
                 BMAX                      , &
                 TOND                      , &
                 QMAX        , TQMAX       , &
                 EMAX                      , &
                 ! Donnees
                 Z                         , &
                 Q1          , Q2          , &
                 S1          , S2          , &
                 B1          , B2          , &
                 BS                        , &
                 P1          , P2          , &
                 RH1         , RH2         , &
                 BETA                      , &
                 PROF                      , &
                 PROFIL_PLAN               , &
                 Temps, TempsInitial       , &
                 Iteration                 , &
                 ! Modele
                 X                         , &
                 ZREF                      , &
                 IDT         , XDT         , &
                 CF1                       , &
                 ! Etats
                 ZInitial                  , &
                 ! Parametres
                 Noyau                     , &
                 PresenceZoneStockage      , &
                 DebProgressifLM           , &
                 DZarriveeFront            , &
                 PhaseSimulation           , &
                 VarCalc                   , &
                 LoiFrottement             , &
                 RepriseCalcul             , &
                 FichierRepriseLecture     , &
                 ! Erreur
                 Erreur                      &
                                           )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!  FONCTION :
!  --------
!
!             CONSTRUCTION DES VARIABLES HYDRAULIQUES
!  choisies par l'utilisateur a partir des variables de sortie HYDRAU
!  et des variables planimetrees fournies par la fonctionnalite PLANIM
!
!----------------------------------------------------------------------------
!                             ARGUMENTS
! .___________________.____._________________________________________________
! !    NOM            !TYPE!MODE!                   ROLE
! !___________________!____!____!____________________________________________
! ! TAUF              ! R  !<-->! Contrainte au fond
! ! Y                 ! R  !<-->! Hauteur d'eau maximale
! ! HMOY              ! R  !<-->! Hauteur d'eau moyenne
! ! Q2G               ! R  !<-->! Debit majeur gauche
! ! Q2D               ! R  !<-->! Debit majeur droit
! ! VOL               ! R  !<-->! Volume du lit actif
! ! VOLS              ! R  !<-->! Volume de stockage
! ! CHARG             ! R  !<-->! Charge
! ! SS                ! R  !<-->! Section mouillee des zones de stockage
! ! V1                ! R  !<-->! Vitesse mineure
! ! V2                ! R  !<-->! Vitesse majeure
! ! ZMAX              ! R  !<-->! Cote maximale atteinte
! ! TZMAX             ! R  !<-->! Instant de cote maximale atteinte
! ! ZMIN              ! R  !<-->! Cote minimale atteinte
! ! TZMIN             ! R  !<-->! Instant de cote minimale atteinte
! ! V1MIN             ! R  !<-->! Vitesse mineure minimale
! ! V1MAX             ! R  !<-->! Vitesse majeure minimale
! ! BMAX              ! R  !<-->! Largeur au miroir maximale
! ! VZMAX             ! R  !<-->! Vitesse a la cote maximale
! ! TOND              ! R  !<-->! Instant d'arrivee d'onde
! ! QMAX              ! R  !<-->! Debit maximal
! ! TQMAX             ! R  !<-->! Instant de debit maximal
! ! EMAX              ! R  !<-->! Energie maximale
! ! X                 ! R  ! -->! Abscisse des sections
! ! ZREF              ! R  ! -->! Cote du fond
! ! CF1               ! R  ! -->! Coefficient de frottement mineur
! ! Z                 ! R  ! -->! Cote d'eau a une section de calcul
! ! Q1                ! R  ! -->! Debit mineur
! ! Q2                ! R  ! -->! Debit majeur
! ! S1                ! R  ! -->! Section mouillee mineure
! ! S2                ! R  ! -->! Section mouillee majeure
! ! B1                ! R  !<-->! Largeur au miroir mineure
! ! B2                ! R  !<-->! Largeur au miroir majeure
! ! BS                ! R  !<-->! Largeur au miroir des zones de stockage
! ! P1                ! R  !<-->! Perimetre mouille mineur
! ! P2                ! R  !<-->! Perimetre mouille majeur
! ! RH1               ! R  !<-->! Rayon hydraulique mineur
! ! RH2               ! R  !<-->! Rayon hydraulique majeur
! ! BETA              ! R  ! -->! Coefficient beta du modele DEBORD
! ! IDT               ! I  ! -->! Indices  des sections de donnees
! ! XDT               ! R  ! -->! Position des sections de donnees
! ! PROF              ! T  ! -->! Structure des profils de donnees
! ! PROFIL_PLAN       ! T  ! -->! Structure des variables des profils
! ! Noyau             ! I  ! -->! Noyau de calcul
! ! TEMPS             ! R  ! -->! Temps
! ! Iteration         ! I  ! -->! Iteration
! ! TempsInitial      ! R  ! -->! Temps de debut de la simulation
! ! ZInitial          ! R  !<-->! Valeurs initiales des cotes
! ! PresenceZoneStockage! L! -->! Existence ou non d'une zone de stockage
! ! DebProgressifLM   ! L  ! -->! Presence d'un debordement progress du lit majeur
! ! PhaseSimulation   ! I  ! -->! Phase de la simulation
! ! ModeleCalcul      ! I  ! -->! Modele de calcul utilise
! ! VarCalc           ! L  ! -->! Liste des variables a calculer
! ! LoiFrottement     ! L  ! -->! Loi de frottement
! ! Erreur            ! T  !<-->! Erreur
! !___________________!____!____!____________________________________________
!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!----------------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :        Neant
!   ----------------------
!
!   SOUS-PROGRAMME(S) APPELANT(S) :
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S)   :
!   ---------------------------     - TAUFOND : Calcul de la contrainte au fond
!                                   - VOLUME : calcul du volume entre deux sections
!                                   - RHSBP_GENERIQUE : interpolation sur les profils
!
!   COMMENTAIRES : L'ordre de calcul des variables est a respecter
!   ------------
!
!   DOCUMENTATION EXTERNE :
!   ---------------------
!
!***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PROFIL_T             ! Definition du type PROFIL
   use M_PROFIL_PLAN_T        ! Definition du type PROFIL_PLAN_T
   use M_RHSBP_S              ! Sous-programme de module RHSBP_GENERIQUE
   use M_CONSTANTES_CALCUL_C  ! Constantes pour les phases et modeles de calcul
   use M_INDEX_VARIABLE_C     ! Constantes servant a reperer les variables
   use M_PARAMETRE_C          ! EPS_ONDE, W23, GPES
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_MESSAGE_C            ! liste des messages d'erreurs
   use M_TRAITER_ERREUR_I     ! Traitement des erreurs
   use M_FICHIER_T            ! Definition du type Fichier

   !.. Declaration Explicite ..
   !---------------------------
   implicit none
   !.. Arguments ..
   !---------------
   ! Resultats
   type(FICHIER_T)              , intent(in) :: FichierRepriseLecture
   real(DOUBLE)   , dimension(:), pointer :: TAUF
   real(DOUBLE)   , dimension(:), pointer :: Y
   real(DOUBLE)   , dimension(:), pointer :: HMOY
   real(DOUBLE)   , dimension(:), pointer :: Q2G
   real(DOUBLE)   , dimension(:), pointer :: Q2D
   real(DOUBLE)   , dimension(:), pointer :: VOL
   real(DOUBLE)   , dimension(:), pointer :: VOLS
   real(DOUBLE)   , dimension(:), pointer :: CHARG
   real(DOUBLE)   , dimension(:), pointer :: SS
   real(DOUBLE)   , dimension(:), pointer :: V1
   real(DOUBLE)   , dimension(:), pointer :: V2
   real(DOUBLE)   , dimension(:), pointer :: ZMAX
   real(DOUBLE)   , dimension(:), pointer :: TZMAX
   real(DOUBLE)   , dimension(:), pointer :: ZMIN
   real(DOUBLE)   , dimension(:), pointer :: TZMIN
   real(DOUBLE)   , dimension(:), pointer :: V1MIN
   real(DOUBLE)   , dimension(:), pointer :: V1MAX
   real(DOUBLE)   , dimension(:), pointer :: BMAX
   real(DOUBLE)   , dimension(:), pointer :: VZMAX
   real(DOUBLE)   , dimension(:), pointer :: TOND
   real(DOUBLE)   , dimension(:), pointer :: QMAX
   real(DOUBLE)   , dimension(:), pointer :: TQMAX
   real(DOUBLE)   , dimension(:), pointer :: EMAX
   ! Donnees
   real(DOUBLE)   , dimension(:), pointer :: Z
   real(DOUBLE)   , dimension(:), pointer :: Q1
   real(DOUBLE)   , dimension(:), pointer :: Q2
   real(DOUBLE)   , dimension(:), pointer :: S1
   real(DOUBLE)   , dimension(:), pointer :: S2
   real(DOUBLE)   , dimension(:), pointer :: B1
   real(DOUBLE)   , dimension(:), pointer :: B2
   real(DOUBLE)   , dimension(:), pointer :: BS
   real(DOUBLE)   , dimension(:), pointer :: P1
   real(DOUBLE)   , dimension(:), pointer :: P2
   real(DOUBLE)   , dimension(:), pointer :: RH1
   real(DOUBLE)   , dimension(:), pointer :: RH2
   real(DOUBLE)   , dimension(:), pointer :: BETA
   type (PROFIL_T), dimension(:), intent(in   ) :: PROF
   type (PROFIL_PLAN_T)         , intent(in   ) :: PROFIL_PLAN
   real(DOUBLE)                 , intent(in   ) :: Temps
   real(DOUBLE)                 , intent(in   ) :: TempsInitial
   integer                      , intent(in   ) :: Iteration
   ! Modele
   real(DOUBLE)   , dimension(:), pointer       :: X
   real(DOUBLE)   , dimension(:), pointer       :: ZREF
   real(DOUBLE)   , dimension(:), pointer       :: CF1
   integer        , dimension(:), intent(in   ) :: IDT
   real(DOUBLE)   , dimension(:), intent(in   ) :: XDT
   ! Etat
   real(DOUBLE)   , dimension(:), pointer       :: ZInitial
   ! Parametres
   integer                      , intent(in   ) :: Noyau
   logical                      , intent(in   ) :: PresenceZoneStockage
   logical                      , intent(in   ) :: DebProgressifLM
   real (DOUBLE)                , intent(in   ) :: DZArriveeFront
   integer                      , intent(in   ) :: PhaseSimulation
   logical        , dimension(:), intent(in   ) :: VarCalc
   integer                      , intent(in   ) :: LoiFrottement
   logical                      , intent(in   ) :: RepriseCalcul
   type(ERREUR_T)               , intent(inout) :: Erreur
   !.. Constantes ..
   !----------------
   ! Constante donnant la valeur minimale par defaut de HMOY
   real(DOUBLE), parameter :: HMOY_MIN = EPS3
   !.. Variables locales ..
   !-----------------------
   real(DOUBLE), dimension(size(X))   :: Q        ! debit total
   real(DOUBLE), dimension(size(X))   :: S        ! section mouillee totale
   real(DOUBLE), dimension(size(X))   :: rhmoy
   real(DOUBLE), dimension(size(X))   :: vmoy
   real(DOUBLE), dimension(size(X))   :: var_temp
   integer                            :: nb_sect  ! nombre de sections de calcul
   integer                            :: retour   ! code de retour des fonctions
                                                  ! intrinseques
   integer        :: isec            ! Compteur sur les sections
   integer        :: ul              ! Unite logique
   integer        :: ivar            ! numero de variable
   !character(132) :: !arbredappel_old ! arbre d'appel d'avant l'entree
                                      ! dans la procedure
   character(4)   :: Nom

   !============================ Instructions ===================================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>POST'

   nb_sect = size(X)
   ul      = FichierRepriseLecture%Unite

!   if( PhaseSimulation == PHASE_INITIALISATION ) then
      ! Allocations
      if (VarCalc(VAR_TOND) ) then
       if(.not.associated(ZInitial)) then
         allocate( ZInitial(nb_sect) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'ZInitial' )
            return
         end if
         ZInitial(:) = Z(:)
       endif
      endif

      if( VarCalc(VAR_Y) ) then
         if(.not.associated(Y)) then
            allocate (Y(nb_sect), STAT = retour)
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Y' )
               return
            end if
            Y(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_HMOY) ) then
         if(.not.associated(HMOY)) then
            allocate (HMOY(nb_sect), STAT = retour)
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'HMOY' )
               return
            end if
            HMOY(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_SS) ) then
         if(.not.associated(SS)) then
            allocate (SS(nb_sect), STAT = retour)
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'SS' )
               return
            end if
            SS(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_Q2G) ) then
         if(.not.associated(Q2G)) then
            allocate( Q2G(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Q2G' )
               return
            end if
            Q2G(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_Q2D) ) then
         if(.not.associated(Q2D)) then
            allocate( Q2D(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Q2D' )
               return
            end if
            Q2D(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_VOL) ) then
         if(.not.associated(VOL)) then
            allocate( VOL(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'VOL' )
               return
            end if
            VOL(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_VOLS) ) then
         if(.not.associated(VOLS)) then
            allocate( VOLS(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'VOLS' )
               return
            end if
            VOLS(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_CHARG) ) then
         if(.not.associated(CHARG)) then
            allocate( CHARG(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'CHARG' )
               return
             end if
             CHARG(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_ZMAX) ) then
         if(.not.associated(ZMAX)) then
            allocate( ZMAX(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'ZMAX' )
               return
            end if
            ZMAX(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_TZMAX) ) then
         if(.not.associated(TZMAX)) then
            allocate( TZMAX(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'TZMAX' )
               return
            end if
            TZMAX(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_VZMAX) ) then
         if(.not.associated(VZMAX)) then
            allocate( VZMAX(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'VZMAX' )
               return
            end if
            VZMAX(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_ZMIN) ) then
         if(.not.associated(ZMIN)) then
            allocate( ZMIN(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'ZMIN' )
               return
            end if
            ZMIN(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_TZMIN) ) then
         if(.not.associated(TZMIN)) then
            allocate( TZMIN(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'TZMIN' )
               return
            end if
            TZMIN(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_V1MIN) ) then
         if(.not.associated(V1MIN)) then
            allocate( V1MIN(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'V1MIN' )
               return
            end if
            V1MIN(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_V1MAX) ) then
         if(.not.associated(V1MAX)) then
            allocate( V1MAX(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft   = err_5
               Erreur%ft_c = err_5c
               call TRAITER_ERREUR  (Erreur, 'V1MAX')
               return
            end if
            V1MAX(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_BMAX) ) then
         if(.not.associated(BMAX)) then
            allocate( BMAX(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR  (Erreur, 'BMAX')
               return
            end if
            BMAX(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_TOND) ) then
         if(.not.associated(TOND)) then
            allocate( TOND(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'TOND' )
               return
            end if
            TOND(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_QMAX) ) then
         if(.not.associated(QMAX)) then
            allocate( QMAX(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'QMAX' )
               return
            end if
            QMAX(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_TQMAX) ) then
         if(.not.associated(TQMAX)) then
            allocate( TQMAX(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'TQMAX' )
               return
            end if
            TQMAX(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_EMAX) ) then
         if(.not.associated(EMAX)) then
            allocate( EMAX(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'EMAX' )
               return
            end if
            EMAX(:) = 0._DOUBLE
         endif
      endif

      if( VarCalc(VAR_TAUF) ) then
         if(.not.associated(TAUF)) then
            allocate( TAUF(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'TAUF' )
               return
            end if
         endif
      endif

      if( VarCalc(VAR_V1) ) then
         if(.not.associated(V1)) then
            allocate( V1(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'V1' )
               return
            end if
         endif
      endif

      if( VarCalc(VAR_V2) ) then
         if(.not.associated(V2)) then
            allocate( V2(nb_sect) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'V2' )
               return
            end if
         endif
      endif

      if( Noyau == NOYAU_MASCARET ) then

         if( VarCalc(VAR_B1) ) then
            if(.not.associated(B1)) then
               allocate( B1(nb_sect) , STAT = retour )
               if( retour /= 0 ) then
                  Erreur%Numero = 5
                  Erreur%ft     = err_5
                  Erreur%ft_c   = err_5c
                  call TRAITER_ERREUR( Erreur , 'B1' )
                  return
               end if
            endif
         endif

         if( VarCalc(VAR_B2) ) then
            if(.not.associated(B2)) then
               allocate( B2(nb_sect) , STAT = retour )
               if( retour /= 0 ) then
                  Erreur%Numero = 5
                  Erreur%ft     = err_5
                  Erreur%ft_c   = err_5c
                  call TRAITER_ERREUR( Erreur , 'B2' )
                  return
               end if
            endif
         endif

         if( VarCalc(VAR_BS) ) then
            if(.not.associated(BS)) then
               allocate( BS(nb_sect) , STAT = retour )
               if( retour /= 0 ) then
                  Erreur%Numero = 5
                  Erreur%ft   = err_5
                  Erreur%ft_c = err_5c
                  call TRAITER_ERREUR( Erreur , 'BS' )
                  return
               end if
            endif
         endif

         if( VarCalc(VAR_P1) ) then
            if(.not.associated(P1)) then
               allocate( P1(nb_sect) , STAT = retour )
               if( retour /= 0 ) then
                  Erreur%Numero = 5
                  Erreur%ft     = err_5
                  Erreur%ft_c   = err_5c
                  call TRAITER_ERREUR( Erreur , 'P1' )
                  return
               end if
            endif
         endif

         if( VarCalc(VAR_P2) ) then
            if(.not.associated(P2)) then
               allocate( P2(nb_sect) , STAT = retour )
               if( retour /= 0 ) then
                  Erreur%Numero = 5
                  Erreur%ft     = err_5
                  Erreur%ft_c   = err_5c
                  call TRAITER_ERREUR( Erreur , 'P2' )
                  return
               end if
            endif
         endif

         if( VarCalc(VAR_RH1) ) then
            if(.not.associated(RH1)) then
               allocate( RH1(size(X)) , STAT = retour )
               if( retour /= 0 ) then
                  Erreur%Numero = 5
                  Erreur%ft     = err_5
                  Erreur%ft_c   = err_5c
                  call TRAITER_ERREUR( Erreur , 'RH1' )
                  return
               end if
            endif
         endif

         if( VarCalc(VAR_RH2) ) then
            if(.not.associated(RH2)) then
               allocate( RH2(size(X)) , STAT = retour )
               if( retour /= 0 ) then
                  Erreur%Numero = 5
                  Erreur%ft     = err_5
                  Erreur%ft_c   = err_5c
                  call TRAITER_ERREUR( Erreur , 'RH2' )
                  return
               end if
            endif
         endif
      endif

   if( PhaseSimulation == PHASE_INITIALISATION ) then

      if (VarCalc(VAR_TOND) ) then
       ZInitial(:) = Z(:)
      endif 
      !
      ! relecture des variables max stockees
      !
      IF( RepriseCalcul ) THEN
      !============================
      ! ouverture du fichier a lire
      !============================
         If( VarCalc(VAR_ZMAX) )  then
            ivar = 30
            read(ul,'(1X,A)') Nom
            read(ul, *) (ZMAX(isec),isec = 1, nb_sect)
         endif
         If( VarCalc(VAR_TZMAX) ) then
            ivar = 31
            read(ul,'(1X,A)') Nom
            read(ul, *) (TZMAX(isec),isec = 1, nb_sect)
         endif
         If( VarCalc(VAR_VZMAX) ) then
            ivar = 32
            read(ul,'(1X,A)') Nom
           read(ul, *) (VZMAX(isec),isec = 1, nb_sect)
         endif
         If( VarCalc(VAR_TOND) ) then
           ivar = 38
           read(ul,'(1X,A)') Nom
           read(ul, *) (TOND(isec),isec = 1, nb_sect)
         endif
         If( VarCalc(VAR_QMAX) ) then
            read(ul,'(1X,A)') Nom
            read(ul, *) (QMAX(isec), isec = 1, nb_sect)
         endif
         If( VarCalc(VAR_V1MAX) ) then
            read(ul,'(1X,A)') Nom
            read(ul, *) (V1MAX(isec), isec = 1, nb_sect)
         endif
         If( VarCalc(VAR_TQMAX) ) then
            read(ul,'(1X,A)') Nom
            read(ul, *) (TQMAX(isec), isec = 1, nb_sect)
         endif
         If( VarCalc(VAR_EMAX) ) then
            read(ul,'(1X,A)') Nom
            read(ul, *) (EMAX(isec), isec = 1, nb_sect)
         endif
      ELSE
         !
         ! initialisation des variables max et min
         !
         If( VarCalc(VAR_ZMAX) )   ZMAX(:) = 0._DOUBLE
         If( VarCalc(VAR_TZMAX) ) TZMAX(:) = 0._DOUBLE
         If( VarCalc(VAR_VZMAX) ) VZMAX(:) = 0._DOUBLE
         If( VarCalc(VAR_TZMIN) ) TZMIN(:) = 0._DOUBLE
         If( VarCalc(VAR_V1MIN) ) V1MIN(:) = 0._DOUBLE
         If( VarCalc(VAR_ZMIN) )   ZMIN(:) = 0._DOUBLE
         If( VarCalc(VAR_TOND) )   TOND(:) = TempsInitial
         If( VarCalc(VAR_QMAX) )   QMAX(:) = 0._DOUBLE
         if( VarCalc(VAR_V1MAX) ) V1MAX(:) = 0._DOUBLE
         if( VarCalc(VAR_TQMAX) ) TQMAX(:) = 0._DOUBLE
         if( VarCalc(VAR_EMAX) )   EMAX(:) = 0._DOUBLE
      end if
   endif

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_B1) ) then
      ! Calcul de la largeur au miroir mineure
      !----------------------------------------
      call RHSBP_GENERIQUE_S( &
           B1               , &
           PROFIL_PLAN%B1   , &
           ZREF             , &
           Z                , &
           IDT              , &
           XDT              , &
           PROF             , &
           nb_sect          , &
           Erreur             &
                            )
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_B2) ) then
      ! Calcul de la largeur au miroir majeure
      !----------------------------------------
      call RHSBP_GENERIQUE_S( &
           B2               , &
           PROFIL_PLAN%B2   , &
           ZREF             , &
           Z                , &
           IDT              , &
           XDT              , &
           PROF             , &
           nb_sect          , &
           Erreur             &
                           )
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_BS) ) then
      ! Calcul de la largeur au miroir des zones de stockage
      !------------------------------------------------------
      call RHSBP_GENERIQUE_S( &
           BS               , &
           PROFIL_PLAN%BS   , &
           ZREF             , &
           Z                , &
           IDT              , &
           XDT              , &
           PROF             , &
           nb_sect          , &
           Erreur             &
                           )
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_P1) ) then
      ! Calcul du perimetre mouille mineur
      !------------------------------------
      call RHSBP_GENERIQUE_S( &
           P1               , &
           PROFIL_PLAN%P1   , &
           ZREF             , &
           Z                , &
           IDT              , &
           XDT              , &
           PROF             , &
           nb_sect          , &
           Erreur             &
                           )

   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_P2) ) then
      ! Calcul du perimetre mouille majeur
      !------------------------------------
      call RHSBP_GENERIQUE_S( &
           P2               , &
           PROFIL_PLAN%P2   , &
           ZREF             , &
           Z                , &
           IDT              , &
           XDT              , &
           PROF             , &
           nb_sect          , &
           Erreur             &
                           )
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_RH1) ) then
      ! Calcul du Rayon hydraulique mineur
      !------------------------------------
      call RHSBP_GENERIQUE_S( &
           S1               , &
           PROFIL_PLAN%S1   , &
           ZREF             , &
           Z                , &
           IDT              , &
           XDT              , &
           PROF             , &
           nb_sect          , &
           Erreur             &
                          )

      call RHSBP_GENERIQUE_S( &
           P1               , &
           PROFIL_PLAN%P1   , &
           ZREF             , &
           Z                , &
           IDT              , &
           XDT              , &
           PROF             , &
           nb_sect          , &
           Erreur            &
                          )

      RH1(:) = S1(:) / P1(:)

   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_RH2) ) then
      ! Calcul du Rayon hydraulique majeur
      !------------------------------------
      call RHSBP_GENERIQUE_S( &
           S2               , &
           PROFIL_PLAN%S2   , &
           ZREF             , &
           Z                , &
           IDT              , &
           XDT              , &
           PROF             , &
           nb_sect          , &
           Erreur             &
                           )

      call RHSBP_GENERIQUE_S( &
           P2               , &
           PROFIL_PLAN%P2   , &
           ZREF             , &
           Z                , &
           IDT              , &
           XDT              , &
           PROF             , &
           nb_sect          , &
           Erreur             &
                           )

      where( P2(:) > EPS3 )
         RH2(:) = S2(:) / P2(:)
      elsewhere
         RH2(:) = 0._DOUBLE
         S2(:)  = 0._DOUBLE
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_V1) ) then
      ! Calcul de la vitesse du lit mineur
      !------------------------------------
      where( abs(S1(:)).GT.EPS6 )
         V1(:) = Q1(:) / S1(:)
      elsewhere
         V1(:) = 0._DOUBLE
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_V2) ) then
      ! Calcul de la vitesse du lit majeur
      !------------------------------------
      where( abs(S2(:)).GT.EPS6 )
         V2(:) = Q2(:) / S2(:)
      elsewhere
         V2(:) = 0._DOUBLE
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_Y) ) then
      ! Calcul  de la hauteur d'eau
      !-----------------------------
      Y(:) = Z(:) - ZREF(:)
      where( Y(:) <= EPS3 )
         Y(:) = EPS3
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_HMOY) ) then
      ! Calcul de la hauteur d'eau moyenne
      !------------------------------------
      where( abs(B1(:)).GT.EPS6 )
         HMOY(:) = ( S1(:) + S2(:) ) / ( B1(:) + B2(:) )
      elsewhere
         HMOY(:) = HMOY_MIN
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_SS) ) then
      ! Calcul de la section mouillee de la zone de stockage
      !------------------------------------------------------
      call RHSBP_GENERIQUE_S( &
           SS               , &
           PROFIL_PLAN%SS   , &
           ZREF             , &
           Z                , &
           IDT              , &
           XDT              , &
           PROF             , &
           nb_sect          , &
           Erreur             &
                           )
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_Q2G) ) then
      ! Calcul du debit dans le lit majeur gauche
      !-------------------------------------------
      call RHSBP_GENERIQUE_S( &
           Q2G              , &
           PROFIL_PLAN%S2G  , &
           ZREF             , &
           Z                , &
           IDT              , &
           XDT              , &
           PROF             , &
           nb_sect          , &
           Erreur             &
                           )

      ! Dans RHSBP on interpole en fait S2G
      ! Pour obtenir Q2G on doit multiplier par V2
      Q2G(:) = Q2G(:) * V2(:)
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_Q2D) ) then
      ! Calcul du debit dans le lit majeur droit
      !------------------------------------------
      Q2D(:) = Q2(:) - Q2G(:)
   end if


   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_VOL) .or. VarCalc(VAR_VOLS) ) then
      call VOLUME(              &
          VOL                 , &
          VOLS                , &
          X                   , &
          S1                  , &
          S2                  , &
          SS                  , &
          nb_sect             , &
          PresenceZoneStockage, &
          DebProgressifLM       &
                 )
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_CHARG) ) then
      ! Calcul de la charge
      !---------------------
      var_temp(:) = ( Q1(:) + Q2(:) ) / ( S1(:) + S2(:) )
      CHARG(:)    = Z(:) + BETA(:) * ( var_temp(:) * var_temp(:) / ( 2._DOUBLE * GPES ) )
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_TZMAX) ) then
      ! Calcul de l'instant ou la cote maximale est atteinte
      !------------------------------------------------------
      where ( Z(:) > zmax(:) )
         tzmax(:) = Temps
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_VZMAX) ) then
      ! Calcul de la vitesse a la cote maximale
      !-----------------------------------------
      where( Z(:) > zmax(:) )
         vzmax(:) = V1(:)
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_ZMAX) ) then
      ! Calcul de la cote maximale atteinte
      !-------------------------------------
      where( Z(:) > zmax(:) )
         zmax (:) = Z(:)
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_ZMIN) ) then
      ! Calcul de la cote minimale est atteinte
      !------------------------------------------------------
      where( Z(:) <= zmin(:) )
         zmin(:) = Z(:)
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_TZMIN) ) then
      ! Calcul de l'instant ou la cote minimale atteinte
      !-------------------------------------
      where( Z(:) <= zmin(:) )
         tzmin (:) = Temps
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_V1MIN) ) then
      ! Calcul de la vitesses mineure minimale
      !----------------------------------------
      where( V1(:) <= v1min(:) .or. Iteration == 0 )
         v1min(:) = v1(:)
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_V1MAX) ) then
      ! Calcul de la vitesses mineure maximale
      !----------------------------------------
      where( V1(:) >= v1max(:) )
        v1max(:) = v1(:)
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_BMAX) ) then
      ! Calcul de la largeur au miroir maximale
      !-----------------------------------------
      var_temp(:) =  B1(:) + B2(:)
      where( var_temp(:) > bmax(:) )
         bmax(:) = var_temp(:)
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_TOND) ) then
      ! Calcul du temps d'arrivee de l'onde
      !-------------------------------------
      ! var_temp represente ici l'ecart des Z
      var_temp(:) = dabs( ZInitial(:) - Z(:) )
      where( ( var_temp(:) > DZArriveeFront ) .and. ( TOND(:) == TempsInitial ) )
         TOND(:) = Temps
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_TQMAX) ) then
      ! Calcul de l'instant ou le debit maximal est atteint
      !-----------------------------------------------------
      ! var_temp represente ici le debit total
      var_temp(:) = Q1(:) + Q2(:)
      where ( var_temp(:) > qmax(:) )
         tqmax(:) = Temps ! instant ou le debit maximal est atteint
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_QMAX) ) then
      ! Calcul du debit maximal
      !-------------------------
      ! var_temp represente ici le debit total
      var_temp(:) = Q1(:) + Q2(:)
      where( var_temp(:) > qmax(:) )
         qmax (:) = var_temp(:)
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_EMAX) ) then
      ! Calcul de l'energie maximale
      !------------------------------
      var_temp(:) = V1(:)**2 / ( 2 * GPES )
      where( var_temp(:) > EMAX(:) )
         EMAX (:) = var_temp(:)
      end where
   end if

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if( VarCalc(VAR_TAUF) ) then
      ! Calcul de la hauteur d'eau moyenne
      !------------------------------------
      ! var_temp represente ici la variable habituelle HMOY
      where( abs(B1(:)).GT.EPS6 )
         var_temp(:) = ( S1(:) + S2(:) ) / ( B1(:) + B2(:) )
      elsewhere
         var_temp(:) = HMOY_MIN
      end where

      ! Calcul du rayon hydraulique moyen
      !----------------------------------
      rhmoy(:) = ( S1(:) + S2(:) ) / ( P1(:) + P2(:) )

      ! Calcul de la vitesse moyenne
      !------------------------------
      vmoy(:) = ( Q1(:) + Q2(:) ) / ( S1(:) + S2(:) )

      S(:) = S1(:) + S2(:)

      ! Calcul de la contrainte au fond
      !---------------------------------

      Q(:) = Q1(:) + Q2(:)

      call TAUFOND       ( &
           TAUF          , &
           var_temp      , &
           rhmoy         , &
           S             , &
           Q             , &
           CF1           , &
           LoiFrottement   &
                         )

   end if

   !Erreur%arbredappel = !arbredappel_old

   return

contains


   !***********************************************************************
   !
   !  SOUS-PROGRAMME INTERNE
   !
   !***********************************************************************

   subroutine TAUFOND       ( &
              TAUF          , & ! Contrainte au fond
              HMoy          , & ! Hauteur moyenne
              RHMoy         , & ! rayon hydraulique moyen
              S             , & ! Section mouillee moyenne
              Q             , & ! Debit total
              CF1           , & ! Coefficient de frottement mineur
              LoiFrottement   & ! Loi de frottement
                            )
   !***********************************************************************
   !
   !  FONCTION :   CALCUL DES VALEURS DE LA CONTRAINTE AU FOND
   !  --------
   !
   !  SOUS-PROGRAMME(S) APPELANT(S) : POST
   !  -----------------------------
   !  SOUS-PROGRAMME(S) APPELE(S)   : Neant
   !  ---------------------------
   !***********************************************************************

      !============================= Declarations ===========================
      use M_PRECISION
      use M_PARAMETRE_C
      use M_DEBITANCE_S

      !.. Declaration Explicite ..
      implicit none

      !.. Arguments ..
      real(DOUBLE), dimension(:), intent(out) :: TAUF
      real(DOUBLE), dimension(:), intent(in ) :: HMoy  ! Hauteur d'eau
      real(DOUBLE), dimension(:), intent(in ) :: RHMoy ! Rayon hydraulique
      real(DOUBLE), dimension(:), intent(in ) :: S     ! Section mouillee totale
      real(DOUBLE), dimension(:), intent(in ) :: CF1   ! Coefficient de frottement
      real(DOUBLE), dimension(:), intent(in ) :: Q
      integer                   , intent(in ) :: LoiFrottement
      !.. Constantes ..
      real(DOUBLE), parameter :: RHO = 1000.0_DOUBLE   ! Masse volumique de l'eau
      !.. Variables locales ..
      real(DOUBLE)                        :: st1_temp  ! variable sans objet
      real(DOUBLE), dimension(size(tauf)) :: pente     ! Pente d'energie
      real(DOUBLE), dimension(size(tauf)) :: debitance ! debitancee

      !============================ Instructions ==============================
      do isec = 1 , size(TAUF)
         call DEBITANCE_S ( &
           debitance(isec), &
           st1_temp       , &
           RHMoy(isec)    , &
           S(isec)        , &
           LoiFrottement  , &
           CF1(isec)      , &
           Erreur           &
                          )
      end do

      pente(:) = ( Q(:) / debitance(:) )**2

      where( abs(HMoy(:)).GT.EPS6 )
         TAUF(:) = RHO * GPES * RHMoy(:) * pente(:)
      elsewhere
         TAUF(:) = 0._DOUBLE
      end where

   end subroutine TAUFOND

   !***********************************************************************
   !
   !  SOUS-PROGRAMME INTERNE
   !
   !***********************************************************************

   subroutine VOLUME(        &
       VOL                 , & ! Volume du lit actif
       VOLS                , & ! Volume de stockage
       X                   , & ! Abscisses des sections de calcul
       S1                  , & ! Section mouillee mineure
       S2                  , & ! Section mouillee majeure
       SS                  , & ! Section mouillee de la zone de stockage
       NbSect              , & ! Nombre de sections
       PresenceZoneStockage, & ! Existence ou non d'une zone de stockage
       DebProgressifLM       & ! Existence ou non d'un debordement du lit majeur
                    )
   !***********************************************************************
   !
   !  FONCTION :
   !  --------
   !         CALCUL DU VOLUME DANS LE LIT ACTIF (LIT MINEUR + LIT MAJEUR),
   !         CALCUL DU VOLUME DANS LA ZONE DE STOCKAGE
   !
   !  SOUS-PROGRAMME(S) APPELANT(S) : POST
   !  -----------------------------
   !  SOUS-PROGRAMME(S) APPELE(S)   : Neant
   !  ---------------------------
   !***********************************************************************

      !============================= Declarations ===========================
      !.. Declaration Explicite ..
      implicit none

      !.. Arguments ..
      real(DOUBLE), dimension(:), intent(out) :: VOL
      real(DOUBLE), dimension(:), intent(out) :: VOLS
      real(DOUBLE), dimension(:), intent(in ) :: X
      real(DOUBLE), dimension(:), intent(in ) :: S1
      real(DOUBLE), dimension(:), intent(in ) :: S2
      real(DOUBLE), dimension(:), intent(in ) :: SS
      integer                   , intent(in ) :: NbSect
      logical                   , intent(in ) :: PresenceZoneStockage
      logical                   , intent(in ) :: DebProgressifLM

      !.. Variables locales ..
      real(DOUBLE) :: delta_x

      !============================ Instructions ==============================
      ! Calcul du volume dans le lit actif (lit mineur + lit majeur)
      !--------------------------------------------------------------
      VOL (1) = 0._DOUBLE
      do isec = 2 , NbSect
         delta_x    = X(isec) - X(isec - 1)
         VOL (isec) = ( S1(isec - 1) + S2(isec - 1) + S1(isec) + S2(isec) ) * delta_x / 2._DOUBLE
      end do

      ! Calcul du volume dans la zone de stockage
      !-------------------------------------------
      if( .not.PresenceZoneStockage .and. .not.DebProgressifLM ) then
         VOLS(:) = 0._DOUBLE
      else
         do isec = 2 , NbSect
            delta_x    = X(isec) - X(isec - 1)
            VOLS(isec) = (SS(isec - 1) + SS(isec)) * delta_x / 2._DOUBLE
         end do
      end if

   end subroutine VOLUME

end subroutine POST
