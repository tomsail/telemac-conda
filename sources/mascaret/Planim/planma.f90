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

subroutine PLANMA         ( &
                            ! Resultats
                  SectionPlan , & ! Section planimetrees
                            ! Donnees
                       Profil , & ! Caracteritiques des profils
                   ProfilPlan , & ! Profils planimetrees
                            ! Modele
                        NbPas , & ! Nombre de pas de planimetrage
                            X , & ! Abscisse des sections de calcul
                           DZ , & ! Caracteristiques des sections
                           XD , & ! Abscisse des interfaces
                          DZD , & ! Pas de planimetrage d'une interface
                          XDT , & ! Position relative de la section/Profil
                          IDT , & ! Profil de donnees amont de la section
                      Connect , & ! Connectivite du reseau
                          CF1 , & ! Coefficient de frottement mineur
                          CF2 , & ! Coefficient de frottement majeur
                             ! Parametre
         PresenceZoneStockage , & ! Presence de zone de stockage
                LoiFrottement , & ! Loi de frottement utilisee
               OptionCourlis  , & ! Activation de Courlis
                       varsed , & ! Courlis : profil evolution
                 TempsInitial , & ! Courlis
                       Erreur )

! *********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
! FONCTION :
! --------
! CODE MASCARET : CALCUL DES TABLEAUX PLANIMETRES COMPLETS POUR MASCARET
!
!
!-----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE : --
!   -------------------------
!
!   SOUS PROGRAMME APPELANT :    PLANIM
!   -------------------------
!   SOUS-PROGRAMMES APPELES :
!   -------------------------
!       - CALAIG : CALCUL DE L'INTEGRALE DE 0 A S DE -1/S*DC/DX (A S CONSTANT)
!       - CALDEB : CALCUL DE LA DEBITANCE
!       - CALDYG : CALCUL DE DY/DX A S CONSTANT
!***********************************************************************

   !============================= Declarations ============================
   !.. Modules importes ..
   !----------------------
   use M_MY_CPT_PLANIM      ! PU2017: Ajout du module pour la variable globale MY_CPT_PLANIM
!   use M_MY_PLANIM_VAR      ! PU2017: Ajout du module pour la variable globale DELTAH
   use M_MY_GLOBAL_VAR_SED ! PU2017: Module pour les variables globales contenant les volumes et epaisseurs de sediments transportes
   use M_SHARE_VAR, ONLY: Temps ! Courlis

   use M_PRECISION
   use M_PARAMETRE_C    ! GPES, SEPS
   use M_MESSAGE_C     ! Messages d'erreur
   use M_PROFIL_PLAN_T  ! Profils planimetres
   use M_PROFIL_T       ! Caracteristiques des profils
   use M_SECTION_PLAN_T ! Sections planimetres
   use M_SECTION_T      ! Caracteristiques des Sections
   use M_CONNECT_T      ! Tables de connectivite du reseau
   use M_ERREUR_T       ! Type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_CALAIG_I       ! Interface du sous-programme CALAIG
   use M_CALDEB_I       ! Interface du sous-programme CALDEB
   use M_CALDYG_I       ! Interface du sous-programme CALDYG

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! Resultat
   type(SECTION_PLAN_T)          , intent(inout) :: SectionPlan
   ! Donnees
   type(PROFIL_T) , dimension(:) , intent(in   ) :: Profil
   type(PROFIL_PLAN_T)           , intent(in   ) :: ProfilPlan
   ! Modele
   integer                       , intent(in   ) :: NbPas
   real(DOUBLE), dimension(:)    , intent(in   ) :: X
   real (DOUBLE)  , dimension(:) , pointer       :: DZ
   real(DOUBLE), dimension(:)    , pointer       :: XD
   real(DOUBLE), dimension(:)    , pointer       :: DZD
   real(DOUBLE), dimension(:)    , intent(in   ) :: XDT
   integer     , dimension(:)    , intent(in   ) :: IDT
   type(CONNECT_T)               , intent(in   ) :: Connect
   real(DOUBLE), dimension(:)    , intent(in   ) :: CF1
   real(DOUBLE), dimension(:)    , intent(in   ) :: CF2
   ! Parametre
   logical     ,                   intent(  out) :: PresenceZoneStockage
   integer     ,                   intent(in   ) :: LoiFrottement
   type(ERREUR_T),                 intent(inout) :: Erreur
   !.. Variables locales ..
   !-----------------------
   integer :: isec  ! Compteur sur les sections
   integer :: ipas  ! compteur sur les pas de planimetrage
   integer :: ibief ! Compteur sur les biefs
   integer :: profil_gauche
   integer :: profil_droit
   integer :: nb_pas
   integer :: retour ! Code de retour des fonctions intrinseques
   !character(132) :: !arbredappel_old
   real(DOUBLE) :: DELTAX             ! Ecart entre 2 profils consecutifs
   real(DOUBLE) :: surface_gauche     ! Section mouillee profil gauche
   real(DOUBLE) :: surface_droite     ! Section mouillee profil droit
   real(DOUBLE) :: larg_miroir_gauche ! Largeur au miroir profil gauche
   real(DOUBLE) :: larg_miroir_droite ! Largeur au miroir profil droit
   real(DOUBLE) :: Trav1(Nbpas)      ! tableau de travail pour CALAIG

   ! Courlis
   real(DOUBLE)    , dimension(:), pointer      :: varsed
   real(DOUBLE)                  , intent(in   ):: TempsInitial
   logical                       , intent(in   ):: OptionCourlis
   logical                                      :: condition_courlis
   real(DOUBLE) :: Hthres ! PU2017: Hauteur seuil pour nouveau critere de planimetrage


   !============================= Instructions ===========================

   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   retour = 0
   ! ALLOCATION DU TABLEAU SECTION
   !------------------------------
!MS2019 : ajouter une condition sur le lancement de Courlis ou non
   if(Temps .EQ. TempsInitial) then
   if(.not.associated(DZ)) allocate( DZ(size( X )) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'DZ' )
      return
   end if

   if(.not.associated(DZD)) allocate( DZD(size( X )-1) , STAT = retour )
   if (retour /= 0) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'DZD' )
      return
   end if

   if(.not.associated(XD)) allocate( XD(size( X )-1) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'XD' )
      return
   end if

   ! ALLOCATION DES TABLEAUX DE PLANIMETRAGE
   ! DE LA STRUCTURE SECTIONPLAN A NB_PAS
   !----------------------------------------
   nb_pas = NbPas

   if(.not.associated(SectionPlan%S)) allocate( SectionPlan%S(size( X ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%S' )
      return
   end if

   if(.not.associated(SectionPlan%S1)) allocate( SectionPlan%S1(size( X ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%S1' )
      return
   end if

   if(.not.associated(SectionPlan%S2)) allocate( SectionPlan%S2(size( X ),nb_pas) , STAT = retour )
   if (retour /= 0) then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'SectionPlan%S2')
      return
   end if

   if(.not.associated(SectionPlan%SS)) allocate(SectionPlan%SS(size(X),nb_pas), STAT = retour)
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%SS' )
      return
   end if

   if(.not.associated(SectionPlan%S1GEO)) allocate(SectionPlan%S1GEO(size(X),nb_pas), STAT = retour)
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%S1GEO' )
      return
   end if

   if(.not.associated(SectionPlan%CELER)) allocate( SectionPlan%CELER(size( X ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%CELER' )
      return
   end if

   if(.not.associated(SectionPlan%B)) allocate( SectionPlan%B(size( X ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%B' )
      return
   end if

   if(.not.associated(SectionPlan%INV)) allocate( SectionPlan%INV(size( X ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%INV' )
      return
   end if

   if(.not.associated(SectionPlan%INTE)) allocate( SectionPlan%INTE(size( X ),nb_pas), STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%INTE' )
      return
   end if

   if(.not.associated(SectionPlan%DYDX)) allocate( SectionPlan%DYDX(size( X ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%DYDX' )
      return
   end if

   if(.not.associated(SectionPlan%PRESS)) allocate( SectionPlan%PRESS(size( X ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%PRESS' )
      return
   end if

   if(.not.associated(SectionPlan%DEB)) allocate( SectionPlan%DEB(size( X ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%DEB' )
      return
   end if

   if(.not.associated(SectionPlan%DEB1)) allocate( SectionPlan%DEB1(size( X ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR ( Erreur , 'SectionPlan%DEB1' )
      return
   end if

   if(.not.associated(SectionPlan%DEB2)) allocate( SectionPlan%DEB2(size( X ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%DEB2' )
      return
   end if

   if(.not.associated(SectionPlan%SD)) allocate( SectionPlan%SD(size( X )-1,nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%SD' )
      return
   end if

   if(.not.associated(SectionPlan%SD1)) allocate( SectionPlan%SD1(size( X )-1,nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%SD1' )
      return
   end if

   if(.not.associated(SectionPlan%SD2)) allocate( SectionPlan%SD2(size( X )-1,nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR (Erreur, 'SectionPlan%SD2')
      return
   end if

   if(.not.associated(SectionPlan%PRESSD)) allocate( SectionPlan%PRESSD(size( X )-1,nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%PRESSD' )
      return
   end if

   if(.not.associated(SectionPlan%BD)) allocate( SectionPlan%BD(size( X )-1,nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%BD' )
      return
   end if

   if(.not.associated(SectionPlan%DEBD)) allocate( SectionPlan%DEBD(size( X )-1,nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'SectionPlan%DEBD' )
      return
   end if


   ! AUTRES INITIALISATIONS
   !-----------------------
   SectionPlan%CELER(:,1) = 0._DOUBLE
   SectionPlan%INV  (:,1) = 0._DOUBLE
   SectionPlan%INTE (:,1) = 0._DOUBLE
   SectionPlan%DYDX (:,1) = 0._DOUBLE
   SectionPlan%PRESS(:,1) = 0._DOUBLE
   SectionPlan%S1GEO(:,1) = 0._DOUBLE
 end if

   ! Boucle sur les sections
   ! ***********************
   boucle1_section : do isec = 1 , size(X)

      if(optionCourlis .AND. clipping_option) then
        ! critere de variation sedimentaire
        Hthres = fracH*(myZsl(isec)-Profil(isec)%ZRef)
        condition_courlis = abs(varsed(isec)) > Hthres
      else if (optionCourlis) then
        condition_courlis = abs(varsed(isec)) > absolute_clip
      else
        condition_courlis = .false.
      endif

      if ( Temps .EQ. TempsInitial .OR. condition_courlis ) then ! PU2017: Changement du critere sur le planimetrage


        profil_gauche = IDT(isec)
        profil_droit  = IDT(isec) + 1
        ! Boucle sur les pas
        ! ******************
        boucle1_pas_de_planim : do ipas = 1 , NbPas
           ! 1 ) CALCUL DE LA SECTION MOUILLEE
           ! ---------------------------------
           ! a) MINEURE
           surface_gauche = ProfilPlan%S1(profil_gauche,ipas)
           surface_droite = ProfilPlan%S1(profil_droit,ipas)
           SectionPlan%S1(isec,ipas) = surface_gauche + ( surface_droite - surface_gauche ) * XDT(isec)
           ! b) MAJEURE
           surface_gauche = ProfilPlan%S2(profil_gauche,ipas)
           surface_droite = ProfilPlan%S2(profil_droit ,ipas)
           SectionPLan%S2(isec,ipas) = surface_gauche + ( surface_droite - surface_gauche ) * XDT(isec)
           ! c) TOTALE
           SectionPlan%S(isec,ipas) = SectionPlan%S1(isec,ipas) + SectionPlan%S2(isec,ipas)
           ! d) ZONE DE STOCKAGE
           surface_gauche = ProfilPlan%SS(profil_gauche,ipas)
           surface_droite = ProfilPlan%SS(profil_droit ,ipas)
           SectionPlan%SS(isec,ipas) = surface_gauche + (surface_droite - surface_gauche) * XDT(isec)

           if( SectionPlan%SS(isec,ipas) > 10 * SEPS ) then
              PresenceZoneStockage = .true.
           endif



           ! 2 ) CALCUL DE LA LARGEUR AU MIROIR
           ! ----------------------------------
           larg_miroir_gauche = ProfilPlan%B1(profil_gauche,ipas) + &
                                ProfilPlan%B2(profil_gauche,ipas)
           larg_miroir_droite = ProfilPlan%B1(profil_droit ,ipas) + &
                                ProfilPlan%B2(profil_droit ,ipas)
           SectionPlan%B(isec,ipas) = larg_miroir_gauche + &
                         (larg_miroir_droite - larg_miroir_gauche) * XDT(isec)

           ! 3 ) CALCUL DE LA DEBITANCE
           ! --------------------------
           call CALDEB (           &
               SectionPlan%DEB   , &
               SectionPlan%DEB1  , &
               SectionPlan%DEB2  , &
               isec              , &
               ipas              , &
               ProfilPlan%S1     , &
               ProfilPlan%S2     , &
               ProfilPlan%P1     , &
               ProfilPlan%P2     , &
               CF1               , &
               CF2               , &
               profil_gauche     , &
               profil_droit      , &
               XDT(isec)         , &
               LoiFrottement     , &
               Erreur              &
                             )
           if( Erreur%Numero /= 0 ) then
              return
           endif

           ! 4 ) CALCUL DE LA CELERITE DES ONDES
           ! -----------------------------------
           SectionPlan%CELER(isec,ipas) = DSQRT( GPES * SectionPlan%S(isec,ipas) / SectionPlan%B(isec,ipas) )

        end do boucle1_pas_de_planim

      end if

   end do boucle1_section

   ! BOUCLE SUR LES BIEFS
   ! ********************
   boucle2_bief : do ibief = 1 , size(Connect%OrigineBief)
      boucle2_section : do isec = Connect%OrigineBief(ibief) , Connect%FinBief(ibief)
         if(optionCourlis .AND. clipping_option) then
           ! critere de variation sedimentaire
           Hthres = fracH*(myZsl(isec)-Profil(isec)%ZRef)
           condition_courlis = abs(varsed(isec)) > Hthres
         else if (optionCourlis) then
           condition_courlis = abs(varsed(isec)) > absolute_clip
         else
           condition_courlis = .false.
         endif

         if ( Temps .EQ. TempsInitial .OR. condition_courlis ) then ! PU2017: Changement du critere sur le planimetrage
           profil_gauche = IDT(isec)
           profil_droit  = IDT(isec) + 1
           DZ(isec) = Profil(profil_gauche)%Pas +               &
                (Profil(profil_droit )%Pas - Profil(profil_gauche)%Pas) * XDT(isec)
           boucle2_pas_de_planim : do ipas=2,NbPas
              ! 4 ) CALCUL DE LA PRESSION
              ! -------------------------
              SectionPlan%PRESS(isec,ipas) = SectionPlan%PRESS(isec,ipas-1) +  &
                                    GPES * ( SectionPlan%S(isec,ipas-1) +      &
                                    SectionPlan%S(isec,ipas) ) *               &
                                    DZ(isec) / 2._DOUBLE
              ! 5 ) CALCUL DES INVARIANTS DE RIEMANN ( = INTEGRALE DE (C/S))
              ! ------------------------------------
              if( ipas.gt.2 ) then
                 SectionPlan%INV(isec,ipas) =  SectionPlan%INV(isec,ipas-1)  +  &
                          (SectionPlan%CELER(isec,ipas  ) / SectionPlan%S(isec,ipas  )  +  &
                           SectionPlan%CELER(isec,ipas-1) / SectionPlan%S(isec,ipas-1)) *  &
                          (SectionPlan%S(isec,ipas  ) - SectionPlan%S(isec,ipas-1)) /  &
                                  2._DOUBLE
              else
                 SectionPlan%INV(isec,ipas) = 2._DOUBLE * SQRT(GPES * DZ(isec))
              endif
                 ! 6 ) CALCUL DES Termes CUBIQUES pour boussinesq
              ! ------------------------------------
              if( ipas.gt.2 ) then
                 SectionPlan%S1GEO(isec,ipas) =  SectionPlan%S1GEO(isec,ipas-1)  +  &
                                  (((ipas-1)*DZ(isec))*SectionPlan%S(isec,ipas-1)   &
                                  +(ipas*DZ(isec))*SectionPlan%S(isec,ipas))*DZ(isec)/2._DOUBLE
              else
                SectionPlan%S1GEO(isec,ipas) = 0._DOUBLE
              endif
              ! 6 ) CALCUL DE ...
              !     ( = INTEGRALE DE 0 A S DE (-1/S*DC/DX (A S CONSTANT) )
              ! -----------------
              call CALAIG (                     &
                   SectionPlan%INTE           , &
                   isec                       , &
                   ipas                       , &
                   X                          , &
                   SectionPlan%S              , &
                   SectionPlan%CELER          , &
                   SectionPlan%B              , &
                   Connect%OrigineBief(ibief) , &
                   Connect%FinBief(ibief)     , &
                   Trav1                      , &
                   NbPas                      , &
                   Erreur                       &
                   )
              if( Erreur%Numero /= 0 ) then
                 return
              endif
              ! 7 ) CALCUL DE ...     ( = DY/DX A S CONSTANT)
              ! -----------------
              call CALDYG (                    &
                   SectionPlan%DYDX          , &
                   isec                      , &
                   ipas                      , &
                   SectionPlan%S             , &
                   X                         , &
                   Profil%Pas                , &
                   IDT                       , &
                   XDT                       , &
                   Connect%OrigineBief(ibief), &
                   Connect%FinBief(ibief)    , &
                   NbPas                     , &
                   Erreur                      &
                   )
              if( Erreur%numero /= 0 ) then
                 return
              endif
           end do boucle2_pas_de_planim
         end if
      end do boucle2_section
   end do boucle2_bief

   ! CALCUL DES VARIABLES NECESSAIRES AUX INTERFACES
   ! ***********************************************
   boucle3_bief : do ibief = 1 , size(Connect%OrigineBief)
      boucle3_section : do isec = Connect%OrigineBief(ibief), Connect%FinBief(ibief) - 1
         if(optionCourlis .AND. clipping_option) then
           ! critere de variation sedimentaire
           Hthres = fracH*(myZsl(isec)-Profil(isec)%ZRef)
           condition_courlis = abs(varsed(isec)) > Hthres
         else if (optionCourlis) then
           condition_courlis = abs(varsed(isec)) > absolute_clip
         else
           condition_courlis = .false.
         endif

         if ( Temps .EQ. TempsInitial .OR. condition_courlis ) then ! PU2017: Changement du critere sur le planimetrage

           XD(isec) = ( X(isec+1) + X(isec) ) / 2._DOUBLE
           ! DETERMINATION DES PROFILS DE DONNES SERVANT A L'INTERPOLATION
           ! -------------------------------------------------------------
           if( IDT(isec) == IDT(isec+1) ) then
              profil_gauche = IDT(isec)
              profil_droit  = IDT(isec) + 1
           else
              if( XD(isec) < Profil(IDT(isec+1))%AbsAbs ) then
                 profil_gauche = IDT(isec)
                 profil_droit  = IDT(isec) + 1
              else
                 profil_gauche = IDT(isec+1)
                 profil_droit  = IDT(isec+1) + 1
              endif
           endif
           DELTAX    = Profil(profil_droit)%AbsAbs - Profil(profil_gauche)%AbsAbs
           DZD(isec) = (Profil(profil_droit)%Pas *                  &
                       (XD(isec) - Profil(profil_gauche)%AbsAbs) +  &
                        Profil(profil_gauche)%Pas *                 &
                       (Profil(profil_droit)%AbsAbs - XD(isec)) ) / DELTAX

           boucle3_pas_de_planim : do ipas = 1 , NbPas
              ! 1 ) CALCUL DE LA SECTION MOUILLEE
              ! ---------------------------------
              ! a ) MINEURE
              SectionPlan%SD1(isec,ipas) = ( SectionPlan%S1(isec,ipas) + SectionPlan%S1(isec+1,ipas)) / 2._DOUBLE
              ! b ) MAJEURE
              SectionPlan%SD2(isec,ipas) = ( SectionPlan%S2(isec,ipas) + SectionPlan%S2(isec+1,ipas)) / 2._DOUBLE
              ! c ) TOTALE
              SectionPlan%SD(isec,ipas)  = ( SectionPlan%S (isec,ipas) + SectionPlan%S (isec+1,ipas)) / 2._DOUBLE
              ! 2 ) CALCUL DE LA LARGEUR AU MIROIR
              ! ----------------------------------
              SectionPLan%BD(isec,ipas)  = ( SectionPlan%B (isec,ipas) + SectionPlan%B (isec+1,ipas)) / 2._DOUBLE
              ! 3 ) CALCUL DE LA DEBITANCE
              ! --------------------------
              SectionPlan%DEBD(isec,ipas) = (SectionPlan%DEB(isec,ipas) + SectionPlan%DEB(isec+1,ipas) ) / 2._DOUBLE
              ! 4 ) CALCUL DE LA PRESSION
              ! -------------------------
              if( ipas == 1 ) then
                 SectionPlan%PRESSD(isec,ipas) = 0._DOUBLE
              else
                 SectionPlan%PRESSD(isec,ipas) = SectionPLan%PRESSD(isec,ipas-1) + &
                 GPES*( SectionPlan%SD(isec,ipas-1) +             &
                 SectionPlan%SD(isec,ipas) ) *        &
                 DZD(isec) / 2._DOUBLE
              endif
           end do   boucle3_pas_de_planim
         end if
      end do      boucle3_section
   end do         boucle3_bief

   ! Fin des traitements
   !--------------------

   !Erreur%arbredappel = !arbredappel_old

  return


end subroutine PLANMA
