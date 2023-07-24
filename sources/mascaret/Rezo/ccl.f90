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

subroutine CCL(                     &
                RDLIM, SDLIM, TDLIM , & ! Conditions aux limites calcul
                Z, Q1, Q2           , & ! Cote, debit mineur et majeur
                ZREF                , & ! Cote de reference
                CF1                 , & ! Coeff de frottement mineur
                IDT                 , & ! Positionnement des section/profils
                XDT                 , & ! Positionnement des section/profils
                Profil              , & ! Profils geometriques
                ProfilPlan          , & ! Profils planimetres
                Extremite           , & ! Extremites libres
                Connect             , & ! table de connectivite
                Temps               , & ! Temps
                LoiFrottement       , & ! Loi de frottement
                Erreur                & ! Erreur
                                      )

! *********************************************************************
! PROGICIEL : MASCARET        S.MANDELKERN
!
! VERSION : V8P4R0               EDF-CEREMA
! *********************************************************************
!   FONCTION : CALCUL DES CONDITIONS AUX LIMITES
!   --------
!
!   FICHIERS ENTREE/SORTIE :  --
!   ----------------------
!   SOUS PROGRAMMES APPELANTS : - REZO
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    - LOILIM
!   -------------------------
!------------------------------------------------------------------------

   !========================== Declarations ==============================
   use M_PRECISION           ! Type DOUBLE
   use M_CONSTANTES_CALCUL_C ! Consatantes calcul
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_PARAMETRE_C         ! Constantes numeriques 2/3
   use M_CONNECT_T           ! Definition du type CONNECT_T
   use M_ERREUR_T            ! Definition du type ERREUR_T
   use M_EXTREMITE_T         ! Definition du type EXTREMITE_T
   use M_PROFIL_PLAN_T       ! Profils planimetres
   use M_PROFIL_T            ! Profils geometriques
   use M_RHSBP_S             ! Sous programme RHSBP_SECTION_S
   use M_DEBITANCE_S         ! Sous programme DEBITANCE_S
   use M_LOILIM_I            ! Interface du sous-programme LOILIM
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   implicit none

   !.. Arguments ..
   real(DOUBLE)     , dimension(:), intent(  out) :: RDLIM, SDLIM, TDLIM
   real(DOUBLE)     , dimension(:), intent(in   ) :: Z
   real(DOUBLE)     , dimension(:), intent(in   ) :: Q1
   real(DOUBLE)     , dimension(:), intent(in   ) :: Q2
   real(DOUBLE)     , dimension(:), intent(in   ) :: ZREF
   real(DOUBLE)     , dimension(:), intent(in   ) :: CF1
   real(DOUBLE)     , dimension(:), intent(in   ) :: XDT
   integer          , dimension(:), intent(in   ) :: IDT
   type(PROFIL_T)   , dimension(:), intent(in   ) :: Profil
   type(PROFIL_PLAN_T)            , intent(in   ) :: ProfilPlan
   type(EXTREMITE_T), dimension(:), intent(in   ) :: Extremite
   type(CONNECT_T)                , intent(in   ) :: Connect
   real(DOUBLE)                   , intent(in   ) :: Temps
   integer                        , intent(in   ) :: LoiFrottement
   type(ERREUR_T)                 , intent(inout) :: Erreur

   !.. Variables locales ..
   real(DOUBLE), dimension(size(Q1(:))) :: Q   ! debit total
   integer        :: nombre_point
   integer        :: iext
   integer        :: NSCBIS
   integer        :: num_bief        ! numero du bief
   integer        :: NSC             ! numero de section de calcul
   integer        :: num_prof        ! numero du profil
   integer        :: nb_pas          ! nombre de pas de planimetrage
   integer        :: num_iter        ! numero d'iteration
   integer        :: sens            ! sens d'iteration
   real(DOUBLE)   :: pas             ! valeur du pas de planimetrage
   real(DOUBLE)   :: cote            ! cote de la surface libre (en cours de recherche)
   real(DOUBLE)   :: surface_mouillee  !
   real(DOUBLE)   :: perimetre_mouille !
   real(DOUBLE)   :: rayon_hydrau      !
   real(DOUBLE)   :: pente_fond      ! pente du fond
   real(DOUBLE)   :: debitance       ! debitance
   real(DOUBLE)   :: debitance_cible ! debitance vers laquelle converger
   real(DOUBLE)   :: st1_temp        ! strickler (non utilise)
   integer        :: type_condition
   !character(132) :: arbredappel_old

   ! .. Constantes ..
   integer :: NB_ITER_MAX      = 1000
   integer :: SENS_CROISSANT   =    1
   integer :: SENS_DECROISSANT =   -1

   !========================== Instructions ==============================
   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   !arbredappel_old    = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>CCL'

   Q(:) = Q1(:) + Q2(:)

   do iext = 1 , size(Extremite)

      type_condition = Extremite(iext)%Type

      select case (type_condition)

         ! Hydrogramme
         !------------
         case( CONDITION_TYPE_DEBIT_IMPOSE )

            RDLIM(iext) = 1._DOUBLE
            SDLIM(iext) = 0._DOUBLE
            NSC         = Connect%NumSectionExtLibre(iext)
            TDLIM(iext) = Extremite(iext)%PtQ(1) - Q(NSC)

         ! Limnigramme
         !------------
         case( CONDITION_TYPE_COTE_IMPOSE )

            RDLIM(iext) = 0._DOUBLE
            SDLIM(iext) = 1._DOUBLE
            NSC         = Connect%NumSectionExtLibre(iext)
            TDLIM(iext) = Extremite(iext)%PtZ(1) - Z(NSC)

         ! Loi Z(Q)
         !---------
         case( CONDITION_TYPE_COTE_DEBIT , CONDITION_TYPE_DEBIT_COTE )

            NSC          = Connect%NumSectionExtLibre(iext)
            nombre_point = size(Extremite(iext)%PtZ)

            !          ======
            call LOILIM                            ( &
            !          ======
              RDLIM(iext), SDLIM(iext), TDLIM(iext), &
              Extremite(iext)%PtZ                  , &
              Extremite(iext)%PtQ                  , &
              nombre_point                         , &
              Z(NSC), Q(NSC)                       , &
              iext                                 , &
              Erreur                                 &
                                                   )

            if( Erreur%Numero /= 0 ) then
               return
            end if

         case( CONDITION_TYPE_ZAVAL_QAMONT )

            ! Numero de la section de l'extremite libre
            NSC = Connect%NumSectionExtLibre(iext)

            ! Numero du bief
            num_bief = Connect%NumBiefExtLibre(iext)

            ! Numero de la section origine
            NSCBIS = Connect%OrigineBief(num_bief)

            !-------------------------------------------------------
            ! Si on est a l'aval mais que l'extremite a ete definie
            ! comme une origine de bief,
            ! on se sert du debit a l'autre bout de la branche
            !-------------------------------------------------------
            if( NSCBIS == NSC ) then
               NSCBIS = Connect%FinBief(num_bief)
            end if

            nombre_point = size(Extremite(iext)%PtZ)

            !          ======
            call LOILIM                            ( &
            !          ======
              RDLIM(iext), SDLIM(iext), TDLIM(iext), &
              Extremite(iext)%PtZ                  , &
              Extremite(iext)%PtQ                  , &
              nombre_point                         , &
              Z(NSC), Q(NSCBIS)                    , &
              iext                                 , &
              Erreur                                 &
                                                   )

            if( Erreur%Numero /= 0 ) then
               return
            end if

         case( CONDITION_TYPE_NORMALE )

            NSC             = Connect%NumSectionExtLibre(iext)
            pente_fond      = Extremite(iext)%PenteFond
            debitance_cible = Q1(NSC) / pente_fond**W12 ! Debitance pour un ecoulement normal
            num_prof        = IDT(NSC)
            nb_pas          = Profil(num_prof)%NbPas
            pas             = Profil(num_prof)%Pas
            num_iter        = 0
            sens            = 1
            cote            = ZREF(NSC)

            do while( num_iter /= NB_ITER_MAX + 1 )

               cote = cote + pas

               call RHSBP_SECTION_S   ( &
                  surface_mouillee    , & ! Variable a interpoler sur les profils
                  ZREF(NSC)           , & ! Cote du fond a la section de calcul
                  cote                , & ! Cote d'eau   a la section de calcul
                  IDT(NSC)            , & ! Indices du profil de donnees amont
                  XDT(NSC)            , & ! Position de la section / profils
                  Profil              , & ! Profils geometriques
                  ProfilPlan%S1       , & ! Variable planimetree
                  Erreur                & ! Erreur
                                      )

               if( Erreur%Numero /= 0 ) then
                  return
               end if

               call RHSBP_SECTION_S   ( &
                  perimetre_mouille   , & ! Variable a interpoler sur les profils
                  ZREF(NSC)           , & ! Cote du fond a la section de calcul
                  cote                , & ! Cote d'eau   a la section de calcul
                  IDT(NSC)            , & ! Indices du profil de donnees amont
                  XDT(NSC)            , & ! Position de la section / profils
                  Profil              , & ! Profils geometriques
                  ProfilPlan%P1       , & ! Variable planimetree
                  Erreur                & ! Erreur
                                      )

               if( Erreur%Numero /= 0 ) then
                  return
               end if

               rayon_hydrau = surface_mouillee / perimetre_mouille

               call DEBITANCE_S    ( &
                   debitance       , &
                   st1_temp        , &
                   rayon_hydrau    , &
                   surface_mouillee, &
                   LoiFrottement   , &
                   CF1(NSC)        , &
                   Erreur            &
                                   )

               if( Erreur%Numero /= 0 ) then
                  return
               endif

               if( dabs( debitance - debitance_cible )  <=  EPS3 ) then
                  exit
               endif

               num_iter = num_iter + 1
               if( num_iter == NB_ITER_MAX ) then
                  Erreur%numero = 606
                  Erreur%ft     = err_606
                  Erreur%ft_c   = err_606c
                  call TRAITER_ERREUR( Erreur , Temps , iext , trim(Extremite(iext)%Nom) )
               endif

               if( debitance > debitance_cible .and. sens == SENS_CROISSANT .or. &
                   debitance < debitance_cible .and. sens == SENS_DECROISSANT ) then
                  pas  = - pas /10
                  sens = - sens
               endif

            end do

            RDLIM(iext) = 0._DOUBLE
            SDLIM(iext) = 1._DOUBLE
            NSC         = Connect%NumSectionExtLibre(iext)
            TDLIM(iext) = cote - Z(NSC)

         case (CONDITION_TYPE_SORTIE_LIBRE)

      end select

   end do

  !Erreur%arbredappel = arbredappel_old

  return

end subroutine CCL
