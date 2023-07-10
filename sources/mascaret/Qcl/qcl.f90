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

subroutine QCL( &
                ! Resultats : Objets dont l'etat est mis a jour
           Apport , & ! tableau des Apports
      Singularite , & ! tableau des singularites
        Extremite , & ! tableau des Extremites libres
                ! Donnees
        LoiHydrau , & ! tableau des lois hydrauliques
            Temps , & ! Temps
               Np , & ! Numero de pas de temps
               Q1 , & ! Debits mineurs dans les sections de calcul
           Froude , & ! Nombre de Froude
                ! Modele
          Connect , & ! Connectivite du reseau
                ! Parametre
      NoyauCalcul , & ! Noyau de calcul utilise
           Erreur   & ! Erreur
                    )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!                             D. POIZAT
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
!  FONCTION :
!  --------
!
!  La fonctionnalite QCL fournit les valeurs au temps t :
!  - de chaque loi aux limites Q, ou Z, ou Q et Z, ou loi Z(Q)
!  - des debits d'apport
!  - des cotes amont des singularites obeissant a une loi Z(t)
!
!----------------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :        Neant
!   ----------------------
!
!   SOUS-PROGRAMME(S) APPELANT(S) :
!   -----------------------------
!   SOUS-PROGRAMME(S) APPELE(S)   : - INTERPOLATION_S (dans module)
!   ---------------------------
!
!   COMMENTAIRES :
!   ------------
!
!   DOCUMENTATION EXTERNE :
!   ---------------------
!
!***********************************************************************

   !============================= Declarations ============================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C  ! Constantes pour les phases et modeles de calcul
   use M_MESSAGE_C            ! Messages d'erreur
   use M_APPORT_T             ! Definition du type APPORT_T
   use M_CONNECT_T            ! Definition du type CONNECT_T
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_EXTREMITE_T          ! Definition du type EXTREMITE_T
   use M_LOI_T                ! Definition du type LOI_T
   use M_SINGULARITE_T        ! Definition du type SINGULARITE_T
   use M_TRAITER_ERREUR_I     ! Interface generique de gestion des erreurs
   use M_INTERPOLATION_S      ! Sous-programme INTERPOLATION_S

   !.. Declaration Explicite ..
   !---------------------------
   implicit none

   !.. Arguments ..
   !---------------
   type(APPORT_T)     , dimension(:), intent(inout) :: Apport
   type(SINGULARITE_T), dimension(:), pointer       :: Singularite
   type(EXTREMITE_T)  , dimension(:), intent(inout) :: Extremite
   ! Donnees
   type(LOI_T)        , dimension(:), intent(in   ) :: LoiHydrau
   real(DOUBLE)                     , intent(in   ) :: Temps
   Integer                          , intent(in   ) :: np
   real(DOUBLE)       , dimension(:), intent(in   ) :: Q1
   real(DOUBLE)       , dimension(:), intent(in   ) :: Froude
   ! Modele
   type(CONNECT_T)                  , intent(in   ) :: Connect
   ! Parametres
   integer                          , intent(in   ) :: NoyauCalcul
   ! Gestion des erreurs
   type(ERREUR_T)                   , intent(inout) :: Erreur
   !.. Constantes ..
   !----------------
   ! Constante  pour les interpolations
   integer , parameter :: ORDRE_INTERPOLATION = 1
   !.. Variables locales ..
   !-----------------------
   integer :: nb_apport
   integer :: nb_sing
   integer :: nb_extremite
   integer :: nb_loi_hydrau
   integer        :: iapp            ! Compteur sur les apports
   integer        :: ising           ! Compteur sur les singularitess
   integer        :: iext            ! Compteur sur les extremites
   integer        :: ibief           ! Compteur sur les branches
   integer        :: num_loi         ! Numero de la loi utilisee
   !character(132) :: !arbredappel_old ! Ancien arbre d'appel
   integer :: num_sect
   logical :: regime_torrentiel
   logical :: debut_bief         ! position de l'extremite
   logical :: amont_bief         ! idem par rapport a l'ecoulmt

   !============================ Instructions ===================================
   Erreur%Numero = 0
   !  !arbredappel_old    = trim(!Erreur%arbredappel)
   !  !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>QCL'

   nb_loi_hydrau = size(LoiHydrau)

   !==============================================================
   !           Calcul du debit d'apport au temps Temps
   !==============================================================
   nb_apport = size(Apport)
   if( nb_apport /= 0 ) then

      do iapp = 1 , nb_apport

         num_loi = Apport(iapp)%NumeroLoi

         !--------------------------------------------
         ! Interpolation du debit au temps Temps
         ! a partir de la loi LoiHydrau correspondante
         !--------------------------------------------
         call INTERPOLATION_S (                     &
              Apport(iapp)%Debit                  , &
              Temps                               , &
              ORDRE_INTERPOLATION                 , &
              LoiHydrau(num_loi)%Temps            , &
              LoiHydrau(num_loi)%Debit            , &
              size(LoiHydrau(num_loi)%Temps   )   , &
              Erreur                              )
         if( Erreur%Numero /= 0 ) then
           return
         end if

      end do

   end if

   !==============================================================
   !           Calcul des cotes amont des singularites
   !       ou des cotes inferieures etsuperieures des vannes
   !                           au temps Temps
   !==============================================================
   nb_sing = size(Singularite)
   if( nb_sing /= 0 ) then

      do ising = 1 , nb_sing

         select case( Singularite(ising)%Type )

            case( SINGULARITE_TYPE_Z_T )

               num_loi = Singularite(ising)%NumeroLoi

               !---------------------------------------------
               ! Interpolation de la cote au temps Temps
               !  a partir de la loi LoiHydrau correspondante
               !---------------------------------------------
               call INTERPOLATION_S(                    &
                   Singularite(ising)%PtZ(1)          , &
                   Temps                              , &
                   ORDRE_INTERPOLATION                , &
                   LoiHydrau(num_loi)%Temps           , &
                   LoiHydrau(num_loi)%Cote            , &
                   size(LoiHydrau(num_loi)%Temps)     , &
                   Erreur                             )
               if( Erreur%Numero /= 0 ) then
                  return
               end if

            case( SINGULARITE_TYPE_VANNE )

               num_loi = Singularite(ising)%NumeroLoi

               !---------------------------------------------------
               ! Interpolation de la cote inferieure au temps Temps
               !  a partir de la loi LoiHydrau correspondante
               !---------------------------------------------------
               call INTERPOLATION_S(                    &
                   Singularite(ising)%PtZInf          , &
                   Temps                              , &
                   ORDRE_INTERPOLATION                , &
                   LoiHydrau(num_loi)%Temps           , &
                   LoiHydrau(num_loi)%CoteInf         , &
                   size(LoiHydrau(num_loi)%Temps)     , &
                   Erreur                             )
               if( Erreur%Numero /= 0 ) then
                  return
               end if

               !---------------------------------------------------
               ! Interpolation de la cote superieure au temps Temps
               !  a partir de la loi LoiHydrau correspondante
               !---------------------------------------------------
               call INTERPOLATION_S(                    &
                   Singularite(ising)%PtZSup          , &
                   Temps                              , &
                   ORDRE_INTERPOLATION                , &
                   LoiHydrau(num_loi)%Temps           , &
                   LoiHydrau(num_loi)%CoteSup         , &
                   size(LoiHydrau(num_loi)%Temps)     , &
                   Erreur                             )
               if( Erreur%Numero /= 0 ) then
                  return
               end if

         end select

      end do

   end if

   !==============================================================
   !           Calcul des lois aux limites au temps Temps
   !==============================================================
   nb_extremite = size(Extremite)

   do iext = 1 , nb_extremite
      ! test du regime torrentiel
      !--------------------------
      num_sect = Connect%NumSectionExtLibre(iext)

      if( Froude(num_sect) >= 0.99_DOUBLE ) then
         regime_torrentiel = .true.
      else
         regime_torrentiel = .false.
      endif

      ! Si Regime torrentiel
      !---------------------
      if( regime_torrentiel ) then
         ! Test sur la compatibilite du regime avec le noyau utilise
         !----------------------------------------------------------
         if( NoyauCalcul == NOYAU_REZODT ) then
            Erreur%Numero = 701
            Erreur%ft     = err_701
            Erreur%ft_c   = err_701c
            call TRAITER_ERREUR( Erreur , iext , Extremite(iext)%Nom )
            return
         endif
      end if

      ! test de la position debut/fin de branche
      !-----------------------------------------
      debut_bief = .false.
      do ibief = 1 , size(Connect%OrigineBief)
         if( num_sect == Connect%OrigineBief(ibief) ) then
            debut_bief = .true.
            exit
         endif
      end do

      ! test amont/aval de la branche
      !------------------------------
      if( ( Q1(num_sect) >= 0._DOUBLE .and. debut_bief)  .or. &
          ( Q1(num_sect) <  0._DOUBLE .and. .not.debut_bief ) ) then
         amont_bief = .true.
      else
         amont_bief = .false.
      endif

      select case( Extremite(iext)%Type )

         !----------------------------------------
         case( CONDITION_TYPE_COTE_DEBIT_IMPOSES )
         !----------------------------------------

            num_loi = Extremite(iext)%NumeroLoi

            call INTERPOLATION_S(                        &
                     Extremite(iext)%PtQ(1)            , &
                     Temps                             , &
                     ORDRE_INTERPOLATION               , &
                     LoiHydrau(num_loi)%Temps          , &
                     LoiHydrau(num_loi)%Debit          , &
                     size(LoiHydrau(num_loi)%Temps)    , &
                     Erreur                            )

            call INTERPOLATION_S(                        &
                     Extremite(iext)%PtZ(1)            , &
                     Temps                             , &
                     ORDRE_INTERPOLATION               , &
                     LoiHydrau(num_loi)%Temps          , &
                     LoiHydrau(num_loi)%COTE           , &
                     size(LoiHydrau(num_loi)%Temps)    , &
                     Erreur                            )
            if( Erreur%Numero /= 0 ) then
               return
            end if

         !----------------------------------
         case( CONDITION_TYPE_DEBIT_IMPOSE )
         !----------------------------------

            num_loi = Extremite(iext)%NumeroLoi

            if( .not. regime_torrentiel .or. (regime_torrentiel) ) then
               ! Interpolation du debit au temps Temps
               ! a partir de la liste des lois LoiHydrau
               !----------------------------------------
               call INTERPOLATION_S(                     &
                     Extremite(iext)%PtQ(1)            , &
                     Temps                             , &
                     ORDRE_INTERPOLATION               , &
                     LoiHydrau(num_loi)%Temps          , &
                     LoiHydrau(num_loi)%Debit          , &
                     size(LoiHydrau(num_loi)%Temps)    , &
                     Erreur                            )
               if( Erreur%Numero /= 0 ) then
                  return
               end if
            else
            ! Sinon, affectation de la valeur 0 a Q
            !--------------------------------------
               Extremite(iext)%PtQ(1) = 0
            endif

            ! Si Regime torrentiel et amont branche
            ! on fournit aussi Z
            !---------------------------------------
            if( LoiHydrau(num_Loi)%type == LOI_TYPE_LIMNHYDROGRAMME ) then
               if( size(LoiHydrau(num_loi)%Cote) == 0 ) then
                  Erreur%Numero = 702
                  Erreur%ft     = err_702
                  Erreur%ft_c   = err_702c
                  call TRAITER_ERREUR( Erreur , Temps , iext , Extremite(iext)%Nom )
                  return
               endif

               call INTERPOLATION_S(                        &
                      Extremite(iext)%PtZ(1)              , &
                      Temps                               , &
                      ORDRE_INTERPOLATION                 , &
                      LoiHydrau(num_loi)%Temps            , &
                      LoiHydrau(num_loi)%Cote             , &
                      size(LoiHydrau(num_loi)%Temps)      , &
                      Erreur                              )
               if( Erreur%Numero /= 0 ) then
                  return
               end if
            else
               ! Sinon, affectation de la valeur 0 a Z
               !--------------------------------------
               Extremite(iext)%PtZ(1) = 0
            endif

         !--------------------------------
         case (CONDITION_TYPE_COTE_IMPOSE)
         !--------------------------------

            num_loi = Extremite(iext)%NumeroLoi

            if( .not. regime_torrentiel .or. (regime_torrentiel ) ) then
               ! Interpolation de Z au temps Temps
               !  a partir de la liste des lois LoiHydrau
               !-----------------------------------------
               call INTERPOLATION_S(                       &
                     Extremite(iext)%PtZ(1)              , &
                     Temps                               , &
                     ORDRE_INTERPOLATION                 , &
                     LoiHydrau(num_loi)%Temps            , &
                     LoiHydrau(num_loi)%Cote             , &
                     size(LoiHydrau(num_loi)%Temps)      , &
                     Erreur                              )
               if( Erreur%Numero /= 0 ) then
                  return
               end if
            else
               ! Sinon, affectation de la valeur 0 a Z
               !--------------------------------------
               !
               Extremite(iext)%PtZ(1) = 0
            endif

            ! Si Regime torrentiel, et amont bief,
            ! on fournit aussi Q
            !-------------------------------------
            if( LoiHydrau(num_Loi)%type == LOI_TYPE_LIMNHYDROGRAMME ) then
               if( size(LoiHydrau(num_loi)%Debit) == 0 ) then
                  Erreur%Numero = 702
                  Erreur%ft     = err_702
                  Erreur%ft_c   = err_702c
                  call TRAITER_ERREUR( Erreur , Temps , iext , Extremite(iext)%Nom )
                  return
               endif

               call INTERPOLATION_S(                      &
                      Extremite(iext)%PtQ(1)            , &
                      Temps                             , &
                      ORDRE_INTERPOLATION               , &
                      LoiHydrau(num_loi)%Temps          , &
                      LoiHydrau(num_loi)%Debit          , &
                      size(LoiHydrau(num_loi)%Temps)    , &
                      Erreur                            )
               if( Erreur%Numero /= 0 ) then
                  return
               end if

            else
               ! Sinon, affectation de la valeur 0 a Q
               !--------------------------------------
               Extremite(iext)%PtQ(1) = 0
            endif

         !------------------------------------
         case( CONDITION_TYPE_COTE_DEBIT,    &
               CONDITION_TYPE_ZAVAL_QAMONT,  &
               CONDITION_TYPE_DEBIT_COTE     )

            !------------------------------------
            ! Les lois sont implementees dans PRETRAIT


         !-----------
         case default
         !-----------

         ! (CONDITION_TYPE_SORTIE_LIBRE, CONDITION_TYPE_NORMALE)

      end select

   end do

   return

end subroutine QCL
