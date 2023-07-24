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

subroutine LEC_ZONE_SECHE ( &
     ZoneSeche            , & ! Zones seches
     Connect              , & ! Connectivite du reseau
     X                    , & ! Maillage
     Profil               , & ! Profils geometriques
     ProfDebBief          , & ! Premiers profils des biefs
     ProfFinBief          , & ! Derniers profils des biefs
     AbscRelExtDebBief    , &
     AbscRelExtFinBief    , &
     UniteListing         , & ! Unite logique fichier listing
     unitNum              , & ! Unite logique .xcas
     Erreur                 & ! Erreur
                          )

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!                            F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_CONNECT_T           ! Type CONNECT_T
   use M_ERREUR_T            ! Type ERREUR_T
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_ZONE_SECHE_T        ! Type ZONE_SECHE_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_XINDIC_S            ! Calcul de l'indice de section de calcul
   use M_XCAS_S

   implicit none

   ! Arguments
   type(ZONE_SECHE_T), dimension(:)  , pointer       :: ZoneSeche
   type(CONNECT_T)                   , intent(in   ) :: Connect
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: X
   type(PROFIL_T)    , dimension(:)  , intent(in   ) :: Profil
   integer           , dimension(:)  , intent(in   ) :: ProfDebBief
   integer           , dimension(:)  , intent(in   ) :: ProfFinBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtDebBief
   real(DOUBLE)      , dimension(:)  , intent(in   ) :: AbscRelExtFinBief
   integer                           , intent(in   ) :: UniteListing
   integer, intent(in)                               :: unitNum
   type(ERREUR_T)                    , intent(inout) :: Erreur
   ! Variables locales
   integer      :: nb_zone_seche    ! nombre de zones seches
   integer      :: num_branche      ! Numero de la branche
   real(DOUBLE) :: abs_rel_deb      !
   real(DOUBLE) :: abs_rel_fin      !
   real(DOUBLE) :: abs_abs_deb      !
   real(DOUBLE) :: abs_abs_fin      !
   integer      :: num_branche_prec ! numero de branche
   real(DOUBLE) :: abs_rel_fin_prec ! abscisse fin de zone
   integer      :: izone         ! compteur sur les zones
   integer      :: retour        ! code de retour des fonctions intrinseques
   integer, allocatable :: itab(:)
   real(double), allocatable :: rtab1(:),rtab2(:)
   character(len=256)  :: pathNode
   character(len=8192) :: line
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   num_branche_prec = -1
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_ZONE_SECHE'

   if (UniteListing >0) write(UniteListing,10000)

   ! Nombre de zones seches
   pathNode = 'parametresConditionsInitiales/zonesSeches'
   line = xcasReader(unitNum, pathNode)
   if(len(trim(line)).eq.0) then
       nb_zone_seche = 0
   else
       pathNode = 'parametresConditionsInitiales/zonesSeches/nb'
       line = xcasReader(unitNum, pathNode)
       read(unit=line, fmt=*) nb_zone_seche

       if( nb_zone_seche < 0 ) then
          Erreur%Numero = 306
          Erreur%ft     = err_306
          Erreur%ft_c   = err_306c
          call TRAITER_ERREUR( Erreur , 'Nombre de zones seches' )
          return
       end if
   endif

   if (UniteListing >0) write(UniteListing,10010) nb_zone_seche

   if( nb_zone_seche > 0 ) then

      if(.not.associated(ZoneSeche)) allocate( ZoneSeche(nb_zone_seche) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'ZoneSeche' )
         return
      end if

       allocate( itab(nb_zone_seche) , STAT = retour )
       if( retour /= 0 ) then
           Erreur%Numero = 5
           Erreur%ft     = err_5
           Erreur%ft_c   = err_5c
           call TRAITER_ERREUR( Erreur , 'itab' )
           return
       end if
       allocate( rtab1(nb_zone_seche) , STAT = retour )
       if( retour /= 0 ) then
           Erreur%Numero = 5
           Erreur%ft     = err_5
           Erreur%ft_c   = err_5c
           call TRAITER_ERREUR( Erreur , 'rtab1' )
           return
       end if
       allocate( rtab2(nb_zone_seche) , STAT = retour )
       if( retour /= 0 ) then
           Erreur%Numero = 5
           Erreur%ft     = err_5
           Erreur%ft_c   = err_5c
           call TRAITER_ERREUR( Erreur , 'rtab2' )
           return
       end if

      pathNode = 'parametresConditionsInitiales/zonesSeches/branche'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) itab

      pathNode = 'parametresConditionsInitiales/zonesSeches/absDebut'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab1

      pathNode = 'parametresConditionsInitiales/zonesSeches/absFin'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab2

      ! Description des zones seches
      do izone = 1 , nb_zone_seche
         num_branche = itab(izone)
         if( num_branche < 0 .or. num_branche > size(Connect%OrigineBief) ) then
            Erreur%Numero = 332
            Erreur%ft     = err_332
            Erreur%ft_c   = err_332c
            call TRAITER_ERREUR( Erreur , 'zones seches' , num_branche , izone )
            return
         end if

         abs_rel_deb = rtab1(izone)
         if( abs_rel_deb < AbscRelExtDebBief(num_branche) .or. &
             abs_rel_deb > AbscRelExtFinBief(num_branche) ) then
            Erreur%Numero = 341
            Erreur%ft     = err_341
            Erreur%ft_c   = err_341c
            call TRAITER_ERREUR( Erreur ,'debut' , izone , num_branche )
            return
         endif

         abs_rel_fin = rtab2(izone)
         if( abs_rel_fin < AbscRelExtDebBief(num_branche) .or. &
             abs_rel_fin > AbscRelExtFinBief(num_branche) ) then
            Erreur%Numero = 341
            Erreur%ft     = err_341
            Erreur%ft_c   = err_341c
            call TRAITER_ERREUR( Erreur ,'debut' , izone , num_branche )
            return
         endif

         if( abs_rel_fin < abs_rel_deb ) then
            Erreur%Numero = 314
            Erreur%ft     = err_314
            Erreur%ft_c   = err_314c
            call TRAITER_ERREUR( Erreur , 'des zones seches' , izone )
            return
         end if

         ! Controle du chevauchement des zones
         !------------------------------------
         if( num_branche == num_branche_prec .and. &
             abs_rel_deb <= abs_rel_fin_prec ) then
            Erreur%Numero = 375
            Erreur%ft     = err_375
            Erreur%ft_c   = err_375c
            call TRAITER_ERREUR( Erreur , 'des zones seches' , izone - 1 , izone )
            return
         endif

         if (UniteListing >0) write(UniteListing,10020) izone , num_branche , abs_rel_deb , abs_rel_fin

         ! Passage en abscisses absolues
         abs_abs_deb = ABS_ABS_S     ( &
             num_branche             , &
             abs_rel_deb             , &
             Profil                  , &
             ProfDebBief             , &
             ProfFinBief             , &
             Erreur                    &
                                   )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         abs_abs_fin = ABS_ABS_S     ( &
             num_branche             , &
             abs_rel_fin             , &
             Profil                  , &
             ProfDebBief             , &
             ProfFinBief             , &
             Erreur                    &
                                   )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         !------------------------------
         ! calcul des indices de section
         ! correspondant aux abscisses
         !------------------------------
         call XINDIC_S( ZoneSeche(izone)%SectionDeb , abs_abs_deb , X , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         call XINDIC_S( ZoneSeche(izone)%SectionFin , abs_abs_fin , X , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         num_branche_prec = num_branche
         abs_rel_fin_prec = abs_rel_fin

      end do

      deallocate(itab)
      deallocate(rtab1)
      deallocate(rtab2)

   else

      if(.not.associated(ZoneSeche)) allocate( ZoneSeche(0) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'ZoneSeche' )
         return
      end if
   endif  ! Zone seche

   !Erreur%arbredappel = !arbredappel_old

   return

   ! Formats
   10000 format (/,'ZONES SECHES',/, &
                &  '------------',/)
   10010 format ('Nombre de zones seches : ',i3,/)
   10020 format ('Zone n0',i3,' Branche n0 ',i3,' Abscisse debut : ',f12.3,' Abscisse fin : ',f12.3)

end subroutine LEC_ZONE_SECHE
