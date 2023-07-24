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

subroutine LEC_STOCKAGE  ( &
     Profil              , & ! Profils geometriques
     PresenceZoneStockage, & ! Flag d'existence de zones de stockage
     UlLst               , & ! Unite logique fichier listing
     unitNum             , & ! Unite logique .xcas
     Erreur                & ! Erreur
                         )

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!                            F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_XCAS_S

   implicit none

   ! Arguments
   type(PROFIL_T)    , dimension(:)  , intent(inout) :: Profil
   logical                           , intent(  out) :: PresenceZoneStockage
   integer                           , intent(in   ) :: UlLst
   integer, intent(in)                               :: unitNum
   type(ERREUR_T)                    , intent(inout) :: Erreur
   ! Variables locales
   integer, dimension(:), allocatable :: deja_traite  ! numero du profil deja traite
   integer      :: iprof               ! compteur sur les profils
   integer      :: i                   ! compteur sur les profils
   integer      :: ipoint              ! compteur sur les points
   integer      :: nb_point            ! nombre de points
   integer      :: nb_profil_zone_sto
   integer      :: num_profil_sto
   real(DOUBLE) :: limite_maj_gauche
   real(DOUBLE) :: limite_maj_droite
   !character(132) :: !arbredappel_old
   integer      :: retour               ! code de retour d'erreur
                                        ! des fonctions intrinseques
   integer, allocatable :: itab(:)
   real(double), allocatable :: rtab1(:),rtab2(:)
   character(len=256)  :: pathNode
   character(len=8192) :: line

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_STOCKAGE'

   ! ZONES DE STOCKAGE
   !------------------
   if (UlLst >0) write(UlLst,10020)

   ! Nombre de profils avec zones de stockage
   !-----------------------------------------
   pathNode = 'parametresCalage/zoneStockage/nbProfils'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) nb_profil_zone_sto

   if( nb_profil_zone_sto < 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Nombre de profils avec zones de stockage' )
      return
   end if

   if (UlLst >0) write(UlLst,10000) nb_profil_zone_sto

   if( nb_profil_zone_sto > 0 ) then

      PresenceZoneStockage = .true.
      allocate( deja_traite(nb_profil_zone_sto) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'deja_traite' )
         return
      end if

       allocate( itab(nb_profil_zone_sto) , STAT = retour )
       if( retour /= 0 ) then
           Erreur%Numero = 5
           Erreur%ft     = err_5
           Erreur%ft_c   = err_5c
           call TRAITER_ERREUR( Erreur , 'itab' )
           return
       end if
       allocate( rtab1(nb_profil_zone_sto) , STAT = retour )
       if( retour /= 0 ) then
           Erreur%Numero = 5
           Erreur%ft     = err_5
           Erreur%ft_c   = err_5c
           call TRAITER_ERREUR( Erreur , 'rtab1' )
           return
       end if
       allocate( rtab2(nb_profil_zone_sto) , STAT = retour )
       if( retour /= 0 ) then
           Erreur%Numero = 5
           Erreur%ft     = err_5
           Erreur%ft_c   = err_5c
           call TRAITER_ERREUR( Erreur , 'rtab2' )
           return
       end if

      deja_traite(:) = 0

      pathNode = 'parametresCalage/zoneStockage/numProfil'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) itab

      pathNode = 'parametresCalage/zoneStockage/limGauchLitMaj'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab1

      pathNode = 'parametresCalage/zoneStockage/limDroitLitMaj'
      line = xcasReader(unitNum, pathNode)
      read(unit=line, fmt=*) rtab2

      ! Affectation des valeurs
      !------------------------
      do iprof = 1 , nb_profil_zone_sto
         num_profil_sto = itab(iprof)
         if( num_profil_sto <= 0 ) then
            Erreur%Numero = 374
            Erreur%ft     = err_374
            Erreur%ft_c   = err_374c
            call TRAITER_ERREUR( Erreur , 'Numero du profil comportant des zones de stockage' , iprof )
            return
         end if

         ! A t il ete deja traite ?
         if( iprof >= 2 ) then
            do i = 1 , iprof - 1
               if( deja_traite(i) == num_profil_sto ) then
                  Erreur%Numero = 376
                  Erreur%ft     = err_376
                  Erreur%ft_c   = err_376c
                  call TRAITER_ERREUR( Erreur , 'des profils comportant des zones de stockage' , iprof )
                  return
               end if
            end do
         endif

         deja_traite(iprof) = num_profil_sto

         limite_maj_gauche = rtab1(iprof)
         limite_maj_droite = rtab2(iprof)

         if (UlLst >0) write(UlLst,10010) iprof , num_profil_sto , limite_maj_gauche , limite_maj_droite

         nb_point = size(Profil(num_profil_sto)%X(:))

         !---------------------------------------
         ! Controle de coherence des valeurs lues
         !---------------------------------------
         if( limite_maj_gauche < Profil(num_profil_sto)%X(1)        .or. &
             limite_maj_droite > Profil(num_profil_sto)%X(nb_point) .or. &
             limite_maj_gauche >= limite_maj_droite) then
            Erreur%Numero = 307
            Erreur%ft     = err_307
            Erreur%ft_c   = err_307c
            call TRAITER_ERREUR( Erreur , num_profil_sto )
            return
         end if

         do ipoint = 1 , nb_point
            if( Profil(num_profil_sto)%X(ipoint) >= limite_maj_gauche ) then
               Profil(num_profil_sto)%LimiteMaj(1) = ipoint
               exit
            endif
         end do

         do ipoint = 1 , nb_point
            if( Profil(num_profil_sto)%X(ipoint) >= limite_maj_droite ) then
               Profil(num_profil_sto)%LimiteMaj(2) = ipoint
               exit
            endif
         end do

         if( Profil(num_profil_sto)%LimiteMaj(1) ==1 ) then
            Profil(num_profil_sto)%LimiteMaj(1) = 2
         end if

         if( Profil(num_profil_sto)%LimiteMaj(2) == nb_point ) then
            Profil(num_profil_sto)%LimiteMaj(2) = nb_point-1
         end if

         !--------------------------------------------------
         ! Controle de la position des limites du lit majeur
         ! par rapport a celle du lit mineur
         !--------------------------------------------------
         if( Profil(num_profil_sto)%LimiteMaj(1) > Profil(num_profil_sto)%LimiteMin(1) .or. &
             Profil(num_profil_sto)%LimiteMaj(2) < Profil(num_profil_sto)%LimiteMin(2)) then
            Erreur%Numero = 378
            Erreur%ft     = err_378
            Erreur%ft_c   = err_378c
            call TRAITER_ERREUR( Erreur , num_profil_sto )
            return
         endif
      end do

      deallocate( deja_traite , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 6
         Erreur%ft     = err_6
         Erreur%ft_c   = err_6c
         call TRAITER_ERREUR( Erreur , 'deja_traite' )
         return
      end if

       deallocate(itab)
       deallocate(rtab1)
       deallocate(rtab2)

   endif

   !Erreur%arbredappel = !arbredappel_old

   return

   10020 format (/,'ZONES DE STOCKAGE',/, &
                &  '-----------------',/)
   10000 format ('Nombre de profil avec zones de stockage : ',i3,/)
   10010 format (i3,' Profil n0 ',i3,' Limite majeur gauche : ',f12.3,' droite : ',f12.3)

end subroutine LEC_STOCKAGE
