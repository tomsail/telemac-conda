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

subroutine LEC_PLANIM    ( &
     Profil              , & ! Profils geometriques
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
   integer                           , intent(in   ) :: UlLst
   integer, intent(in)                               :: unitNum
   type(ERREUR_T)                    , intent(inout) :: Erreur
   ! Variables locales
   integer      :: iprof               ! compteur sur les profils
   integer      :: izone               ! compteur sur les zones
   integer      :: nb_prof             ! nombre de profils
   integer      :: nb_pas_planim
   integer      :: nb_zone_planim
   real(DOUBLE) :: pas_planim
   integer      :: profdeb_zone_planim
   integer      :: proffin_zone_planim
   integer      :: branche_zone_prec
   integer      :: proffin_zone_planim_prec
   integer      :: branche_zone
   real(double), allocatable :: rtab(:)
   integer, allocatable :: itabdeb(:),itabfin(:)
   integer :: retour              ! code de retour des fonctions intrinseques
   character(len=256)  :: pathNode
   character(len=8192) :: line
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_PLANIM'

   if (UlLst >0) write(UlLst,10000)

   ! Nombre de pas de planimetrage
   !------------------------------
   pathNode = 'parametresPlanimetrageMaillage/planim/nbPas'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) nb_pas_planim

   if( nb_pas_planim <= 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Nombre de pas de planimetrage' )
      return
   end if

   if (UlLst >0) write(UlLst,10010) nb_pas_planim

   Profil(:)%NbPas = nb_pas_planim

   ! Nombre de zones de planimetrage
   !--------------------------------
   pathNode = 'parametresPlanimetrageMaillage/planim/nbZones'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) nb_zone_planim

   if( nb_zone_planim <= 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Nombre de zone de planimetrage' )
      return
   end if

   if (UlLst >0) write(UlLst,10020) nb_zone_planim

   ! Affectation du pas aux profils
   branche_zone_prec        = 0
   proffin_zone_planim_prec = 0

   allocate( rtab(nb_zone_planim) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab' )
       return
   end if
   allocate( itabdeb(nb_zone_planim) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itabdeb' )
       return
   end if
   allocate( itabfin(nb_zone_planim) , STAT = retour )
   if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itabfin' )
       return
   end if

   pathNode = 'parametresPlanimetrageMaillage/planim/valeursPas'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) rtab

   pathNode = 'parametresPlanimetrageMaillage/planim/num1erProf'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) itabdeb

   pathNode = 'parametresPlanimetrageMaillage/planim/numDerProf'
   line = xcasReader(unitNum, pathNode)
   read(unit=line, fmt=*) itabfin

   do izone = 1 , nb_zone_planim

      pas_planim = rtab(izone)
      if( pas_planim <= 0 ) then
         Erreur%Numero = 374
         Erreur%ft     = err_374
         Erreur%ft_c   = err_374c
         call TRAITER_ERREUR( Erreur , 'du pas de planimetrage' , izone )
         return
      end if

      profdeb_zone_planim = itabdeb(izone)
      if( profdeb_zone_planim <= 0 ) then
         Erreur%Numero = 374
         Erreur%ft     = err_374
         Erreur%ft_c   = err_374c
         call TRAITER_ERREUR( Erreur , 'du profil de debut d''une zone de planimetrage' , izone )
         return
      end if

      proffin_zone_planim = itabfin(izone)
      if( proffin_zone_planim <= 0 ) then
         Erreur%Numero = 374
         Erreur%ft     = err_374
         Erreur%ft_c   = err_374c
         call TRAITER_ERREUR( Erreur , 'du profil de fin de zone d''une planimetrage' , izone )
         return
      end if

      if( proffin_zone_planim <= profdeb_zone_planim ) then
         Erreur%Numero = 314
         Erreur%ft     = err_314
         Erreur%ft_c   = err_314c
         call TRAITER_ERREUR( Erreur , 'des zones de planimetrage' , izone )
         return
      end if

      do iprof = profdeb_zone_planim , proffin_zone_planim
         Profil(iprof)%Pas = pas_planim
      end do

      ! Controle du non chevauchement des zones
      !----------------------------------------
      branche_zone = Profil(profdeb_zone_planim)%NumBief
      if( branche_zone == branche_zone_prec .and. profdeb_zone_planim /= proffin_zone_planim_prec + 1 ) then
         Erreur%Numero = 375
         Erreur%ft     = err_375
         Erreur%ft_c   = err_375c
         call TRAITER_ERREUR( Erreur , 'des zones de planimetrage' , izone - 1 , izone )
         return
      endif

      if (UlLst >0) write(UlLst,10030) izone , profdeb_zone_planim , proffin_zone_planim , pas_planim

      branche_zone_prec        = branche_zone
      proffin_zone_planim_prec = proffin_zone_planim

   end do

   nb_prof = size(Profil)
   if( proffin_zone_planim /= nb_prof ) then
      Erreur%Numero = 381
      Erreur%ft     = err_381
      Erreur%ft_c   = err_381c
      call TRAITER_ERREUR( Erreur , proffin_zone_planim , nb_prof )
      return
   endif

   !Erreur%arbredappel = !arbredappel_old

   deallocate(rtab)
   deallocate(itabdeb)
   deallocate(itabfin)

   return

   ! Formats
   10000 format (/,'PLANIMETRAGE',/, &
                &  '------------',/)
   10010 format ('Nombre de pas de planimetrage   : ',i3)
   10020 format ('Nombre de zones de planimetrage : ',i3,/)
   10030 format ('Zone : ',i3,' Profil debut : ',i5,' Profil fin : ',i5,' Pas : ',f12.3)

end subroutine LEC_PLANIM
