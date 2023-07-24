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

SUBROUTINE STRICK()

! **********************************************************************
!  PROGICIEL : MASCARET           S.MANDELKERN
!
!  VERSION : V8P4R0            EDF-CEREMA
! **********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   USE M_SHARE_VAR           ! Variables globales

   implicit none

   ! Arguments

   ! Variables locales
   integer      :: izone              ! compteur sur les zones de frottement
   integer      :: isect              ! compteur sur les sections
   integer      :: indice             ! indice de debut de zone
   integer      :: indice2            ! indice de fin de zone
                                      ! intrinseques
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================

   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>STRICK'

   do izone = 1 , nb_zone_frottement
      !------------------------------
      ! calcul des indices de section
      ! correspondant aux abscisses
      !------------------------------
      call XINDIC_S( indice , calage_frott(izone)%abscdeb_zone_frott , X , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      call XINDIC_S( indice2 , calage_frott(izone)%abscfin_zone_frott , X , Erreur )
      if( Erreur%Numero /= 0 ) then
         return
      endif

      do isect = indice , indice2
         CF1(isect) = calage_frott(izone)%valeur_coeff_min
         CF2(isect) = calage_frott(izone)%valeur_coeff_maj
      end do
   end do

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine STRICK
