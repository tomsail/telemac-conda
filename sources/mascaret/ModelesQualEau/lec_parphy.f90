!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

subroutine LEC_PARPHY( &
      FICHIER_PARPHY , & ! Fichier des parametres physiques
              PARPHY , & ! Parmetres physiques
              ERREUR )

!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - N.GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_PARAMETRES_QUALITE_EAU_T ! Type Parametres Qualite d'eau_T
   use M_FICHIER_T
   use M_ERREUR_T
   use M_TRAITER_ERREUR_I         ! Inteface generique de gestion des erreurs
   use M_MESSAGE_C                ! Definition des messages d'erreur

   implicit none

   ! Arguments
   type(PARAMETRES_QUALITE_EAU_T) :: ParPhy
   type (Fichier_T)               :: Fichier_Parphy
   type (ERREUR_T)                :: Erreur
   integer                        :: nb_parphy
   !
   !  Variables locales
   !
   Integer i,retour

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LEC_PARPHY'

   !
   ! Ouverture du fichier a lire
   ! ---------------------------
   !
   open( unit = fichier_Parphy%Unite , file = fichier_Parphy%Nom , access = 'SEQUENTIAL' , &
         action = 'READ' , form = 'FORMATTED' , iostat = RETOUR , &
         position = 'rewind' , status = 'OLD' )
   !
   ! Nombre de parametres physiques
   ! ------------------------------
   !
   READ(fichier_Parphy%Unite,*) Nb_parphy

   if( nb_parphy < 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Nombre de parametres physiques' )
      return
   end if

   if( nb_parphy > 0 ) then

      allocate( parphy%ParQual_eau(nb_parphy) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'parametres physiques de qualite d_eau' )
         return
      end if

      do i = 1 , nb_parphy
         read( fichier_ParPhy%Unite,*) parphy%ParQual_eau(i)
      enddo

   endif
   close( unit = fichier_Parphy%Unite )


   ! Fin des traitements
   !--------------------

   !Erreur%Arbredappel = arbredappel_old

   return

end subroutine LEC_PARPHY
