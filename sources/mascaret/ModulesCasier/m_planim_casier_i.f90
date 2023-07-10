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

module M_PLANIM_CASIER_I
!***********************************************************************
! PROGICIEL : MASCARET       C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine PLANIM_CASIER( Casier , Icasier , Option , Erreur )
   ! ******************************************************************
   ! LECTURE DE LA VARIABLE CASIER
   ! ******************************************************************
   !
   !   FICHIERS ENTREE/SORTIE :  --
   !   ----------------------
   !   SOUS PROGRAMMES APPELANTS : - GEO_CASIER
   !   ---------------------------
   !   SOUS PROGRAMMES APPELES :    --
   !   -------------------------

   !========================== Declarations ==============================
   use M_PRECISION
   use M_CASIER_T            ! type Casier
   use M_ERREUR_T            ! type Erreur
   use M_MESSAGE_CASIER_C    ! messages d erreur propres a Casier
   use M_CONSTANTES_CASIER_C ! constantes propres a casier
   use M_PARAMETRE_C         ! constantes numeriques
   use M_TRAITER_ERREUR_CASIER_I    ! traitement des erreurs

   implicit none

   !.. Arguments ..
   type(CASIER_T), intent(inout) :: Casier
   type(ERREUR_T), intent(inout) :: Erreur
   integer, intent(in   ) :: Option
   integer, intent(in   ) :: Icasier
   !.. Variables locales ..
   integer :: retour          ! code de retour des fonctions intrinseques
   integer :: nb_point_interieur,&     ! nombre de points interieurs au Casier
              nb_point_frontiere,&     ! nombre de points frontiere du casier 
              iinterieur,&             ! compteur sur le nombre de points interieurs
              compteur,&               ! compteur quelconque
              stock,&                  ! variable de stockage
              icote,&                  ! compteur sur le nombre de cotes de planimetrage
              NbPoint                  ! argument de la fonction AIRE
   integer, dimension(:), allocatable :: rangement_cote ! rangement des cotes par ordre croissant
   !character(132) :: !arbredappel_old
   real(DOUBLE) :: surface_max       ,&  ! surface maximale du casier
                   surface           ,&  ! variable surface de stockage
                   volume            ,&  ! variable volume de stockage
                   Zcourant          ,&  ! cote du pas courant
                   compteur_precedent,&  ! reel correspondant au compteur - 1
                   compteur_actuel   ,&  ! reel correspondant au compteur
                   Z1                ,&  ! cote du point interieur au rang actuel - 1
                   Z2                ,&  ! cote du point interieur au rang actuel
                   alpha             ,&
                   surface1          ,&
                   surface2

   end subroutine PLANIM_CASIER

   end interface

end module M_PLANIM_CASIER_I
