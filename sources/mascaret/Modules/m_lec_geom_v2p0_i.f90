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

module M_LEC_GEOM_V2P0_I
!***********************************************************************
! PROGICIEL : MASCARET        P. CHERUBINI
!                             A. LEBOSSE
!                             S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  LEC_GEOM_V2P0     ( &
                              Profil , & ! Resultats
                 FrottParoiVerticale , & ! Donnees non modifiees
                             Fichier , & ! Fichier geometrie
                        UniteListing , & ! Unite logique fichier listing
                              Erreur )

   !***********************************************************************
   !  FONCTION :
   !  --------
   !
   !       LECTURE DES DONNEES GEOMETRIQUES (PROFILS EN TRAVERS)
   !       AU FORMAT LIDO V2P0
   !       CONTROLES
   !-----------------------------------------------------------------------
   !
   !   FICHIERS  ENTREE/SORTIE :
   !   ------------------------
   !
   !                UL             : FICHIER GEOMETRIE
   !                UniteListing   : IMPRESSION DU LISTING
   !
   !   SOUS PROGRAMME APPELANT :  LEC_GEOM
   !   -------------------------
   !   SOUS PROGRAMMES APPELES :  ---
   !   -------------------------
   !
   !   COMMENTAIRES :
   !   --------------
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !***********************************************************************

   !=========================== Declarations ================================
   use M_PRECISION
   use M_PARAMETRE_C ! INFINI
   use M_PROFIL_T    ! Format PROFIL_T
   use M_FICHIER_T   ! Format FICHIER_T
   use M_ERREUR_T    ! Format ERREUR_T
   use M_TRAITER_ERREUR_I
   use M_MESSAGE_C

   !.. Implicit Declarations ..
   implicit none

   !.. Formal Arguments ..
   type(PROFIL_T), dimension(:), pointer       :: Profil   ! profils
   type(FICHIER_T)             , intent(in   ) :: Fichier  ! fichier des profils
   logical                     , intent(in   ) :: FrottParoiVerticale ! test de frottement
   integer                     , intent(in   ) :: UniteListing
   type(ERREUR_T)              , intent(inout) :: Erreur   ! Erreur

   end subroutine LEC_GEOM_V2P0

   end interface

end module M_LEC_GEOM_V2P0_I
