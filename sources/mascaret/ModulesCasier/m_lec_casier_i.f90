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

module M_LEC_CASIER_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSEE    C. RISSOAN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine LEC_CASIER( &
                     Casier , & ! tableau des casiers
          FichierGeomCasier , &
                   unitNum  , & ! unite logique du fichier .xcas
                     Erreur )   ! erreur

   ! ******************************************************************
   ! LECTURE DE LA VARIABLE CASIER
   ! ******************************************************************
   !
   !   FICHIERS ENTREE/SORTIE :  --
   !   ----------------------
   !   SOUS PROGRAMMES APPELANTS : - PRETRAIT
   !   ---------------------------
   !   SOUS PROGRAMMES APPELES :    - LEC_GEOM_CASIER1, LEC_GEOM_CASIER2
   !   -------------------------

   !========================== Declarations ==============================
   use M_CASIER_T  ! type Casier
   use M_ERREUR_T  ! type Erreur
   use M_PARAMETRE_C
   use M_FICHIER_T
   use M_MESSAGE_CASIER_C  ! messages d erreur propres a Casier
   use M_CONSTANTES_CASIER_C
   use M_TRAITER_ERREUR_CASIER_I  ! traitement des erreurs
   use M_TRAITER_ERREUR_I         ! Traitement de l'errreur
   use M_LEC_GEOM_CASIER2_I
   use M_LEC_GEOM_CASIER1_I

   implicit none

   !.. Arguments ..
   type(CASIER_T), dimension(:), pointer       :: Casier
   type(ERREUR_T)              , intent(inout) :: Erreur
   type(FICHIER_T)             , intent(inout) :: FichierGeomCasier
   integer, intent(in)                         :: unitNum

   end subroutine LEC_CASIER

   end interface

end module M_LEC_CASIER_I
