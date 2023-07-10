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

module M_CONTQ_I
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine  CONTQ   ( &
            Connect     , & ! Table de connectivite
            Q           , & ! Debit
            Impression  , & ! Flag d'impression
            UniteListing, & ! Unite logique Fichier listing
            Erreur        & ! Erreur
                        )

!**********************************************************************
!
!   FONCTION : VERIFICATION DE LA CONTINUITE DES DEBITS AUX NOEUDS
!   --------
!
!
! ----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :    UniteListing  : LISTING
!   ----------------------  
!   SOUS PROGRAMMES APPELANTS : REZO
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    ---
!   -------------------------
   !=========================== Declarations ===========================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_CONNECT_T       ! Definition du type CONNECT_T

   implicit none

   !.. Arguments .. 
   real(DOUBLE)   , dimension(:), intent(in   ) :: Q
   type(CONNECT_T)              , intent(in   ) :: Connect
   logical                      , intent(in   ) :: Impression
   integer                      , intent(in   ) :: UniteListing
   type(ERREUR_T)               , intent(inout) :: Erreur

   end subroutine CONTQ

   end interface

end module M_CONTQ_I
