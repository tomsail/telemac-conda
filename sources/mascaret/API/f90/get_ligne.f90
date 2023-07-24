!== Copyright (C) 2000-2018 EDF-CEREMA ==
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

! *********************************************************************
! PROGICIEL : MASCARET       J.-M. LACOMBE
!
! VERSION : V8P4R0              EDF-CEREMA
! *********************************************************************
   !.................................................................................................................................
   ! Renvoie la ligne d'eau (debit, cote) du modele a l'instant courant
   ! .................................................................................................................................
subroutine  GET_LIGNE(Erreur, Identifiant, Q, Z)
   use M_APIMASCARET_STATIC
   use M_MODELE_MASCARET_T   ! Type MODELE_MASCARET_T
   use M_ETAT_MASCARET_T     ! Type ETAT_MASCARET_T
   use M_CONSTANTES_CALCUL_C

   implicit none

   integer, intent(out)                        :: Erreur         ! different de 0 si erreur
   integer                                     :: Identifiant    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   real(8), dimension(*), intent(out)          :: Q              ! Tableau des debits
   real(8), dimension(*), intent(out)          :: Z              ! Tableau des cotes


   !-- Scalaires locaux
   type(MODELE_MASCARET_T)  :: Model
   type(ETAT_MASCARET_T)    :: Etat
   integer :: nb_sect

   Erreur = TEST_INIT_AND_ID(Identifiant, 'GET_LIGNE')
   if (Erreur > 0 ) then
      RETURN
   end if
   Model = ptrTabMascaret(Identifiant)%ModeleMascaret
   Etat  = ptrTabMascaret(Identifiant)%EtatMascaret

! LIGNE D'EAU
!------------
   nb_sect = size(Model%X)
   if(ASSOCIATED(Etat%Q)) Q(1:nb_sect) = Etat%Q(1:nb_sect)
   if(ASSOCIATED(Etat%Z)) Z(1:nb_sect) = Etat%Z(1:nb_sect)
   return

end subroutine GET_LIGNE



