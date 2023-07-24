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

module M_OPT2FORT_I
!***********************************************************************
! PROGICIEL : MASCARET      S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine OPT2FORT( &
              chaine_fort , &   ! Resultat
               chaine_opt , &   ! Donnee
                   Erreur )     ! Variables de controle d'erreur

   ! .....................................................................
   !  FONCTION : TRANSFORMATION DES POINTS VIRGULES D UNE CHAINE
   !  --------   DE CARACTERES EN CARACTERE BLANC
   !
   !
   !   FICHIERS ENTREE/SORTIE :       ---
   !   ----------------------
   !
   !   SOUS-PROGRAMME(S) APPELANT(S) :
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S)   : ---
   !   ---------------------------
   !   COMMENTAIRES :
   !   ------------
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !
   !***********************************************************************

   !=============================== Declarations ==========================
   use M_ERREUR_T             ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I     ! Interface generique de gestion des erreurs
   use M_MESSAGE_C            ! Messages d'erreur

   implicit none

   ! Arguments
   character(*)              , intent(in   ) :: chaine_opt  ! donnee
   character(*)              , intent(  out) :: chaine_fort ! resultat
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine OPT2FORT

   end interface

end module M_OPT2FORT_I
