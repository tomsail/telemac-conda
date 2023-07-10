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

module M_LIRE_CHAINE_S
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   contains

   subroutine LIRE_CHAINE_S( &
                        chaine , & ! chaine lue
                       Fichier , & ! Fichier en lecture
            chaine_commentaire , & ! chaine indiquant une ligne commentaire
                        RETOUR )   ! code de retour d'erreur

   !========================================================================
   !  FONCTION : SAUT DES LIGNES COMMENTAIRES IDENTIFIES PAR
   !  --------   LA chaine_commentaire EN DEBUT DE LIGNE
   !
   !------------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :        --
   !   ----------------------
   !
   !   SOUS-PROGRAMME(S) APPELANT(S) : - LEC_HYDRAU
   !   -----------------------------
   !   SOUS-PROGRAMME(S) APPELE(S)   : --
   !   ---------------------------
   !   COMMENTAIRES :
   !   ------------
   !
   !   DOCUMENTATION EXTERNE :
   !   ---------------------
   !
   !***********************************************************************

   ! ========================== Declarations ===============================
   use M_FICHIER_T
   use M_ERREUR_T
   use M_MESSAGE_C
   use M_TRAITER_ERREUR_I

   implicit none

   ! Arguments
   character(*)   , intent(  out) :: chaine
   type(FICHIER_T), intent(in   ) :: Fichier
   character(*)   , intent(in   ) :: chaine_commentaire
   integer        , intent(inout) :: RETOUR
   ! Variables locales
   integer :: ul

   ! =========================== Instructions ===============================

   ! INITIALISATIONS
   !----------------
   ul = Fichier%Unite

   ! TRAITEMENTS
   !------------
   read(ul, '(A)', iostat=RETOUR) chaine

   do while( RETOUR == 0 .and. index(chaine,chaine_commentaire) == 1 )
      read( ul , '(A)' , iostat = RETOUR ) chaine
   end do

   return

   end subroutine LIRE_CHAINE_S

end module M_LIRE_CHAINE_S
