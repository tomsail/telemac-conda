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

module M_ALGOP_I
!***********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE              S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine ALGOP( &
            Algorithme , &
                NbSect , &
            nbExtNoeud , &
               Connect , &
      ImpressionReseau , &
          UniteListing , &
                Erreur  &
                       )

!
!  FONCTION :
!  --------
!                          SARA PERMANENT
!                          >>>>>>><<<<<<<
!         DEFINITION DE L'ALGORITHME DE RESOLUTION :
!         CONSTITUTION DU TABLEAU D'APPELS AUX SOUS-PROGRAMMES
!         RESOLVANT LE CALCUL DE LA LIGNE D'EAU EN REGIME PERMANENT .
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____._______________________________________________
! !    NOM    !TYPE!MODE!                   ROLE
! !___________!____!____!_______________________________________________
! ! IAPPEL    ! I  !<-- ! TABLEAUX DES APPELS AUX S.P. RESOLVANT LE
! !           !    !    ! CALCUL DE LIGNES D'EAU :
! !           !    !    !  IAPPEL = ISUB + I
! !           !    !    !   AVEC  ISUB = 100  ==>  CALL QBIEF
! !           !    !    !         ISUB = 200  ==>  CALL PERMAT
! !           !    !    !         ISUB = 300  ==>  CALL QNODE (1ER PAS)
! !           !    !    !         ISUB = 400  ==>  CALL QNODE (2EM PAS)
! !           !    !    !         ISUB = 500  ==>  CALL QREPAR(1ER PAS)
! !           !    !    !         ISUB = 600  ==>  CALL QREPAR(2EM PAS)
! !           !    !    !     ET   I = LE NUMERO DU NOEUD OU DU BIEF
! ! ical      ! I  ! -- ! ical(L) = +1 SI ON A TRAITE LA LIEME
! !           !    !    ! SECTION EXTREME ( ORIGINE )
! !           !    !    ! ical(L) = -1 SI ON A TRAITE LA LIEME
! !           !    !    ! SECTION EXTREME ( EXTREMITE )
! !           !    !    ! ical(L) =  0 SI C'EST UNE LIMITE LIBRE
! !           !    !    ! ical(L) =  1 SI LA LIEME SECTION EXTREME
! !           !    !    ! EST RELIEE A UN NOEUD
! !___________!____!____!_____________________________________________
!----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!                UniteListing  : IMPRESSION LISTING
!
!   SOUS PROGRAMMES APPELANTS :  PRETRAIT
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    ---
!   -------------------------
!***********************************************************************

   !============================= Declarations ===========================
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_CONNECT_T        ! Definition du type CONNECT_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   ! Arguments
   integer, dimension(:), pointer       :: Algorithme
   integer              , intent(in   ) :: NbSect
   integer, dimension(:), intent(in   ) :: NbExtNoeud
   type(CONNECT_T)      , intent(in   ) :: Connect
   logical              , intent(in   ) :: ImpressionReseau
   integer              , intent(in   ) :: UniteListing
   type(ERREUR_T)       , intent(inout) :: Erreur

   end subroutine ALGOP

   end interface

end module M_ALGOP_I
