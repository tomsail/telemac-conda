!== Copyright (C) 2000-2017 EDF-CEREMA-ARTELIA ==
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

module M_LIAISON_SIPHON_I
! *********************************************************************
! PROGICIEL : MASCARET        S. DELMAS    C. COULET
!                             
! VERSION : V8P4R0         EDF-CEREMA-ARTELIA
! *********************************************************************
   interface
   
    subroutine  LIAISON_SIPHON      ( &
                AS,BS,CS            , & ! Coeff de l'equation discretisee de la singularite
                ZAM,ZAV             , & !
                ZfAM, ZfAV          , &
                Liaison             , &
                Erreur              & ! Erreur
                                )

! *********************************************************************
! PROGICIEL : MASCARET        
!                             
! VERSION :
! *********************************************************************
!  FONCTION :
!  --------
!   CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UN LIAISON
!   DEFINI DE TYPE SIPHON.
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!   SOUS PROGRAMME APPELANT :  KLIAI
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!   COMMENTAIRES :
!   ------------
!
!------------------------------------------------------------------------

!========================== Declarations ==============================

   use M_PRECISION          ! type DOUBLE
   use M_PARAMETRE_C        ! constantes numeriques 2/3
   use M_LIAISON_CHENAL_I   ! interface du sous-programme CHENAL
   use M_LIAISON_T
   use M_ERREUR_T           ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I   ! Traitement des erreurs

   implicit none

   !.. Formal Arguments ..
   real(DOUBLE)              , intent(  out) :: AS, BS, CS
   real(DOUBLE)              , intent(in   ) :: ZAM, ZAV, ZfAM, ZfAV
   type(LIAISON_T)           , intent(inout) :: Liaison
   type(ERREUR_T)            , intent(inout) :: Erreur
   
   
   end subroutine LIAISON_SIPHON

   end interface

end module M_LIAISON_SIPHON_I