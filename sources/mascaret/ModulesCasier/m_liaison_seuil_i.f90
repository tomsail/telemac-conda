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

module M_LIAISON_SEUIL_I

   interface

  subroutine  LIAISON_SEUIL       ( &
            AS,BS,CS    , & ! Coeff de l'equation discretisee de la singularite
            ZAM,ZAV     , & !
            Liaison     , &
            Erreur        & ! Erreur
                        )

! *********************************************************************
! PROGICIEL : MASCARET        S. DELMAS    C. COULET
!                             
! VERSION : V8P4R0         EDF-CEREMA-ARTELIA
! *********************************************************************
!  FONCTION :
!  --------
!
!   CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UN LIAISON
!   DEFINI DE TYPE SEUIL.
!
!
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS PROGRAMME APPELANT :  KLIAI
!   -------------------------
!   SOUS PROGRAMMES APPELES :  ---
!   -------------------------
!
!   COMMENTAIRES :
!   ------------
!
!
! . Q EST > 0 DE L'INDICE 1 VERS L'INDICE 2
!   EN REGIME DENOYE , LA LOI STANDARD EST APPLIQUEE
!   EN REGIME NOYE , LA CORRECTION EST DONNEE PAR LE COEFFICIENT C :
!
! . RH=(ZAVAL-CoteSeuil)/(ZAMONT-CoteSeuil)
!      ---          RH < 0.8   C= +1
!      ---   0.8  < RH < 1.0   C=  C1*RH**3 + C2*RH**2 + C3 *RH + C4
!
! . AS,BS ET CS SONT LES COEFFICIENTS DE L'EQUATION DISCRETISEE :
!   Q = AS*DZAMONT + BS*DZAVAL + CS
!   DZAMONT REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
!   DZAMONT REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
!   DZAVAL  REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
!   DZAVAL  REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
!
!------------------------------------------------------------------------

   !============================ Declarations ==============================
   use M_PRECISION        ! Type DOUBLE
   use M_PARAMETRE_C      ! Parametres de calcul
   use M_MESSAGE_C        ! Liste des messages d'erreur
   use M_LIAISON_T
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Formal Arguments ..
   real(DOUBLE)              , intent(  out) :: AS, BS, CS
   real(DOUBLE)              , intent(in   ) :: ZAM, ZAV
   type(LIAISON_T)           , intent(inout) :: Liaison
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine LIAISON_SEUIL

   end interface

end module M_LIAISON_SEUIL_I
