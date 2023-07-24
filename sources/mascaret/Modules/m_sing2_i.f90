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

module M_SING2_I
!***********************************************************************
! PROGICIEL : MASCARET      A. LEBOSSE
!                           P. CHERUBINI
!                           S. PERON
!                           S. MANDELKERN
!
! VERSION : V8P4R0             EDF-CEREMA
!***********************************************************************
   interface

   subroutine  SING2       ( &
               AS,BS,CS    , & ! Coeff de l'equation discretisee de la singularite
               ZAM,ZAV     , & ! Cotes amont et aval
               CoteCrete   , & ! cote de crete de la singularite
               NbQRef      , & ! Nombre de points definissant la loi en regime denoye
               ZAMONT      , & ! points Z de la loi
               QAMONT      , & ! Points Q de la loi
               Erreur        & ! Erreur
                           )

   ! **********************************************************************
   !   FONCTION :
   !   --------
   !   CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UNE SINGULARITE
   !   EN UTILISANT LA LOI EN REGIME DENOYE (SINGULARITE DE TYPE 2) .
   !
   !   Q =AS*DHAMONT + BS*DHAVAL + CS
   !
   ! ----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !
   !   SOUS PROGRAMME APPELANT :  - KSING
   !   -------------------------
   !   SOUS PROGRAMMES APPELES :  - INTERPOLATION_S : INTERPOLATION DE LAGRANGE D'ORDRE N
   !   -------------------------
   !
   !   COMMENTAIRES :
   !   ------------
   !
   ! . LA LOI EN REGIME DENOYE EST DONNEE PAR LES TABLEAUX ZAMONT,QAMONT
   !
   ! . Q EST > 0 DE L'INDICE 1 VERS L'INDICE 2
   !   EN REGIME NOYE , LA CORRECTION EST DONNEE PAR LE COEFFICIENT C :
   !
   ! . RH=(HAVAL-CoteCrete)/(HAMONT-CoteCrete)
   !      ---          RH < 0.8   C= +1
   !      ---   0.8  < RH < 1.0   C= C1*RH**3 + C2*RH**2 + C3*RH + C4
   !
   ! . AS,BS ET CS SONT LES COEFFICIENTS DE L'EQUATION DISCRETISEE :
   !   Q = AS*DHAMONT + BS*DHAVAL + CS
   !   DHAMONT REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
   !   DHAMONT REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
   !   DHAVAL  REPRESENTE DZ2 SI L'ECOULEMENT A LIEU DANS LE SENS 1->2
   !   DHAVAL  REPRESENTE DZ1 SI L'ECOULEMENT A LIEU DANS LE SENS 2->1
   !
   !------------------------------------------------------------------------

   !============================ Declarations ==============================
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_INTERPOLATION_S ! Interpolation

   implicit none

   !.. Formal Arguments ..
   real(DOUBLE)              , intent(  out) :: AS, BS, CS
   type(ERREUR_T)            , intent(inout) :: Erreur
   real(DOUBLE)              , intent(in   ) :: ZAM, ZAV
   real(DOUBLE)              , intent(in   ) :: CoteCrete
   integer                   , intent(in   ) :: NbQRef
   real(DOUBLE), dimension(:), intent(in   ) :: ZAMONT, QAMONT

   end subroutine SING2

   end interface

end module M_SING2_I
