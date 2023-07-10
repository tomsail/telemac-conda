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

module M_SING3_I

   interface

   subroutine  SING3( &
        AS,BS,CS    , & ! Coeff de l'equ discretisee de la singularite
         ZAM,ZAV    , & !
        BAM,VAM     , & ! largeur au miroir et vitesse amont
        CoteCrete   , & ! Cote de crete de la singularite
        DXP,DYP     , & ! Points X et Y de la crete
        CoeffDebit  , & ! Coefficient de debit
    Epaisseur_Seuil , & ! Epaisseur_Seuil
            Erreur    & ! Erreur
                    )

   !***********************************************************************
   ! PROGICIEL : MASCARET      A. LEBOSSE
   !                           P. CHERUBINI
   !                           S. MANDELKERN
   !
   ! VERSION : V8P4R0             EDF-CEREMA
   !***********************************************************************
   !
   !  FONCTION :
   !  --------
   !
   !   CALCUL DES COEFFICIENTS DE L'EQUATION DISCRETISEE D'UN SINGULARITE
   !   DEFINI DE MANIERE STANDARD ( SINGULARITE DE TYPE 3 ) .
   !
   !
   !-----------------------------------------------------------------------
   !
   !   FICHIERS ENTREE/SORTIE :
   !   ----------------------
   !
   !   SOUS PROGRAMME APPELANT :  KSING
   !   -------------------------
   !   SOUS PROGRAMMES APPELES :  ---
   !   -------------------------
   !
   !   COMMENTAIRES :
   !   ------------
   !
   ! . CALCUL DE LA LOI Q(Z) SUR UNE SINGULARITE DONT LA GEOMETRIE DE LA
   !   CRETE EST DONNEE PAR DES POINTS
   !
   ! . Q EST > 0 DE L'INDICE 1 VERS L'INDICE 2
   !   EN REGIME DENOYE , LA LOI STANDARD EST APPLIQUEE
   !   EN REGIME NOYE , LA CORRECTION EST DONNEE PAR LE COEFFICIENT C :
   !
   ! . RH=(ZAVAL-CoteCrete)/(ZAMONT-CoteCrete)
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
   use M_PRECISION       ! Type DOUBLE
   use M_PARAMETRE_C     ! Parametres de calcul
   use M_MESSAGE_C       ! Liste des messages d'erreur
   use M_ERREUR_T        ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   implicit none

   !.. Formal Arguments .. 
   real(DOUBLE)              , intent(  out) :: AS, BS, CS
   real(DOUBLE)              , intent(in   ) :: ZAM, ZAV
   real(DOUBLE)              , intent(in   ) :: BAM, VAM
   real(DOUBLE), dimension(:), intent(in   ) :: DXP, DYP
   real(DOUBLE)              , intent(in   ) :: CoeffDebit
   real(DOUBLE)              , intent(in   ) :: CoteCrete
   Integer                   , intent(in   ) :: Epaisseur_Seuil
   type(ERREUR_T)            , intent(inout) :: Erreur

   end subroutine SING3

   end interface

end module M_SING3_I
