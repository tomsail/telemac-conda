!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_HYP1FA_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   interface

   subroutine HYP1FA( &
                      ! RESULTATS
               FNP1 , &   ! RESULTAT APRES CONVECTION
                      ! DONNEES NON MODIFIEES
                 FN , &   ! TABLEAU DES VALEURS A CONVECTER
                VIT , &   ! TABLEAU DES VITESSES
                  X , &   ! TABLEAU DES COORDONNEES
                 DT , &   ! PAS DE TEMPS
               NOPT , &   ! OPTION POUR L'EQUATION A RESOUDRE :
                             !               0:FORME CONSERVATIVE
                             !               1:FORME NON CONSERVATIVE
                 IM , &   ! DIMENSION DES TABLEAUX
                 CG , &   ! CONDITONS LIMITES A TN+1 A GAUCHE
                 CD , &   ! CONDITONS LIMITES A TN+1 A DROITE
               PRAD , &   ! PRAD(I) PENTE DU RADIER ENTRE I ET I+1
               FROT , &   ! FROT(I) COEFFICIENT DE FROTTEMENT EN I
             ERREUR )  ! Canal de sortie pour fichier d impression

   use  M_PRECISION
   use  M_PARAMETRE_C
   use  M_ERREUR_T
   !
   !.. Implicit Declarations .. 
   implicit none
   !
   !.. Formal Arguments ..
   !
   Type (ERREUR_T)            ,intent(inout)       :: ERREUR
   ! DONNEES NON MODIFIES
   integer, intent(in)                             :: NOPT
   integer, intent(in)                             :: IM
   real (DOUBLE), intent(in)                       :: DT
   real (DOUBLE), dimension(:), intent(in)         :: FROT
   real (DOUBLE), dimension(:), intent(in)         :: PRAD
   real (DOUBLE), dimension(:), intent(in)         :: X
   real (DOUBLE), dimension(:), intent(in)         :: VIT
   real (DOUBLE), intent(in)                       :: CD
   real (DOUBLE), intent(in)                       :: CG
   real (DOUBLE), dimension(:), intent(in)         :: FN
   ! RESULTATS
   real (DOUBLE), dimension(:), intent(inout)      :: FNP1

   end subroutine HYP1FA

   end interface

end module M_HYP1FA_I
