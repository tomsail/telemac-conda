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

module M_COUT_I
!***********************************************************************
! PROGICIEL : MASCARET       A. LEBOSSE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   SUBROUTINE COUT( &
                    ! /RESULTATS/
                FCOUT , &
                    ! /DONNEES NON MODIFIEES/
                 NMES , Z_MESU , Z_CAL , POND )

! *********************************************************************
! FONCTION :
! --------
!
! CALCUL DE LA FONCTION DE COUT
!
! *********************************************************************
!
! DEFINITION DES VARIABLES
! ------------------------
!
! .________________.____.______________________________________________
! ! NOM       !TYPE!MODE! ROLE
! !___________!____!____!______________________________________________
! !___________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!
! ARGUMENTS
!
! .________________.____.______________________________________________
! ! FCOUT     ! R  !<-- ! VALEUR DE LA FONCTION DE COUT
! ! NMES      ! I  ! -->! NOMBRE DE MESURES (EN ESPACE PLUS EN TEMPS)
! ! ZMESU     ! R  ! -->! COTES MESUREES pour une crue donnee
! ! ZCAL      ! R  ! -->! COTES CALCULEES AUX POINTS DE MESURE
! ! POND      ! R  ! -->! PONDERATIONS DES COTES MESUREES
! !___________!____!____!______________________________________________
!
! VARIABLES EN COMMON
!
! VARIABLES LOCALES
!
! .________________.____.______________________________________________
! !           !    !    !
! !___________!____!____!______________________________________________
!
! *********************************************************************
!
! FICHIERS ENTREE/SORTIE :
! ----------------------
!
! SOUS-PROGRAMMES APPELANTS : DIRECT   GRAD
! -------------------------
!
! SOUS-PROGRAMMES APPELES :
! -----------------------
!
! COMMENTAIRES : LA FONCTION DE COUT EST EGALE A LA RACINE CARREE DE
! ------------   DE LA VALEUR MOYENNE DES CARRES DES ECARTS ENTRE
!                COTES MESUREES ET COTES CALCULEES
!                EN FAIT , LE GRADIENT CALCULE EST CELUI DE LA SOMME
!                DES CARRES DES ECARTS , MAIS LA VALEUR MOYENNE EST
!                PLUS INTERESSANTE AU NIVEAU DES RESULTATS IMPRIMES
!**********************************************************************

   !============================= Declarations ===========================
   !
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   !
   ! Constantes nommees
   !
   use M_PARAMETRE_C       ! GPES
   use M_MESSAGE_C         ! Messages d'erreur
   !
   Implicit none
   !
   real(DOUBLE) , dimension(:)   , intent(in)   :: Z_mesu,Z_Cal    ! cotes mesurees et ponderation pour chaque mesure
   real(DOUBLE) , dimension(:)   , intent(in)   :: POND
   real (DOUBLE)                 , intent(  out):: FCOUT
   Integer                       , intent(in   ):: Nmes

   end subroutine COUT

   end interface

end module M_COUT_I

