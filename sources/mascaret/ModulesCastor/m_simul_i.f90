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
Module M_SIMUL_I
 
   Interface

   SUBROUTINE SIMUL( &
             indic , &   ! Flag de pilotage
                 n , &   ! Nombre de variables a optimiser
            cfzone , &   ! Variables d'optimisation
                 f , &   ! Valeur de la fonction cout
                 g , &   ! Gradient de la fonction cout
               izs , &   ! Tableaux de travail d'entiers, de reels simples et doubles
               rzs , &
               dzs   &
             )
!
! *********************************************************************
! PROGICIEL : MASCARET         F. DEMANGEON
!                              F. ZAOUI
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
! FONCTION :
! --------
!
! CALCUL DE LA FONCTION COUT ET DU GRADIENT
!
! *********************************************************************
!
! DEFINITION DES VARIABLES
! ------------------------
!.. Modules importes ..
!----------------------
   USE M_PRECISION
   USE M_PARAMETRE_C
   USE M_MESSAGE_C
   USE M_RHSBP_S
   USE M_NUM_BIEF_S
   USE M_FROUDE_S
   USE M_TRAITER_ERREUR_I
   USE M_CQINJ_I
   USE M_DIFF_JCOUT_I
   USE M_COUT_I  
   USE M_XINDIC_S
   USE M_SHARE_VAR
   USE M_STOCK_CALAGE_I
   IMPLICIT NONE

!
!.. Arguments ..
!---------------
!
   INTEGER,INTENT(INOUT) :: indic
   INTEGER,INTENT(IN) :: n                                     
   DOUBLE PRECISION, INTENT(INOUT), DIMENSION(n) :: cfzone
   DOUBLE PRECISION, INTENT(INOUT) :: f                                   
   DOUBLE PRECISION, INTENT(INOUT), DIMENSION(n) :: g       
   INTEGER, INTENT(INOUT) :: izs
   REAL, INTENT(INOUT)  :: rzs
   DOUBLE PRECISION,DIMENSION(size(x)),INTENT(INOUT)  :: dzs

   end subroutine SIMUL

   end interface

   end module M_SIMUL_I


