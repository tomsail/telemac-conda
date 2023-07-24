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

MODULE M_CALAGE_N2QN1_I
!***********************************************************************
! PROGICIEL : MASCARET        F. DEMANGEON
!                             F. ZAOUI
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
  INTERFACE 

  SUBROUTINE CALAGE_N2QN1( &
                        z , &   ! Cote de la surface libre
                       q1 , &   ! Debit mineur
                       q2 , &   ! Debit majeur
                   qinjec , &   ! Qinjecte
                   pcsing , &   ! Pertes de charge singulieres
               impression   &   ! Flag d'impression
                          )
!
! *********************************************************************
! PROGICIEL : MASCARET         F. DEMANGEON
!                              F. ZAOUI
!
! VERSION : 7.2.0                EDF-CEREMA
! *********************************************************************
! FONCTION :
! --------
!
! SOUS-PROGRAMME DE BASE : ESTIMATION ITERATIVE DES COEFFICIENTS DE
! RUGOSITE MINEURS OU MAJEURS
!
! *********************************************************************
!
!
! COMMENTAIRES : LE PRINCIPE EST DE MINIMISER LA FONCTION DE COUT
! ------------   EGALE A LA SOMME DES CARRES DE (ZMES-ZCAL) , OU
!                ZMES EST UNE COTE MESUREE , ET ZCAL LA COTE
!                CALCULEE ASSOCIE
!
!                LA METHODE RETENUE EST UNE METHODE DE QUASI-NEWTON BFGS
!                --- SOLVEUR D'OPTIMISATION : N2QN1 - INRIA ---
!                       AUTEURS : C. LEMARECHAL et J.-CH. GILBERT 
!                
!                LES DERIVEES dZ/dCF12 SONT CALCULEES PAR DERIVATION AUTOMATIQUE
!                --- DIFFERENTIATEUR : TAPENADE - INRIA ---
!                       AUTEURS : L. HASCOET et V. PASCUAL
!
! *********************************************************************
!
! DEFINITION DES VARIABLES
! ------------------------
!.. Modules importes ..
!----------------------
   USE M_PRECISION
! Constantes nommees
! GPES
   USE M_PARAMETRE_C
! Messages d'erreur
   USE M_MESSAGE_C
! Numero de bief d'une section
   USE M_NUM_BIEF_S
! Calcul du nombre de Froude
   USE M_FROUDE_S
! Interface generique de traitement des erreurs
   USE M_TRAITER_ERREUR_I
   USE M_XINDIC_S
   USE M_STRICK_I
   USE M_DIRECT_I
   USE M_SIMUL_I
   USE M_SHARE_VAR

   IMPLICIT NONE
!.. Arguments ..
!---------------
! TABLEAU  DIMENSIONNE  A NbSect
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z
! TABLEAU  DIMENSIONNE  A NbSect
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: q1, q2
! TABLEAUX DIMENSIONNES A NMSCAL
        DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: pcsing
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qinjec
        LOGICAL, INTENT(IN) :: impression

      END SUBROUTINE CALAGE_N2QN1
  END INTERFACE

END MODULE M_CALAGE_N2QN1_I

