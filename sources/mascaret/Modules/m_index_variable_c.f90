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

module M_INDEX_VARIABLE_C
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   ! Liste des indices sur les tableaux de variables
   !=================================================

   ! 1. Variables principales
   !--------------------------
   integer , parameter :: VAR_X     = 1   ! maillage
   ! `X' doit etre la 1ere variable (indicee par 1)
   integer , parameter :: VAR_ZREF  = 2   ! cote du fond
   integer , parameter :: VAR_RGC   = 3   ! cote de la rive gauche
   integer , parameter :: VAR_RDC   = 4   ! cote de la rive droite
   integer , parameter :: VAR_CF1   = 8   ! coefficient de Strickler mineur
   integer , parameter :: VAR_CF2   = 9   ! coefficient de Strickler majeur
   integer , parameter :: VAR_Z     = 5   ! cote d eau
   integer , parameter :: VAR_Q1    = 6   ! debit mineur
   integer , parameter :: VAR_Q2    = 7  ! debit majeur
   integer , parameter :: VAR_S1    = 10 ! section mouillee mineure
   integer , parameter :: VAR_S2    = 11 ! section mouillee majeure
   integer , parameter :: VAR_FR    = 12  ! nombre de Froude
   integer , parameter :: VAR_BETA  = 13  ! coefficient beta du modele DEBORD

   ! Constante representant le nombre de variables principales
   integer , parameter :: NB_VAR_PRINCIPAL = 13

   ! 2. Variables optionnelles
   !--------------------------
   integer , parameter :: VAR_B1    = NB_VAR_PRINCIPAL + 1  ! largeur au miroir mineure
   integer , parameter :: VAR_B2    = NB_VAR_PRINCIPAL + 2  ! largeur au miroir majeure
   integer , parameter :: VAR_BS    = NB_VAR_PRINCIPAL + 3  ! largeur au miroir des zones de stockage
   integer , parameter :: VAR_P1    = NB_VAR_PRINCIPAL + 4  ! perimetre mouille mineur
   integer , parameter :: VAR_P2    = NB_VAR_PRINCIPAL + 5  ! perimetre mouille majeur
   integer , parameter :: VAR_RH1   = NB_VAR_PRINCIPAL + 6  ! rayon hydraulique mineur
   integer , parameter :: VAR_RH2   = NB_VAR_PRINCIPAL + 7  ! rayon hydraulique majeur
   integer , parameter :: VAR_V1    = NB_VAR_PRINCIPAL + 8  ! vitesse mineure
   integer , parameter :: VAR_V2    = NB_VAR_PRINCIPAL + 9  ! vitesse majeure
   integer , parameter :: VAR_TAUF  = NB_VAR_PRINCIPAL + 10  ! contrainte au fond
   integer , parameter :: VAR_Y     = NB_VAR_PRINCIPAL + 11  ! hauteur d'eau maximale
   integer , parameter :: VAR_HMOY  = NB_VAR_PRINCIPAL + 12  ! hauteur d'eau moyenne
   integer , parameter :: VAR_Q2G   = NB_VAR_PRINCIPAL + 13  ! debit majeur gauche
   integer , parameter :: VAR_Q2D   = NB_VAR_PRINCIPAL + 14  ! debit majeur droit
   integer , parameter :: VAR_SS    = NB_VAR_PRINCIPAL + 15  ! section mouillee de stockage
   integer , parameter :: VAR_VOL   = NB_VAR_PRINCIPAL + 16  ! volume du lit actif
   integer , parameter :: VAR_VOLS  = NB_VAR_PRINCIPAL + 17  ! volume de stockage
   integer , parameter :: VAR_CHARG = NB_VAR_PRINCIPAL + 18  ! charge
   integer , parameter :: VAR_ZMAX  = NB_VAR_PRINCIPAL + 19 ! cote maximale atteinte
   integer , parameter :: VAR_TZMAX = NB_VAR_PRINCIPAL + 20 ! instant de cote maximale atteinte
   integer , parameter :: VAR_VZMAX = NB_VAR_PRINCIPAL + 21 ! vitesse a la cote maximale
   integer , parameter :: VAR_ZMIN  = NB_VAR_PRINCIPAL + 22 ! cote minimale atteinte
   integer , parameter :: VAR_TZMIN = NB_VAR_PRINCIPAL + 23 ! instant de cote minimale atteinte
   integer , parameter :: VAR_V1MIN = NB_VAR_PRINCIPAL + 24 ! vitesse mineure minimale
   integer , parameter :: VAR_V1MAX = NB_VAR_PRINCIPAL + 25 ! vitesse majeure minimale
   integer , parameter :: VAR_BMAX  = NB_VAR_PRINCIPAL + 26 ! largeur au miroir maximale
   integer , parameter :: VAR_TOND  = NB_VAR_PRINCIPAL + 27 ! instant d'arrivee d'onde
   integer , parameter :: VAR_QMAX  = NB_VAR_PRINCIPAL + 28 ! debit maximal
   integer , parameter :: VAR_TQMAX = NB_VAR_PRINCIPAL + 29 ! instant de debit maximal
   integer , parameter :: VAR_EMAX  = NB_VAR_PRINCIPAL + 30 ! energie maximale
   integer , parameter :: VAR_YVRAI = NB_VAR_PRINCIPAL + 31  ! hauteur d'eau analytique
   integer , parameter :: VAR_QVRAI = NB_VAR_PRINCIPAL + 32  ! debit analytique
   integer , parameter :: VAR_QDEV  = NB_VAR_PRINCIPAL + 33  ! debit analytique
   integer , parameter :: VAR_Q     = NB_VAR_PRINCIPAL + 34  ! debit analytique  ! cote d eau
   integer , parameter :: VAR_Debi  = NB_VAR_PRINCIPAL + 35 ! flux de masse 

   ! Constante representant le nombre total de variables
   integer , parameter :: NB_TOT_VAR = 48

end module M_INDEX_VARIABLE_C
