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

module M_INTERSECT_I
!***********************************************************************
! PROGICIEL : MASCARET        S. MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine Intersect( &
                         ! Resultats
                      ZREF , & ! Tableau des cotes de ref aux sections
                       RGC , & ! Cotes de la rive gauche
                       RDC , & ! Cotes de la rive droite
                       CF1 , & ! Coefficient de frottement mineur
                       CF2 , & ! Coefficient de frottement majeur
                         ! Donnees
                    Profil , & ! Profils geometriques
                         ! Modele
                         X , & ! Abscisses des sections de calcul
                       IDT , & ! Positionement des sections / profils
                       XDT , & ! Positionement des sections / profils
                   Connect , & ! Connectivite du reseau
                 Extremite , & ! Extremite libre
                         ! Parametres
              TypeMaillage , & ! Type de calcul du maillage
           ImpressionPlani , & ! flag d'impression
              UniteListing , & ! Unite logique fichier listing
                FormatGeom , & ! Format du fichier geometrie utilise
       InterpLinCoeffFrott , & ! Flag d'interpolation lineaire des Coefficients de frottement
           PhaseSimulation , & ! Phase de la simulation
                    Erreur & ! Erreur
                           )

! **********************************************************************
!  FONCTION :
!  --------
!
!           INTERPOLATION DES CARACTERISTIQUES DES SECTIONS
!               EN FONCTION DES PROFILS
!***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_PARAMETRE_C         ! EPS1
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONNECT_T           ! Type CONNECT_T : connectivite du reseau
   use M_ERREUR_T         ! type ERREUR_T
   use M_EXTREMITE_T         ! Type EXTREMITE_T
   use M_PROFIL_T         ! Definition du type PROFIL_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs

   Implicit none

   ! ... Arguments ...
   ! Resultats
   real(DOUBLE)  , dimension(:) , pointer       :: ZREF
   real(DOUBLE)  , dimension(:) , pointer       :: RGC, RDC
   real(DOUBLE)  , dimension(:) , pointer       :: CF1
   real(DOUBLE)  , dimension(:) , pointer       :: CF2
   ! Donnees
   type(PROFIL_T), dimension(:) , intent(in   ) :: Profil
   ! Modele
   real(DOUBLE)  , dimension(:) , intent(in   ) :: X
   integer       , dimension(:) , intent(in   ) :: IDT
   real(DOUBLE)  , dimension(:) , intent(in   ) :: XDT
   type(CONNECT_T)              , intent(in   ) :: Connect
   type(EXTREMITE_T),dimension(:),intent(inout) :: Extremite
   ! Parametres
   integer                      , intent(in   ) :: TypeMaillage
   logical                      , intent(in   ) :: ImpressionPlani
   integer                      , intent(in   ) :: UniteListing
   logical                      , intent(in   ) :: InterpLinCoeffFrott
   integer                      , intent(in   ) :: FormatGeom
   integer                      , intent(in   ) :: PhaseSimulation

   type(ERREUR_T)               , intent(inout) :: Erreur

   end subroutine Intersect

   end interface

end module M_INTERSECT_I
