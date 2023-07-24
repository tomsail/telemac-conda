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

module M_CALDEB_I
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine CALDEB ( &
     DEB          , & ! Debitance
     DEB1         , & ! Debitance mineur
     DEB2         , & ! Debitance majeur
     Section      , & ! Section de calcul
     Pas          , & ! Pas de planimetrage de calcul
     DS1          , & ! Section mineure planimetree
     DS2          , & ! Section majeure planimetree
     DP1          , & ! Perimetre mouillee mineure planimetree
     DP2          , & ! Perimetre mouillee majeure planimetree
     CF1          , & ! Coeff de frottement mineur aux sections
     CF2          , & ! Coeff de frottement majeur aux sections
     ProfGauche   , & ! Profil gauche
     ProfDroit    , & ! Profil droit
     XDELTA       , & ! Position relative de la section / profils
     LoiFrottement, & ! Loi de frottement utilisee
     Erreur         & ! Erreur
     )

!***********************************************************************
!   FONCTION : CODE MASCARET : CALCUL DE LA DEBITANCE
!
!-----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE : --
!   -------------------------
!
!   SOUS PROGRAMME APPELANT : - PLANMA
!   -------------------------
!   SOUS-PROGRAMMES APPELES : --
!   -------------------------
!***********************************************************************

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! W23, W16, W09, PI, SEPS
   use M_DEBITANCE_S ! calcul de la debitance
   use M_ERREUR_T    ! Type ERREUR_T

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension NbSect
   real(double), dimension(:,:)  , intent(  out) :: DEB,DEB1,DEB2
   integer     ,                   intent(in   ) :: Section
   integer     ,                   intent(in   ) :: Pas
   ! 1ere dimension NbProf
   real(double), dimension(:,:)  , intent(in   ) :: DS1,DS2
   real(double), dimension(:,:)  , intent(in   ) :: DP1,DP2
   real(double), dimension(:)    , intent(in   ) :: CF1,CF2
   integer     ,                   intent(in   ) :: ProfGauche
   integer     ,                   intent(in   ) :: ProfDroit
   real(double),                   intent(in   ) :: XDELTA
   integer     ,                   intent(in   ) :: LoiFrottement
   type(ERREUR_T),                 intent(inout) :: Erreur

   end subroutine CALDEB

   end interface

end module M_CALDEB_I
