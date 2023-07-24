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

module M_SOLEX_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine SOLEX ( &
            CGAUC   , &
            YGAUC   , &
            YDROI   , &
            WR      , &
            URG     , &
            CRG     , &
            YRG     , &
            T       , &
            RINV    , &
            X       , &
            XBAR    , &
            ALARG   , &
            SVRAI   , &
            QVRAI   , &
            UVRAI   , &
            ZVRAI   , &
            YVRAI   , &
            NBSECT  , &
            Erreur    &
                )

!***********************************************************************
!     CODE MASCARET : SOLUTION ANALYTIQUE DE LA RUPTURE DE BARRAGE 
!
!-----------------------------------------------------------------------
!           DANS LE PLAN (X,T)
!           ON APPELLE C L'INTERSECTION A T AVEC X = -CGAUC*T
!                      D L'INTERSECTION A T AVEC X = (URG - CRG)*T
!                      E L'INTERSECTION A T AVEC X =  WR*T
!                                          WR:VITESSE DU RESSAUT
!           A ET B SONT LES LIMITES DU DOMAINE D'ETUDE
!           A GAUCHE DE C L EAU EST IMMOBILE DE COTE YG
!           ENTRE C ET D ON A UNE ONDE DE DETENTE
!           ENTRE D ET E L EAU EST A VITESSE URG ET DE COTE (CRG**2)/G
!           A DROITE DE E L EAU EST IMMOBILE DE COTE YD
!-----------------------------------------------------------------------

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, W23, EPS6
   use M_ERREUR_T    ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(in)    :: CGAUC,YGAUC,YDROI
   real(DOUBLE),                   intent(in)    :: WR,URG,CRG,YRG
   real(DOUBLE),                   intent(in)    :: T
   integer     ,                   intent(in)    :: RINV
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   real(DOUBLE),                   intent(in)    :: XBAR
   real(DOUBLE),                   intent(in)    :: ALARG
   real(DOUBLE), dimension(:)    , intent(  out) :: SVRAI,QVRAI,UVRAI
   real(DOUBLE), dimension(:)    , intent(  out) :: ZVRAI,YVRAI
   integer     ,                   intent(in)    :: NBSECT
   Type (ERREUR_T)               , intent(inout) :: Erreur

   end subroutine SOLEX

   end interface

end module M_SOLEX_I
