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

module M_SEUIL_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine SEUIL ( &
              COTR  , &
              QFIXG , &
              SGAUC , &
              SDROI , &
              ZVRAI , &
              SVRAI , &
              QVRAI , &
              YVRAI , &
              ITEMP , &
             IVALID , &
             NBSECT , &
         Impression , & ! Flag d'impression
       UniteListing , & ! Unite logique fichier listing
             Erreur  &
                   )

!***********************************************************************
!     CODE MASCARET : SOLUTION ANALYTIQUE DU PROBLEME DU SEUIL
!
!-----------------------------------------------------------------------

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, EPS4, EPS6
   use M_ERREUR_T    ! ERREUR
   use M_FH_I        ! Interface de la fonction FH
   use M_FCONJ_I     ! Interface de la fonction FCONJ
   use M_ZBRENT_I    ! Interface du sous-programme ZBRENT

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: COTR
   real(DOUBLE),                   intent(in)    :: QFIXG
   real(DOUBLE),                   intent(in)    :: SGAUC,SDROI
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(  out) :: ZVRAI,SVRAI,QVRAI
   real(DOUBLE), dimension(:)    , intent(  out) :: YVRAI
   integer     ,                   intent(in)    :: ITEMP,IVALID
   integer     ,                   intent(in)    :: NBSECT
   logical                       , intent(in   ) :: Impression
   integer     ,                   intent(in   ) :: UniteListing
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Communs ..
   !-------------
   real (double)  :: ALPHA
   real (double)  :: A1,A2,A3
   common / COEFS  / ALPHA
   common / COEFH  / A1,A2,A3 

   end subroutine SEUIL

   end interface

end module M_SEUIL_I
