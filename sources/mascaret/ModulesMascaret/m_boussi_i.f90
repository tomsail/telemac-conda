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

module M_BOUSSI_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine BOUSSI ( &
        QNODE        , &
        SNODE        , &
        YNODE        , &
        Z            , &
        SCUBE1       , &
        SCUBE        , &
        SPREC        , &
        QPREC        , &
        CE           , &
        ZF           , &
        NSECG        , &
        NSECDL       , &
        X            , & 
        DX           , &
        DT           , &
        NS           , &
        ERREUR         &
                  )

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T    ! ERREUR
   use M_PARAMETRE_C ! GPES, EPS3, EPS6, EPSN6
   use M_CELE_I      ! Interface de la fonction    CELE
   use M_CSUR_I      ! Interface de la fonction    CSUR
   use M_PRESD_I     ! Interface de la fonction    PRESD
   use M_DICHODM_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE), dimension(:),     intent(inout) :: QNODE
   real(DOUBLE), dimension(:),     intent(in)    :: SNODE,SCUBE,SCUBE1
   real(DOUBLE), dimension(:),     intent(in)    :: SPREC,YNODE
   real(DOUBLE), dimension(:),     intent(in)    :: QPREC
   real(DOUBLE), dimension(:),     intent(in)    :: CE,X
   real(DOUBLE), dimension(:),     intent(in)    :: ZF,Z
   integer     ,                   intent(in)    :: NSECG
   integer     ,                   intent(in)    :: NSECDL
   real(DOUBLE),                   intent(in)    :: DX
   real(DOUBLE),                   intent(in)    :: DT
   integer     ,                   intent(in)    :: NS
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine BOUSSI

   end interface

end module M_BOUSSI_I
