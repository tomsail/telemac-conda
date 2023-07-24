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

module M_CALCS_MICROPOL_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   Interface

   SUBROUTINE CALCS_MICROPOL( RNU , S , &
                              Nbsect , NBTRA , Nbsing , &
                              Q , A , H , RH , ST , C , &
                              SA , T , TParph , DT )

   USE  M_PRECISION 
   USE  M_CONSTANTES_TRACER_T
   USE  M_PARAMETRES_QUALITE_EAU_T

   IMPLICIT NONE

   REAL(DOUBLE)       , DIMENSION(:,:)  ,intent(inout) :: RNU , S , SA
   REAL(DOUBLE)       , DIMENSION(:)    ,intent(in   ) :: Q , A , H , ST , RH
   REAL(DOUBLE)       , DIMENSION(:,:)  ,intent(inout) :: C
   INTEGER         :: Nbsect , NBTRA , nbsing
   REAL(DOUBLE)    :: T , DT
   !
   !  DECLARATION DES PARAMETRES PHYSIQUES
   !
   type (PARAMETRES_QUALITE_EAU_T) :: TPARPH

   end subroutine CALCS_MICROPOL

   end Interface

end module M_CALCS_MICROPOL_I
