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

module M_STOCK_CALAGE_I
!***********************************************************************
! PROGICIEL : MASCARET       A. LEBOSSE
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
   interface

   subroutine STOCK_CALAGE( &
                            X , CF1 , CF2 , Z , Q , FCOUTM , &
                            FichierResultat , &
                            FichierResultat1 , &
                            Iter , &
                            TEMPS , &
                            NbCrue , &
                            Crue , &
                            Erreur )

!***********************************************************************
!
!  FONCTION :  STOCKAGE SUR FICHIER AU FORMAT `LIDO NON PERMANENT'
!  --------    DES RESULTATS PROPRES A CASIER
!
!  SOUS PROGRAMMES APPELANT(S) : STOCK
!  ---------------------------
!  SOUS PROGRAMMES APPELE(S)   : Neant
!  -------------------------
!
!***********************************************************************

   !============================= Declarations ===========================
   use M_ERREUR_T
   use M_FICHIER_T
   use M_PRECISION
   use M_CONSTANTES_CALCUL_C

   implicit none

   !.. Arguments ..
   type(ERREUR_T),               intent(inout) :: Erreur
   real(DOUBLE)       , dimension(:)  , intent(in)    :: X
   real(DOUBLE)       , dimension(:)  , intent(in)    :: Z,Q 
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF1
   real(DOUBLE)       , dimension(:)  , intent(in)    :: CF2 
   real(DOUBLE)                       , intent(in)    :: FCOUTM
   real(DOUBLE),                 intent(in   ) :: TEMPS
   integer,                      intent(in   ) :: Iter, NbCrue, Crue
   type(FICHIER_T),              intent(in   ) :: FichierResultat,FichierResultat1
   !.. Constantes ..
   character(LEN=4), parameter :: NOM_FIN       = "FIN "
   character(LEN=4), parameter :: NOM_IDEB_BIEF = "I1  "
   character(LEN=4), parameter :: NOM_IFIN_BIEF = "I2  "

   end subroutine STOCK_CALAGE

   end interface

end module M_STOCK_CALAGE_I
