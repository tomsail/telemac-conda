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

MODULE M_DIFF_JCOUT_I
!***********************************************************************
! PROGICIEL : MASCARET       F. DEMANGEON
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
  IMPLICIT NONE
  INTERFACE 
!
      SUBROUTINE DIFF_JCOUT(grmin, grmaj, z, q, qinjec, &
& pcsing, zinit, dk, z_cal, ic)

!
!  VARIABLES LIEES A L'HYDRAULIQUE
!
!.. Modules importes ..
!----------------------
        USE M_PRECISION
        USE M_PARAMETRE_C
        USE M_MESSAGE_C
        USE M_NUM_BIEF_S
        USE M_FROUDE_S
        USE M_TRAITER_ERREUR_I
        USE M_STRICK_I
        USE M_PERMAT_I
        USE M_XINDIC_S
        USE M_SHARE_VAR
        USE M_DIFF_Z_CF12_FWD_SARAP_VAR
        IMPLICIT NONE
!
!.. Arguments ..
!---------------
!
! TABLEAU  DIMENSIONNE  A NbSect
!
  DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: z
!
! TABLEAU  DIMENSIONNE  A NbSect
!
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: q
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: qinjec
  DOUBLE PRECISION, INTENT(INOUT) :: zinit
! TABLEAUX DIMENSIONNES A Nbsect
  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: pcsing
!
! VARIABLES CASTOR
!
  INTEGER, INTENT(IN) :: ic
! gradient dimensionne au nombre de zones de frottemenr
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: grmin, grmaj
  DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: z_cal
  DOUBLE PRECISION, INTENT(INOUT) :: dk
      END SUBROUTINE DIFF_JCOUT
  END INTERFACE

END MODULE M_DIFF_JCOUT_I
