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

module M_ERODEV_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************
   interface

   subroutine ERODEV ( &
              ZDEV   , &
              ZG     , &
              HG     , &
              QD     , &
              IPOS   , &
              ALGEO  , &
              DZ     , &
              DT     , &
              NMLARG , &
              ERREUR   &
               )

!***********************************************************************
!     CODE MASCARET : CALCUL  DE L'EROSION D'UNE CODE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  ZDEV     !  R !  R ! COTE DE DEVERSEMENT                          !
! !  ZG       !  R !  D ! COTE DE LA CELLULE GAUCHE                    !
! !  HG       !  R !  D ! COTE DE LA HAUTEUR A GAUCHE                  !
! !  QD       !  R !  D ! COTE DE LA CELLULE DROITE                    !
! !  IPOS     !  I !  D ! INDICE DE LA SECTION                         !
! !  ALGEO    ! TR !  D ! LARGEUR PLANIMETREE                          !
! !  DZ       ! TR !  R ! PAS DE PLANIMETRAGE                          !
! !  DT       !  R !  R ! PAS DE TEMPS                                 !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!     TYPE : E (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!    ALGEO  fait partie d'une structure de donnees STRUCTURE_SECTION

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, EPS4
   use M_ERREUR_T    ! ERREUR
   use M_ALARG_I     ! Interface de la fonction ALARG

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(inout) :: ZDEV
   real(DOUBLE),                   intent(in)    :: ZG,HG,QD
   integer     ,                   intent(in)    :: IPOS
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: ALGEO
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   real(DOUBLE),                   intent(in)    :: DT
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)                ,intent(inout) :: ERREUR

   end subroutine ERODEV

   end interface

end module M_ERODEV_I
