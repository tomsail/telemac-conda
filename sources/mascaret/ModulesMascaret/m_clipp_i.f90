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

module M_CLIPP_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine CLIPP ( &
            SNODE   , &
            QNODE   , &
            SGEO    , &
            DZ      , &
            HEPS    , &
            SEPS    , &
           IFIGE    , &
          NBSECT    , &
          NMLARG    , &
          ERREUR    &
                )

!************************************************************************
!     CODE MASCARET : CLIPPING DES HAUTEURS D'EAU NEGATIVES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  SNODE    ! TR !  M ! SURFACE MOUILLEE AU NOEUD                    !
! !  QNODE    ! TR !  M ! DEBIT                                        !
! !  SGEO     ! TR !  D ! SURFACE MOUILLEE PLANIMETREE                 !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  HEPS     !  R !  D ! SEUIL MINIMUM POUR LES HAUTEURS              !
! !  SEPS     !  R !  D ! SEUIL MINIMUM POUR LES SURFACES              !
! !  NBSECT   !  I !  D ! Nombre de sections                           !
! !  NMLARG   !    !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  NOEUD    !  I !  A ! NOEUD CONSIDERE DU MAILLAGE                  !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
! SGEO fait partie d'une structure de donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T
   use M_CSUR_I     ! Interface de la fonction CSUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(inout) :: SNODE
   real(DOUBLE), dimension(:)    , intent(inout) :: QNODE
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   real(DOUBLE),                   intent(in)    :: HEPS,SEPS
   integer     ,dimension(:)    ,   intent(in)    :: IFIGE
   integer     ,                   intent(in)    :: NBSECT
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)                ,intent(inout) :: ERREUR

   end subroutine CLIPP

   end interface

end module M_CLIPP_I
