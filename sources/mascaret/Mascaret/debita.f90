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

subroutine DEBITA( &
                 DEB , &
               NOEUD , &
                SURF , &
                SGEO , &
              DEBGEO , &
              NMLARG , &
              ERREUR )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : CALCUL DE LA DEBITANCE AUX BORNES DE CALCUL 
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  DEB      ! TR !  R ! DEBITANCE AU NOEUD                           !
! !  NOEUD    !  I !  D ! NOEUD CONSIDERE DU MAILLAGE                  !
! !  SURF     !  R !  D ! SURFACE MOUILLEE AU NOEUD                    !
! !  SGEO     ! TR !  D ! SURFACE MOUILLE PLANIMETREE                  !
! !  DEBGEO   ! TR !  D ! DEBITANCE PLANIMETREE                        !
! !  NMLARG   !    !  D !                                              !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!    DEBGEO et SGEO font partie d'une structure de donnees STRUCTURE_SECTION

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_DICHO_I   ! Interface du sous-programme DICHO
   use M_ERREUR_T  ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(  out) :: DEB
   integer     ,                   intent(in)    :: NOEUD
   real(DOUBLE),                   intent(in)    :: SURF
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO,DEBGEO
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE) :: SG,SD,DEBG,DEBD
   integer      :: JG,JD
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0

   call DICHO( JG , JD , SURF , SGEO(NOEUD,:) , ERREUR )

   if( Erreur%Numero /= 0 ) then
      !arbredappel_old    = trim(!Erreur%arbredappel)
      !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>DEBITA'
      return
   endif

   ! SECTION MOUILLEE ET DEBITANCE AUX BORNES
   DEBG = DEBGEO(NOEUD,JG)
   DEBD = DEBGEO(NOEUD,JD)
   SG   = SGEO(NOEUD,JG)
   SD   = SGEO(NOEUD,JD)

   ! INTERPOLATION DE LA DEBITANCE
   ! -----------------------------
   DEB = ( DEBD * ( SURF - SG ) + DEBG * ( SD - SURF ) ) / ( SD - SG )

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine DEBITA
