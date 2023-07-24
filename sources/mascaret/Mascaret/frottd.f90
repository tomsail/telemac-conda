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

subroutine FROTTD( &
                FROT , &
               NOEUD , &
                SURF , &
                   Q , &
              DEBGED , &
               SGEOD , &
                IDEB , &
              NMLARG , &
              Erreur )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : CALCUL DU FROTTEMENT A PARTIR DE LA DEBITANCE
!                     SUR LE MAILLAGE DECALE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  FROT     !  R !  R ! FROTTEMENT                                   !
! !  NOEUD    !  I !  D ! NOEUD CONSIDERE DU MAILLAGE                  !
! !  SURF     !  R !  D ! SURFACE MOUILLEE AU NOEUD                    !
! !  Q        !  R !  D ! DEBIT                                        !
! !  DEBGED   ! TR !  D ! DEBITANCE SUR LE MAILLAGE DECALE             !
! !  SGEOD    ! TR !  D ! SURFACE MOUILLE PLANIMETREE (maillage decale)!
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!     SGEODet DEBGED font partie d'une structure de donnees STRUCTURE_SECTION

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T  ! ERREUR
   use M_DICHODM_I   ! Interface du sous-programme DICHOD

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(  out) :: FROT
   integer     ,                   intent(in)    :: NOEUD,IDEB
   real(DOUBLE),                   intent(in)    :: SURF,Q
   ! 1ere dimension IM1
   real(DOUBLE), dimension(:,:)  , intent(in)    :: DEBGED,SGEOD
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)               , intent(inout) :: Erreur

   !.. Variables locales ..
   !-----------------------
   integer      :: JG,JD
   real(DOUBLE) :: DEBG,DEBD,SG,SD,DEB
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !  !arbredappel_old    = trim(!Erreur%arbredappel)
   ! !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>FROTTD'

   call DICHODM (JG, JD, SURF, NOEUD, SGEOD, IDEB, NMLARG, Erreur)
   if( Erreur%Numero /= 0 ) then
      return
   endif

   ! SECTION MOUILLEE ET DEBITANCE AUX BORNES
   DEBG = DEBGED(NOEUD,JG)
   DEBD = DEBGED(NOEUD,JD)
   SG   = SGEOD(NOEUD,JG)
   SD   = SGEOD(NOEUD,JD)

   ! INTERPOLATION DE LA DEBITANCE
   ! -----------------------------
   DEB  = ( DEBD * ( SURF - SG ) + DEBG * ( SD - SURF ) ) / ( SD - SG )
   FROT = ( Q * dabs( Q ) ) / ( DEB * DEB )

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine FROTTD
