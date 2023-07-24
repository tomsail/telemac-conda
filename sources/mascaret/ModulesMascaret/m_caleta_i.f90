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

module M_CALETA_I
! *********************************************************************
! PROGICIEL : MASCARET         F. MAUREL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine CALETA( &
             S      , &
             Q      , &
             U      , &
             FR     , &
             C      , &
             H      , &
             Z      , &
             PR     , &
             BETA   , &
             INDIC  , &
             SNODE  , &
             QNODE  , &
             UNODE  , &
             ZNODE  , &
             YNODE  , &
      JGNODE,JDNODE , & 
              FROUD , &
              CNODE , &
              BETAN , &
             COTR   , &
             DZ     , &
             DZD    , &
             SGEO   , &
             ALGEO  , &
             SGEOD  , &
             PRGEOD , &
             HEPS   , &
             NOEUD  , &
             CELGAU , &
             NMLARG , &
             ERREUR   &
            )

!*****************************************************************************
!  FONCTION : CALCUL DE L'ETAT DANS UNE CELLULE POUR UN SOLVEUR DE ROE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  S        !  R !  M ! SECTION MOUILLEE                             !
! !  Q        !  R !  M ! DEBIT                                        !
! !  U        !  R !  M ! VITESSE                                      !
! !  FR       !  R !  M ! FROUDE                                       !
! !  C        !  R !  M ! CELERITE                                     !
! !  H        !  R !  M ! TIRANT D'EAU                                 !
! !  Z        !  R !  M ! COTE SURFACE LIBRE                           !
! !  PR       !  R !  M ! PRESSION                                     !
! !  BETA     !  R !  M !                                              !
! !  INDIC    !  I !  M !                                              !
! !  SNODE    ! TR !  D ! SURFACE MOUILLEE                             !
! !  QNODE    ! TR !  D ! DEBIT                                        !
! !  UNODE    ! TR !  D ! VITESSE                                      !
! !  ZNODE    ! TR !  D !                                              !
! !  YNODE    ! TR !  D !                                              !
! !  FROUD    ! TR !  D !                                              !
! !  CNODE    ! TR !  D !                                              !
! !  BETAN    ! TR !  D !                                              !
! !  COTR     ! TR !  D ! COTE DU RADIER                               !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  DZD      ! TR !  D ! PAS DE PLANIMETRAGE AUX INTERFACES           !
! !  SGEO     ! TR !  D ! SURFACES PLANIMETREES                        !
! !  ALGEO    ! TR !  D ! LARGEUR PLANIMETREE                          !
! !  SGEOD    ! TR !  D ! SURFACES PLANIMETREES(Maillage decale)       !
! !  PRGEOD   ! TR !  D ! PRESSION PLANIMETREE (MAILLAGE DECALE)       !
! !  HEPS     !  R !  D ! VALEUR MINIMALE DU TIRANT D'EAU              !
! !  NOEUD    !  I !  D ! NUMERO DE CELLULE COURANT                    !
! !  CELGAU   !  I !  D ! = 1 SI NOEUD : CELLULE GAUCHE DE L INTERFACE !
! !           !    !    ! = 0 SI NOEUD : CELLULE DROITE DE L INTERFACE !
! !  NMLARG   !    !  D !                                              !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!  SGEO, ALGEO,SGEOD  et PRGEOD font partie d'une structure donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T    ! ERREUR
   use M_PARAMETRE_C ! Erreur
   use M_CELE_I      ! Interface de la fonction    CELE
   use M_CSUR_I      ! Interface de la fonction    CSUR
   use M_PRESD_I     ! Interface de la fonction    PRESD
   use M_DICHODM_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                 intent(  out)   :: S,Q,U,FR,C,H,Z,PR,BETA
   integer     ,                 intent(inout)   :: INDIC
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)  , intent(in)      :: SNODE,QNODE,UNODE
   real(DOUBLE), dimension(:)  , intent(in)      :: ZNODE,YNODE
   integer     , dimension(:)  , intent(in)      :: JGNODE,JDNODE
   real(DOUBLE), dimension(:)  , intent(in)      :: FROUD,CNODE,BETAN
   real(DOUBLE), dimension(:)  , intent(in)      :: COTR,DZ
   ! 1ere dimension IM1
   real(DOUBLE), dimension(:)  , intent(in)      :: DZD
   ! 1ere dimension IM , 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:), intent(in)      :: SGEO,ALGEO
   ! 1ere dimension IM1, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:), intent(in)      :: SGEOD,PRGEOD
   real(DOUBLE),                 intent(in)      :: HEPS
   integer     ,                 intent(in)      :: NOEUD,CELGAU
   integer     ,                 intent(in)      :: NMLARG
   Type (ERREUR_T)              , intent(inout)  :: ERREUR

   end subroutine CALETA

   end interface

end module M_CALETA_I
