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

subroutine MINMAJ ( &
                 SMIN , &
                 QMIN , &
                 SMAJ , &
                 QMAJ , &
                 BETA , &
                SNODE , &
                QNODE , &
                NOEUD , &
                   JG , &
                   JD , &
                SGEO1 , &
                 SGEO , &
               DEBGE1 , &
               DEBGE2 , &
               NMLARG , &
               ERREUR   &
                      )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : CALCUL DE LA REPARTITION MINEUR MAJEUR
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  SMIN     ! TR !  R ! SECTION MOUILLEE MINEUR AU NOEUD             !
! !  QMIN     ! TR !  D ! DEBIT MINEUR                                 !
! !  SMAJ     ! TR !  D ! SECTION MOUILLEE MAJEUR                      !
! !  QMAJ     ! TR !  D ! DEBIT MOUILLE MAJEUR                         !
! !  BETA     ! TR !  D ! FONCTION DU MODELE DEBORD                    !
! !  SNODE    ! TR !  D ! SECTION MOUILLEE                             !
! !  QNODE    ! TR !  R ! DEBIT MOUILLE                                !
! !  SGEO1    ! TR !  A ! SURFACE MOUILLE LIT MINEUR                   !
! !  SGEO     ! TR !  A ! SURFACE MOUILLEE TOTALE                      !
! !  DEBGE1   ! TR !  A ! DEBITANCE MINEUR                             !
! !  DEBGE2   ! TR !  A ! DEBITANCE MAJEUR                             !
! !  SEPS     !  R !    ! PRECISION SUR LA SECTION MOUILLEE            !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!    SGEO,SGEO1 font partie d'une structure de donnees STRUCTURE_SECTION
!    DEBGE1 DEBGE2

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! EPS6
   use M_ERREUR_T    ! ERREUR
   use M_DICHO_I   ! Interface du sous-programme DICHO

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(  out) :: SMIN,QMIN,SMAJ,QMAJ,BETA
   real(DOUBLE), dimension(:)    , intent(in)    :: SNODE,QNODE
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO1,SGEO
   real(DOUBLE), dimension(:,:)  , intent(in)    :: DEBGE1,DEBGE2
   integer     ,                   intent(in)    :: NMLARG
   integer                       , intent (in)   :: NOEUD
   integer                       , intent( in)   :: JG,JD
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE)   :: SURF,Q,SURFG,SURFD,SMING,SMIND
   real(DOUBLE)   :: DEB1GG,DEB1GD,DEB1G,DEB2GG,DEB2GD,DEB2G
   real(DOUBLE)   :: ETA
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0

   SURF = SNODE(NOEUD)
   Q    = QNODE(NOEUD)

   SURFG = SGEO(NOEUD,JG)
   SURFD = SGEO(NOEUD,JD)
   SMING = SGEO1(NOEUD,JG)
   SMIND = SGEO1(NOEUD,JD)

   SMIN(NOEUD) = ( SMIND * ( SURF - SURFG ) + SMING * ( SURFD - SURF ) ) / ( SURFD - SURFG )
   SMAJ(NOEUD) = SURF - SMIN(NOEUD)

   if( SMAJ(NOEUD) < 10._DOUBLE * SEPS ) then
      ! LIT MINEUR UNIQUEMNT
      QMIN(NOEUD) = Q
      QMAJ(NOEUD) = 0._DOUBLE
      BETA(NOEUD) = 1._DOUBLE
   else
      ! LIT MINEUR-MAJEUR
      DEB1GG = DEBGE1(NOEUD,JG)
      DEB1GD = DEBGE1(NOEUD,JD)
      DEB2GG = DEBGE2(NOEUD,JG)
      DEB2GD = DEBGE2(NOEUD,JD)

      DEB1G = ( DEB1GD * ( SURF - SURFG ) + DEB1GG * ( SURFD - SURF ) ) / ( SURFD - SURFG )
      DEB2G = ( DEB2GD * ( SURF - SURFG ) + DEB2GG * ( SURFD - SURF ) ) / ( SURFD - SURFG )

      ETA   = DEB1G / DEB2G

      QMAJ(NOEUD) = Q / ( 1 + ETA )
      QMIN(NOEUD) = Q - QMAJ(NOEUD)
      if( dabs(Q) > EPS6 ) then
         BETA(NOEUD) = SURF / ( Q * Q ) * &
                       ( QMIN(NOEUD) * QMIN(NOEUD) / SMIN(NOEUD) + &
                         QMAJ(NOEUD) * QMAJ(NOEUD) / SMAJ(NOEUD) )
      else
         BETA(NOEUD) = 1._DOUBLE
      endif
   endif

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine MINMAJ
