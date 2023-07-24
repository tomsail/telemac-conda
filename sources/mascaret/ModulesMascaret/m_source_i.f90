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

module M_SOURCE_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P4R0                EDF-CEREMA
! *********************************************************************

   interface

   subroutine SOURCE ( &
             SOTILD  , &
             SOPRIM  , &
             SOFROT  , &
             ZG      , &
             ZD      , &
             PRG     , &
             PRD     , &
             CTILD   , &
             BETA    , &
             X       , &
             SNODE   , &
     QNODE   , UNODE , JDNODE, &
             QIN     , &
             PCSing  , &
             COTR    , &
             SGEO    , &
             SGEOD   , &
             PRGEOD  , & 
             DEBGED  , &
             FRTIMP  , &
             DZD     , &
             DZ      , &
             NOEUD   , &
             NSECG   , &
             NMLARG  , &
 PerteElargissementTrans, & 
             CQMV       , &
             Erreur    &
               )

!***********************************************************************
!   CODE MASCARET : RESOLUTION DES EQUATIONS DE SAINT VENANT 
!                      PAR UN SCHEMA DE ROE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  SOTILD   ! TD !  A ! T. SOURCE  INTERFACE                         !
! !  SOPRIM   !  D !  A ! T. SOURCE (CENTRE DE LA CELLULE)             !
! !  SOFROT   ! TD !  A ! FROTTEMENT INTERFACE                         !
! !  ZG       !  D !  A ! COTE SURFACE LIBRE CELLULE DE GAUCHE         !
! !  ZD       !  D !  A ! COTE SURFACE LIBRE CELLULE DE DROITE         !
! !  CTILD    !  D !  A ! CELERITE MOYENNE DE ROE                      !
! !  X        ! TR !  D ! ABSCISSES DES POINTS DU MAILLAGE             !
! !  SNODE    ! TR !    !                                              !
! !  QNODE    ! TR !    !                                              !
! !  QIN      ! TR !  D ! DEBIT D'APPORT                               !
! !  COTR     ! TR !  D ! COTE DU RADIER                               !
! !  SGEO     ! TR !  D ! SURFACES PLANIMETREES                        !
! !  SGEOD    ! TR !  D ! SURFACES PLANIMETREES(MAILLAGE DECALE)       !
! !  PRGEOD   ! TR !  D ! PRESSION PLANIMETREE (MAILLAGE DECALE)       !
! !  DEBGED   ! TR !    !                                              !
! !  FRTIMP   !  L !    !                                              !
! !  DZD      !    !    !                                              !
! !  DZ       !    !    !                                              !
! !  NOEUD    !    !    !                                              !
! !  NSECG    !    !    !                                              !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  SMIL     ! TR !  A ! SECTION MOYENNE ENTRE DEUX CELLULES          !
! !  QMIL     ! TR !  A ! DEBIT MOYEN ENTRE DEUX CELLULES              !
! !  FROT     ! TR !  A ! FROTTEMENT A L'INTERFACE ENTRE DEUX CELLULES !
! !  SGPRI    ! TR !  A ! SECTION MOUILLE A GAUCHE POUR Z=ZD           !
! !  SDPRI    ! TR !  A ! SECTION MOUILLE A DROITE POUR Z=ZG           !
! !  HDPRI    ! TR !  A ! TIRANT D'EAU A DROITE POUR Z=ZG              !
! !  HGPRI    ! TR !  A ! TIRANT D'EAU A GAUCHE POUR Z=ZD              !
! !  DSDX     !  D !  A ! DERIVEE SECTION PAR RAPPORT A X A Z CONSTANT !
! !  PREPIG(D)!  D !  A ! PRESSION LIEE A LA CELLULE                   !
! !___________!____!____!______________________________________________!
!
!     TYPE : E (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES
   use M_ERREUR_T  ! ERREUR
   use M_CSUR_I      ! Interface de la fonction    CSUR
   use M_FROTTD_I    ! Interface du sous-programme FROTTD
   use M_PRESD_I     ! Interface de la fonction    PRESD

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   ! 1ere dimension 2
   real(DOUBLE), dimension(:)    , intent(  out) :: SOTILD,SOPRIM,SOFROT
   real(DOUBLE),                   intent(in)    :: ZG,ZD,PRG,PRD
   real(DOUBLE),                   intent(in)    :: CTILD
   real(DOUBLE),                   intent (in)   :: BETA
   real(DOUBLE), dimension(:)    , intent(in)    :: X,PCSing
   real(DOUBLE), dimension(:)    , intent(in)    :: SNODE,QNODE,QIN,COTR
   real(DOUBLE), dimension(:)    ,intent(in)     :: UNODE
   integer     , dimension(:)    ,intent(in)     :: JDNODE 
   ! 1ere dimension IM
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO
   ! 1ere dimension IM1
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEOD,PRGEOD
   real(DOUBLE), dimension(:,:)  , intent(in)    :: DEBGED
   logical     ,                   intent(in)    :: FRTIMP
   logical     ,                   intent (in)   :: PerteElargissementTrans
   integer     ,                   intent(in)    :: CQMV
   real(DOUBLE), dimension(:)    , intent(in)    :: DZD,DZ
   integer     ,                   intent(in)    :: NOEUD
   integer     ,                   intent(in)    :: NSECG
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   end subroutine SOURCE

   end interface

end module M_SOURCE_I
