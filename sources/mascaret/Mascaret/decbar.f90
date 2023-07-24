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

subroutine DECBAR( &
               ISBAR , &
               ITBAR , &
                ZBAR , &
               pente , &
                  DT , &
               NBARA , &
                ZRUP , &
               NBBAR , &
               ZNODE , &
                COTR , &
              Erreur )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
! FONCTION : DECALAGE DES BARRAGES DEVERSANTS QUI CASSENT
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  IBAR     ! TI !  M ! TYPE DE BARRAGE                              !
! !  NBARA    !  I !  M ! NOMBRE DE BARRAGES AVAL                      !
! !  ZRUP     ! TR !  D ! COTE DE RUPTURE D'UN BARRAGE                 !
! !  NBBAR    !  I !  D ! NOMBRE MAXIMUN DE BARRAGE                    !
! !  ZNODE    ! TR !  D ! COTE AUX SECTIONS DE CALCUL                  !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T  ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------

   !  1 Ere  dimension NMBAR
   integer     ,  dimension(:)      ,intent(inout) :: ISBAR
   integer     ,  dimension(:)      ,intent(inout) :: ITBAR
   real (DOUBLE) , dimension(:)     ,intent(inout) :: ZBAR
   real(DOUBLE) ,  dimension(:)     ,intent(inout) :: pente
   integer     ,                    intent(inout)  :: NBARA
   ! 1ere dimension NMBAR
   real(DOUBLE), dimension(:)    , intent(inout)   :: ZRUP
   integer     ,                   intent(in)      :: NBBAR
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: ZNODE
   real (DOUBLE), dimension(:)   , intent(in)    :: COTR
   real (DOUBLE)                 , intent (in)   :: DT
   Type (ERREUR_T)               , intent(inout) :: Erreur

   !.. Variables locales ..
   !-----------------------
   integer :: IB,IPOS
   !character(132) :: !arbredappel_old ! arbre d'appel precedent
   real (DOUBLE) Delta_Z,XX

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>DECBAR'

   do IB = 1 , NBBAR
      if( ( ITBAR(IB) == 4 ) .or. ( ITBAR(IB) == 6 ) ) then
         IPOS = ISBAR(IB)
         if( ZRUP(IB) < ZNODE(IPOS) ) then
            Delta_Z = pente(IB) * DT
            XX      = ZBAR(IB) - Delta_Z
            if( COTR(IPOS) > XX ) then
               ITBAR(IB) = 0
               NBARA = NBARA - 1
               ! Print *,'effacement du barrage',IB,ZRUP(IB),ZNODE(IPOS),IPOS
            else
               ZBAR (IB) = ZBAR(IB) - Delta_Z
               ZRUP (IB) = COTR(IPOS)
            endif
         endif
      endif
   end do

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine DECBAR
