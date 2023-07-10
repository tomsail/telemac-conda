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

function CSUR( &
           NOEUD , &
          TIRANT , &
              DZ , &
            SGEO , &
          NMLARG , &
          Erreur )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : CALCUL DE LA SURFACE MOUILLEE
!                     EN FONCTION DU TIRANT D'EAU SUR MAILLAGE INITIAL
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  CSUR     !  R !  R ! SURFACE MOUILLE POUR LE TIRANT D'EAU         !
! !  NOEUD    !  I !  D ! NOEUD CONSIDERE DU MAILLAGE                  !
! !  TIRANT   !  R !  D ! TIRANT D'EAU                                 !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  SGEO     ! TR !  D ! SURFACE MOUILLE PLANIMETREE                  !
! !  NMLARG   !  I !  D ! Nombre de pas des sections                   !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  JG       !  I !  A ! BORNE GAUCHE DE L'INTERVALLE CONTENANT SURF  !
! !  JD       !  I !  A ! BORNE DROITE DE L'INTERVALLE CONTENANT SURF  !
! !  SG       !  R !  A ! SURFACE MOUILLE POUR LA BORNE GAUCHE         !
! !  SD       !  R !  A ! SURFACE MOUILLE POUR LA BORNE DROITE         !
! !  DY       !  R !  A ! ELEMENT DE TIRANT D'EAU POUR L'INTERPOLATION !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!   SGEO fait partie d'une structure de donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T  ! Erreur
   use M_MESSAGE_C ! messages d'erreur
   use M_TRAITER_ERREUR_I ! traitement de l'erreur

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)                                  :: CSUR
   integer     ,                   intent(in)    :: NOEUD
   real(DOUBLE),                   intent(in)    :: TIRANT
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEO
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)                ,intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   integer        :: JG,JD
   real(DOUBLE)   :: DY,SG,SD
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0

   ! RECHERCHE DE L'INTERVALLE CONTENANT LE TIRANT D'EAU
   ! ---------------------------------------------------
   JG = int( TIRANT / DZ(NOEUD) ) + 1
   JD = JG + 1

   ! MS2018 : Adaptation for Courlis (deposition optimisation) must be validate
   if( JD > NMLARG ) then
      Erreur%Numero = 100
      Erreur%ft   = err_100
      Erreur%ft_c = err_100c
      JG = NMLARG-1
      JD = JG + 1
      !  call TRAITER_ERREUR  (Erreur, TIRANT, DZ(NOEUD)*(NMLARG-1), NOEUD)
      !   !arbredappel_old    = trim(!Erreur%arbredappel)
      ! !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CSUR'
!      return
   endif
   ! end MS2018

   DY = TIRANT - real( JG - 1 , DOUBLE ) * DZ(NOEUD)

   !     LA SECTION MOUILLEE ETANT COMPRISE ENTRE JD ET JG
   !     ON CALCULE LA SECTION MOUILLEE PAR INTERPOLATION

   SG = SGEO(NOEUD,JG)
   SD = SGEO(NOEUD,JD)

   CSUR = ( SD * DY + SG * (DZ(NOEUD) - DY) ) / DZ(NOEUD)

   !------------------
   ! Fin du traitement
   !------------------

   !  !Erreur%arbredappel = !arbredappel_old

   return

end function CSUR
