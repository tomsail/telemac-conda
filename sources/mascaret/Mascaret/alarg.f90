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

function ALARG( &
                NOEUD  , &
                TIRANT , &
                DZ     , &
                ALGEO  , &
                NMLARG , &
                ERREUR )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!
!                     CALCUL DE LA SURFACE MOUILLEE
!                     EN FONCTION DU TIRANT D'EAU SUR MAILLAGE INITIAL
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  ALARG    !  R !  R !                                              !
! !  NOEUD    !  I !  D ! NOEUD CONSIDERE DU MAILLAGE                  !
! !  TIRANT   !  R !  D ! TIRANT D'EAU                                 !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  ALGEO    ! TR !  D ! LARGEUR AU MIROIR PLANIMETREE                !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  JG       !  I !  A ! BORNE GAUCHE DE L'INTERVALLE CONTENANT SURF  !
! !  JD       !  I !  A ! BORNE DROITE DE L'INTERVALLE CONTENANT SURF  !
! !  LG       !  R !  A !                                              !
! !  LD       !  R !  A !                                              !
! !  DY       !  R !  A ! ELEMENT DE TIRANT D'EAU POUR L'INTERPOLATION !
! !___________!____!____!______________________________________________!
!
!   ALGEO fait partie d'une structure de donnees STRUCTURE_SECTION
!
!***********************************************************************

   !============================= Declarations ===========================
   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_MESSAGE_C           ! Messages d'erreur
   use M_ERREUR_T            ! ERREUR
   use M_TRAITER_ERREUR_I    ! Traitement des erreurs

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)                                 :: ALARG
   integer      ,                 intent(in)    :: NOEUD
   real(DOUBLE) ,                 intent(in)    :: TIRANT
   ! 1ere dimension IM
   real(DOUBLE) , dimension(:)  , intent(in)    :: DZ
   real(DOUBLE) , dimension(:,:), intent(in)    :: ALGEO
   integer      ,                 intent(in)    :: NMLARG
   Type (ERREUR_T)             ,  intent(inout) :: Erreur

   !.. Variables locales ..
   !-----------------------
   integer        :: JG , JD
   real(DOUBLE)   :: DY , LG , LD
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================
   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>ALARG'

   ! RECHERCHE DE L'INTERVALLE CONTENANT LE TIRANT D'EAU
   ! ---------------------------------------------------
   JG = int( TIRANT / DZ(NOEUD) ) + 1
   JD = JG + 1

   if( JD > NMLARG ) then
      Erreur%Numero = 100
      Erreur%ft     = err_100
      Erreur%ft_c   = err_100c
      call TRAITER_ERREUR( Erreur , TIRANT , DZ(NOEUD) * ( NMLARG - 1 ) , NOEUD )
      ALARG = 0.
      return
   endif

   DY = TIRANT - real( JG - 1 , DOUBLE ) * DZ(NOEUD)

   !     LA SECTION MOUILLEE ETANT COMPRISE ENTRE JD ET JG
   !     ON CALCULE LA SECTION MOUILLEE PAR INTERPOLATION

   LG = ALGEO(NOEUD,JG)
   LD = ALGEO(NOEUD,JD)

   ALARG = ( LD * DY + LG * ( DZ(NOEUD) - DY ) ) / DZ(NOEUD)

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end function ALARG
