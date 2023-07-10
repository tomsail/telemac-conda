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

function CSURMD( &
             NOEUD , &
              SURF , &
               DZD , &
             SGEOD , &
            NMLARG , &
            Erreur )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : CALCUL DU TIRANT D'EAU A PARTIR DE LA SURFACE
!                     MOUILLEE SUR MAILLAGE DECALE
!                     INVERSE DE CSURD
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !   NOEUD   !  I ! D  ! NUMERO DU NOEUD   OU LE CALCUL EST MENE      !
! !   SURF    !  R ! D  ! SURFACE MOUILLEE                             !
! !   DZD     ! TR ! D  ! PAS DE PLANIMETRAGE                          !
! !   SGEOD   ! TR ! D  ! SURFACE MOUILLEE PLANIMETREE                 !
! !   NMLARG  !    ! D  !                                              !
! !   ICRE    !  I ! R  ! Code de retour                               !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !   JG      !  I ! A  ! BORNE GAUCHE DE L'INTERVALLE CONTENANT SURF  !
! !   JD      !  I ! A  ! BORNE DROITE DE L'INTERVALLE CONTENANT SURF  !
! !   SG      !  R ! A  ! SURFACE MOUILLE POUR LA BORNE GAUCHE         !
! !   SD      !  R ! A  ! SURFACE MOUILLE POUR LA BORNE DROITE         !
! !   YG      !  R ! A  ! TIRANT D'EAU POUR LA BORNE GAUCHE            !
! !   YD      !  R ! A  ! TIRANT D'EAU POUR LA BORNE DROITE            !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
! SGEOD fait partie d'une structure de donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T  ! ERREUR
   use M_DICHOD_I  ! Interface du sous-programme DICHOD

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)                                  :: CSURMD
   integer     ,                   intent(in)    :: NOEUD
   real(DOUBLE),                   intent(in)    :: SURF
   real(DOUBLE), dimension(:)    , intent(in)    :: DZD
   real(DOUBLE), dimension(:,:)  , intent(in)    :: SGEOD
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)             , intent(inout)   :: ERREUR

   !.. Variables locales ..
   !-----------------------
   integer      :: JG,JD
   real(DOUBLE) :: SG,SD,YG,YD
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !   !arbredappel_old    = trim(!Erreur%arbredappel)
   !  !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>CSURMD'

   ! RECHERCHE DE L'INTERVALLE CONTENANT SURF PAR DICHOTOMIE
   call DICHOD( JG , JD , SURF , NOEUD , SGEOD , NMLARG , Erreur )

   if( Erreur%Numero /= 0 ) then
      CSURMD = 0.
      return
   endif

   ! CALCUL DU TIRANT D'EAU PAR INTERPOLATION
   SG = SGEOD(NOEUD,JG)
   SD = SGEOD(NOEUD,JD)
   YG = real( JG - 1 , DOUBLE ) * DZD(NOEUD)
   YD = real( JD - 1 , DOUBLE ) * DZD(NOEUD)

   CSURMD = ( YD * ( SURF - SG ) + YG * ( SD - SURF ) ) / ( SD - SG )

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end function CSURMD
