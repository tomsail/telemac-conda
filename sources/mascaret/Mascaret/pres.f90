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

function PRES( &
           NOEUD , &
            SURF , &
              DZ , &
           PRGEO , &
            SGEO , &
          NMLARG , &
          ERREUR &
                 )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : CALCUL DU TERME DE PRESSION  EN FONCTION DE LA
!                     SURFACE MOUILLEE AUX INTERFACES DE CELLULES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  NOEUD    !  I !  D ! NOEUD CONSIDERE DU MAILLAGE                  !
! !  SURF     !  R !  D ! SURFACE MOUILLEE AU NOEUD                    !
! !  DZ       ! TR !  D ! PAS DE PLANIMETRAGE                          !
! !  PRGEO    ! TR !  D ! PRESSION PLANIMETRE AUX INTERFACES           !
! !  SGEO     ! TR !  D ! SURFACE MOUILLE PLANIMETREE AUX INTERFACES   !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!                             VARIABLES LOCALES
! .___________.____.____.______________________________________________.
! !  JG       !  I !  R ! BORNE GAUCHE DE L'INTERVALLE CONTENANT SURF  !
! !  JD       !  I !  R ! BORNE DROITE DE L'INTERVALLE CONTENANT SURF  !
! !  SG       !  R !  A ! SURFACE MOUILLE POUR LA BORNE GAUCHE         !
! !  SD       !  R !  A ! SURFACE MOUILLE POUR LA BORNE DROITE         !
! !  PRG      !  R !  A ! PRESSION POUR LA BORNE GAUCHE                !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!   PRGEO et SGEO font partie d'une structure de donnees STRUCTURE _SECTIONS

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES
   use M_ERREUR_T    ! ERREUR
   use M_DICHO_I     ! Interface du sous-programme DICHO

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)                                  :: PRES
   integer     ,                   intent(in)    :: NOEUD
   real(DOUBLE),                   intent(in)    :: SURF
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   real(DOUBLE), dimension(:,:)  , intent(in)    :: PRGEO,SGEO
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------

   integer        :: JG,JD
   real(DOUBLE)   :: SG,SD,PRG
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   ! !arbredappel_old    = trim(!Erreur%arbredappel)
   !  !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>PRES'

   ! RECHERCHE DE L'INTERVALLE CONTENANT SURF PAR DICHOTOMIE
   call DICHO( JG , JD , SURF , SGEO(Noeud,:) , ERREUR )
   if( Erreur%Numero /= 0 ) then
      PRES = 0.
      return
   endif

   ! SECTION MOUILLEE ET PRESSION AUX BORNES
   PRG = PRGEO(NOEUD,JG)
   SG  = SGEO (NOEUD,JG)
   SD  = SGEO (NOEUD,JD)

   ! INTERPOLATION DU TERME DE PRESSION
   ! ----------------------------------
   PRES = PRG + 0.5_DOUBLE * GPES * DZ(NOEUD) * ( ( SURF * SURF - SG * SG ) / ( SD - SG ) )

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

  return

end function PRES
