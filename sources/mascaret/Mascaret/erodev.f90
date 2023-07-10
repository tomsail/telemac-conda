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

subroutine ERODEV( &
                ZDEV , &
                  ZG , &
                  HG , &
                  QD , &
                IPOS , &
               ALGEO , &
                  DZ , &
                  DT , &
              NMLARG , &
              ERREUR )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : CALCUL  DE L'EROSION D'UNE CODE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  ZDEV     !  R !  R ! COTE DE DEVERSEMENT                          !
! !  ZG       !  R !  D ! COTE DE LA CELLULE GAUCHE                    !
! !  HG       !  R !  D ! COTE DE LA HAUTEUR A GAUCHE                  !
! !  QD       !  R !  D ! COTE DE LA CELLULE DROITE                    !
! !  IPOS     !  I !  D ! INDICE DE LA SECTION                         !
! !  ALGEO    ! TR !  D ! LARGEUR PLANIMETREE                          !
! !  DZ       ! TR !  R ! PAS DE PLANIMETRAGE                          !
! !  DT       !  R !  R ! PAS DE TEMPS                                 !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!     TYPE : E (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!    ALGEO  fait partie d'une structure de donnees STRUCTURE_SECTION

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, EPS4
   use M_ERREUR_T    ! ERREUR
   use M_ALARG_I     ! Interface de la fonction ALARG

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(inout) :: ZDEV
   real(DOUBLE),                   intent(in)    :: ZG,HG,QD
   integer     ,                   intent(in)    :: IPOS
   ! 1ere dimension IM, 2nde dimension NMLARG
   real(DOUBLE), dimension(:,:)  , intent(in)    :: ALGEO
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   real(DOUBLE),                   intent(in)    :: DT
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)                ,intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE) :: LAZ,LAHAUT,LABAS,ZHAUT,ZBAS
   real(DOUBLE) :: D50,STRICK,TAUCRI,HCRIT,TAUAMO,TAUAVA
   real(DOUBLE) :: ROEAU,ROSOL,POROS
   real(DOUBLE) :: LAMBDA,HAMONT,HAVAL
   real(DOUBLE) :: VAMONT,VAVAL,FRAMON
   real(DOUBLE) :: HC,HCG,HCD,FR2,DELTAZ
   real(DOUBLE) :: QSAMON,QSAVAL
   real(DOUBLE) :: LDEV
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>ERODEV'
   ROEAU = 1000._DOUBLE
   ROSOL = 2650._DOUBLE
   POROS = 0.4_DOUBLE
   ! DONNEES FOURNIES PAR L'UTILISATEUR
   LAHAUT= 5.0_DOUBLE
   ZHAUT = 418.50_DOUBLE
   LABAS = 70._DOUBLE
   ZBAS  = 375.00_DOUBLE
   D50   = 0.10_DOUBLE
   ! CALCULS
   STRICK = 21._DOUBLE / ( D50**( 1._DOUBLE / 6._DOUBLE ) )
   TAUCRI = 0.05_DOUBLE * ( ROSOL - ROEAU ) * GPES * D50
   LAMBDA = 8._DOUBLE / ( GPES * dsqrt( ROEAU ) * ( ROSOL - ROEAU ) )

   ! GEEOMETRIE DE LA DIGUE 
   LDEV = ALARG( IPOS , HG , DZ , ALGEO , NMLARG , ERREUR )
   if( Erreur%Numero /= 0 ) then
      return
   endif

   LAZ = ( ZDEV - ZBAS ) / ( ZHAUT - ZBAS ) * ( LAHAUT - LABAS ) + LABAS
   HAMONT = ( ZG - ZDEV )
   FRAMON = ( QD * QD ) / ( GPES * LDEV**2 * HAMONT**3 )
   if( FRAMON > 1._DOUBLE ) return

   ! CALCUL DE LA HAUTEUR CRITIQUE AVAL
   HCG    = 0._DOUBLE
   HCD    = ( ZG - ZDEV )
   DELTAZ = 0._DOUBLE

   do while( DELTAZ > EPS4 )

     HC  = ( HCG + HCD ) / 2._DOUBLE
     FR2 = ( QD * QD ) / ( GPES * LDEV**2 * HC**3 )
     if( FR2 > 1._DOUBLE ) then
        HCG = HC
     else
        HCD = HC
     endif
     DELTAZ = dabs( HCD - HCG )

   end do

   HCRIT = ( HCG + HCD ) / 2._DOUBLE

   ! CALCUL DES CONTRAINTES
   HAMONT = ( ZG - ZDEV )
   HAVAL  = HCRIT
   VAMONT = QD / ( HAMONT * LDEV )
   VAVAL  = QD / ( HAVAL * LDEV )
   TAUAMO = ROEAU * GPES * VAMONT**2 / ( STRICK**2 * HAMONT**( 1._DOUBLE / 3._DOUBLE ) )
   TAUAVA = ROEAU * GPES * VAVAL**2 / ( STRICK**2*HAVAL**( 1._DOUBLE / 3._DOUBLE ) )

   ! CALCUL DU DEBIT SOLIDE
   if( TAUAMO > TAUCRI ) then
      QSAMON = LAMBDA * ( TAUAMO - TAUCRI )**( 3._DOUBLE / 2._DOUBLE )
   else
      QSAMON = 0._DOUBLE
   endif

   if( TAUAVA > TAUCRI ) then
      QSAVAL = LAMBDA * ( TAUAVA - TAUCRI )**( 3._DOUBLE / 2._DOUBLE )
   else
      QSAVAL = 0._DOUBLE
   endif

   ! CALCUL DE LA VARIATION DE HAUTEUR
   ZDEV = ZDEV + DT * ( QSAMON - QSAVAL ) / ( LAZ * ( 1._DOUBLE - POROS ) )

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine ERODEV
