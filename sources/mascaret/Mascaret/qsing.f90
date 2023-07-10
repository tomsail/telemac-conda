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

function QSING( &
                C , &
               ZG , &
               HG , &
               ZD , &
               VG , &
             ZDEV , &
               DZ , &
            ALGEO , &
             IPOS , &
  Epaisseur_Seuil , &
           NMLARG , &
           ERREUR &
                  )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!  FONCTION :  CALCUL DU DEBIT TRANSITANT A TRAVERS UNE SINGULARITE
!                    EN STANDARD, EST PROGRAMME LE DEBIT TRANSITANT
!                     AU DESSUS D'UN SEUIL (NOYE ET DENOYE)
!                    L'UTILISATEUR PEUT PROGRAMMER SA PROPRE SINGULARITE
!                    ATTENTION : LA SINGULARITE EST POSITIONNEE A
!                                L'INTERFACE DES DEUX CELLULES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  C        !  R !    !                                              !
! !  ZG       !  R !  A ! COTE DANS LA CELLULE A GAUCHE DE LA SINGU    !
! !  HG       !  R !  A ! HAUTEUR D EAU A GAUCHE                       !
! !  ZD       !  R !  D ! COTE DANS LA CELLULE A DROITE DE LA SINGU    !
! !  ZDEV     !  R !  D ! COTE DU SEUIL                                !
! !  DZ       ! TR !  M ! PAS DE PLANIMETRAGE                          !
! !  ALGEO    ! TR !  M ! TABLEAU DES LARGEURS PLANIMETREES            !
! !  IPOS     !  I !  D ! POSITION DU BARRAGE                          !
! !  NMLARG   !  I !  D !                                              !
! !___________!____!____!______________________________________________!
!
!     TYPE : I (ENTIER), R (REEL), A (ALPHANUMERIQUE), T (TABLEAU)
!            L (LOGIQUE)   .. ET TYPES COMPOSES (EX : TR TABLEAU REEL)
!     MODE : D (DONNEE NON MODIFIEE), R (RESULTAT), M (DONNEE MODIFIEE)
!            A (AUXILIAIRE MODIFIE)
!
!***********************************************************************
!    ALGEO fait partie d'une structure de donnees

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, W32
   use M_ERREUR_T    ! ERREUR
   use M_ALARG_I     ! Interface de la fonction ALARG

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE)                                  :: QSING,VG
   real(DOUBLE),                   intent(in)    :: C
   real(DOUBLE),                   intent(in)    :: ZG,HG,ZD,ZDEV
   ! 1ere dimension IM
   real(DOUBLE), dimension(:)    , intent(in)    :: DZ
   real(DOUBLE), dimension(:,:)  , intent(in)    :: ALGEO
   integer     ,                   intent(in)    :: IPOS
   integer     ,                   intent(in)    :: Epaisseur_Seuil
   integer     ,                   intent(in)    :: NMLARG
   Type (ERREUR_T)                ,intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   real(DOUBLE)   :: LDEV,HDEV,R,K,CHARGE,R1
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>QSING'

   !      CALCUL DE LA HAUTEUR DE DEVERSEMENT
   !             DE LA LARGEUR A LA HAUTEUR HG
   HDEV   = ZG - ZDEV
   LDEV   = ALARG( IPOS , HG , DZ , ALGEO , NMLARG , ERREUR )
   CHARGE = HDEV

   !      LE SEUIL EST-IL NOYE
   R    = ( ZD - ZDEV ) / ( ZG - ZDEV )
   LDEV = ALARG( IPOS , HG , DZ , ALGEO , NMLARG , ERREUR )
   R1   = ZD - ZDEV
   if( Erreur%Numero /= 0 ) then
       QSING = 0.
      return
   endif

   if( Epaisseur_seuil == 1 ) then
      !
      !  SEUIL EPAIS
      !
      if( R > 0.8_DOUBLE ) then
         K = -25._DOUBLE * ( R**2 ) + 40._DOUBLE * R - 15._DOUBLE
      else
         K = 1._DOUBLE
      endif
   else
      !
      !  SEUIL MINCE
      !
      if( R1 > 0._DOUBLE )  then
         K = (1._DOUBLE -R**1.5_DOUBLE)**0.385_DOUBLE
      else
         K = 1._DOUBLE
      endif
   endif

   QSING = K * C * LDEV * CHARGE* dsqrt( 2._DOUBLE * GPES )  * (CHARGE)**(W12)

   !       ICI L'UTILISATEUR PEUT PROGRAMMER SA PROPRE FORMULE DE DEBIT

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end function QSING
