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

subroutine SOLEX ( &
               CGAUC , &
               YGAUC , &
               YDROI , &
                  WR , &
                 URG , &
                 CRG , &
                 YRG , &
                   T , &
                RINV , &
                   X , &
                XBAR , &
               ALARG , &
               SVRAI , &
               QVRAI , &
               UVRAI , &
               ZVRAI , &
               YVRAI , &
              NBSECT , &
              Erreur  &
                      )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!     FONCTION : SOLUTION ANALYTIQUE DE LA RUPTURE DE BARRAGE 
!
!-----------------------------------------------------------------------
!           DANS LE PLAN (X,T)
!           ON APPELLE C L'INTERSECTION A T AVEC X = -CGAUC*T
!                      D L'INTERSECTION A T AVEC X = (URG - CRG)*T
!                      E L'INTERSECTION A T AVEC X =  WR*T
!                                          WR:VITESSE DU RESSAUT
!           A ET B SONT LES LIMITES DU DOMAINE D'ETUDE
!           A GAUCHE DE C L EAU EST IMMOBILE DE COTE YG
!           ENTRE C ET D ON A UNE ONDE DE DETENTE
!           ENTRE D ET E L EAU EST A VITESSE URG ET DE COTE (CRG**2)/G
!           A DROITE DE E L EAU EST IMMOBILE DE COTE YD
!-----------------------------------------------------------------------

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_PARAMETRE_C ! GPES, W23, EPS6
   use M_ERREUR_T    ! ERREUR

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE),                   intent(in)    :: CGAUC,YGAUC,YDROI
   real(DOUBLE),                   intent(in)    :: WR,URG,CRG,YRG
   real(DOUBLE),                   intent(in)    :: T
   integer     ,                   intent(in)    :: RINV
   real(DOUBLE), dimension(:)    , intent(in)    :: X
   real(DOUBLE),                   intent(in)    :: XBAR
   real(DOUBLE),                   intent(in)    :: ALARG
   real(DOUBLE), dimension(:)    , intent(  out) :: SVRAI,QVRAI,UVRAI
   real(DOUBLE), dimension(:)    , intent(  out) :: ZVRAI,YVRAI
   integer     ,                   intent(in)    :: NBSECT
   Type (ERREUR_T)               , intent(inout) :: Erreur

   !.. Variables locales ..
   !-----------------------
   integer      :: NOEUD
   real(DOUBLE) :: YEXACT
   real(DOUBLE) :: XC,XD,XE,XCOUR,XCOUR1
   real(DOUBLE) :: SVRAIN(NBSECT), QVRAIN(NBSECT)
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>SOLEX'

   !     ABSCISSES DES POINTS C , D ET E
   !     -------------------------------
   XC = -CGAUC * T + XBAR 
   XD = ( URG - CRG ) * T + XBAR 
   XE = WR * T + XBAR 

   do NOEUD = 1 , NBSECT
      XCOUR = X(NOEUD)
      if( XCOUR <= XC ) then
         !                         XXXX
         !                         EN XCOUR LE NIVEAU EST YGAUC
         SVRAI(NOEUD) = YGAUC * ALARG
         QVRAI(NOEUD) = 0._DOUBLE
         cycle
      endif
      !                         XXXXX
      if( XCOUR <= XD ) then
         !                         XXXX
         !                         EN XCOUR POINT DE L'ONDE DE DETENTE
         XCOUR1       = XCOUR - XBAR
         YEXACT       = (( W23 * CGAUC - ( XCOUR1 / ( 3._DOUBLE * T ) ) )**2 ) / GPES
         SVRAI(NOEUD) = ALARG * YEXACT
         QVRAI(NOEUD) = ALARG * 2._DOUBLE * ( CGAUC - dsqrt( GPES * YEXACT ) ) * YEXACT
         cycle
      endif
      !                         XXXXX
      if( XCOUR <= XE ) then
         !                         XXXX
         !                         EN XCOUR TIRANT D'EAU A GAUCHE DU RESSAUT
         YEXACT       = YRG
         SVRAI(NOEUD) = ALARG * YEXACT
         QVRAI(NOEUD) = URG * YEXACT * ALARG
         cycle
      endif
      !                         XXXXX
      !        SINON EN XCOUR TIRANT D'EAU A DROITE DU RESSAUT
      SVRAI(NOEUD) = YDROI * ALARG
      QVRAI(NOEUD) = 0._DOUBLE
   end do
   !        SI PROBLEME DE RIEMANN INVERSE (TIRANT AMONT<TIRANT AVAL)
   !        ******************************
   if( RINV == 1 ) then
      do NOEUD = 1 , NBSECT
         SVRAIN(NOEUD) = SVRAI( NBSECT + 1 - NOEUD )
         QVRAIN(NOEUD) = QVRAI( NBSECT + 1 - NOEUD )
      end do
      do NOEUD = 1,NBSECT
         SVRAI(NOEUD) = SVRAIN(NOEUD)
         QVRAI(NOEUD) = -QVRAIN(NOEUD)
      end do
   endif

   !        CALCUL DES VARIABLES YVRAI,ZVRAI,UVRAI
   !        **************************************
   do NOEUD = 1 , NBSECT
      ZVRAI(NOEUD) = SVRAI(NOEUD) / ALARG
      if( SVRAI(NOEUD) < EPS6 ) then
         UVRAI(NOEUD) = QVRAI(NOEUD) / EPS6
      else
         UVRAI(NOEUD) = QVRAI(NOEUD) / SVRAI(NOEUD)
      endif
      YVRAI(NOEUD) = ZVRAI(NOEUD)
   end do

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine SOLEX
