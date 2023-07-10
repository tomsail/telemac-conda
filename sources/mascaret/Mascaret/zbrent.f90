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

subroutine ZBRENT( &
                 FC1 , &
                 EPS , &
                  X1 , &
                  X2 , &
               ITMAX , &
          Impression , &
              Erreur )

!***********************************************************************
! PROGICIEL : MASCARET        J.-M. HERVOUET
!
! VERSION : V8P4R0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION  :  SOLUTION D'UNE EQUATION DONT UN ZERO UNIQUE EST ENTRE
!               LES POINTS X1 ET X2
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! !      NOM       !MODE!                   ROLE
! !________________!____!______________________________________________
! !   FC1          ! -->! FONCTION DONT ON CHERCHE LE ZERO
! !                !    ! DOIT ETRE DEFINIE EN REAL(DOUBLE)
! !                !    ! PAR AILLEURS
! !   EPS          ! -->! PRECISION CHERCHEE
! !   X1,X2        ! -->! ENCADREMENT DE LA SOLUTION ENTREE
! !                !<-->! X2 = SOLUTION EN SORTIE
! !   ITMAX        ! -->! NOMBRE MAXIMUM D'ITERATIONS
! !________________!____!______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
!  FONCTION APPELEE : FC1
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
   real(DOUBLE),                   external      :: FC1
   real(DOUBLE),                   intent(in)    :: EPS
   real(DOUBLE),                   intent(in)    :: X1
   real(DOUBLE),                   intent(inout) :: X2
   integer     ,                   intent(in)    :: ITMAX
   logical     ,                   intent(in)    :: Impression
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   integer LNG,LU
   integer ITER
   real(DOUBLE) A,B,C,D,E
   real(DOUBLE) FA,FB,FC
   real(DOUBLE) EPS2,XM,S,P,Q,R,EPS6
   !character(132) :: !arbredappel_old ! arbre d'appel precedent

   !============================= Instructions ===========================

   ! INITIALISATION
   !===============
   Erreur%Numero = 0
   !arbredappel_old    = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>ZBRENT'
   LU  = 6
   LNG = 1
   EPS6 = 1.D-6

   !-------------------------------------------
   !  ON VERIFIE QU'ON ENCADRE BIEN LA SOLUTION :
   A = X1
   B = X2
   FA = FC1(A)
   FB = FC1(B)
   if( FB * FA > 0.1_DOUBLE ) then
      if( LNG == 1 .and. Impression ) write(LU,*) 'ZBRENT : FC1(X1)*FC1(X2) EST POSITIF'
      Erreur%Numero = 1
      return
      !stop
   endif

   !  ITERATIONS :
   FC = FB
   do ITER = 1 , ITMAX
      if( FB * FC > 0._DOUBLE )then
         C  = A
         FC = FA
         D  = B - A
         E  = D
      endif
      if( dabs(FC) < dabs(FB) ) then
         A  = B
         B  = C
         C  = A
         FA = FB
         FB = FC
         FC = FA
      endif
      EPS2 = 0.5_DOUBLE * EPS
      XM   = 0.5_DOUBLE * ( C - B )
      if( dabs( XM ) <= EPS2.or.abs(FB).LT.EPS6 ) then
         X2 = B
         return
      endif
      if( dabs(E) >= EPS2.and.dabs(FA) > dabs(FB)) then
         S = FB / FA
         if( abs(A-C).LT.EPS6 ) then
            P = 2._DOUBLE * XM * S
            Q = 1._DOUBLE - S
         else
            Q = FA / FC
            R = FB / FC
            P = S * ( 2._DOUBLE * XM * Q * ( Q - R ) - ( B - A ) * ( R - 1._DOUBLE ) )
            Q = ( Q - 1._DOUBLE ) * ( R - 1._DOUBLE ) * ( S - 1._DOUBLE )
         endif
         if( P > 0._DOUBLE ) Q = -Q
         P = dabs(P)
         if( 2._DOUBLE * P  <  dmin1( 3._DOUBLE * XM * Q - dabs( EPS2 * Q ) , dabs( E * Q ) ) ) then
            E = D
            D = P / Q
         else
            D = XM
            E = D
         endif
      else
         D = XM
         E = D
      endif
      A  = B
      FA = FB
      if( dabs(D) > EPS2 ) then
         B = B + D
      else
         B = B + sign( EPS2 , XM )
      endif
      FB = FC1(B)
   end do

   if( LNG == 1 .and. Impression ) write(LU,*) 'ZBRENT : MAXIMUM D''ITERATIONS ATTEINT'
   X2 = B

   !------------------
   ! Fin du traitement
   !------------------

   !Erreur%arbredappel = !arbredappel_old

   return

end subroutine ZBRENT
