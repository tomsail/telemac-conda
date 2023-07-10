!== Copyright (C) 2000-2022 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

SUBROUTINE CALK( RK , RKC , &
                 U , ST , B , H , &
                 Nbsect , NOPTK )

!*****************************************************************************
! PROGICIEL : TRACER         S. MANDELKERN   -   E. LEHMANN
!
! VERSION : V8P4R0              EDF-CEREMA
!*****************************************************************************
!
!  FONCTION :
!  --------
!            CALCUL DU COEFFICIENT DE DIFFUSION
!
!_________________________________________________________________________________
!   NOM   !TYPE!MODE!                      ROLE                                  !
!_________!____!____!____________________________________________________________!
!                                                                                !
!                              PARAMETRES D'APPEL                                !
!________________________________________________________________________________!
!   RK    ! R  !    ! COEFFICIENT DE DIFFUSION                                   !
!   IM    ! E  !    ! Dimension du systeme                                       !
!   X     ! TR !    ! Position de la section (abscisse)                          !
!   H     ! TR !    ! Hauteur d'eau                                              !
!   A     ! TR !    ! Section mouillee                                           !
!   U     ! TR !    ! Champ du vecteur vitesse                                   !
!________________________________________________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS ENTREE/SORTIE :
!   ----------------------
!
!   SOUS-PROGRAMMES APPELANT : TRACER
!   ------------------------
!***********************************************************************

   use M_PRECISION
   use M_PARAMETRE_C
   !
   ! * DECLARATION DES ARGUMENTS *
   real (DOUBLE) , dimension(Nbsect) , intent(inout) :: RK
   real (DOUBLE) , dimension(2)      , intent(inout) :: RKC
   real (DOUBLE) , dimension(Nbsect) , intent(in   ) :: U , ST, B , H
   integer Nbsect , NOPTK
   !
   ! * DECLARATION DES VARIABLES LOCALES *
   integer I
   real(DOUBLE) :: Ueff , EPST
   !
   ! --------------------------------------------------------------------
   !
   ! * CAS OU RK=F(U) *
   !
   if( NOPTK.EQ.1 ) THEN
      do I = 1 , Nbsect
         RK(I) = RKC(1) * U(I) + RKC(2)
         if( RK(I).LT.0 ) THEN
            RK(I) = W0
         endif
      enddo
   endif
   ! --------------------------------------------------------------------
   !
   ! * Formule Elder (1959)
   !
   if( NOPTK == 2 ) THEN
      do I = 1 , Nbsect
         Ueff  = GPES**W12 * U(I) / ( ST(I) * H(I)**W16 )
         RK(I) = 5.93d0 * H(I) * Ueff
      enddo
   endif
   ! --------------------------------------------------------------------
   !
   ! * Formule Fisher (1975)
   !
   if( NOPTK == 3 ) THEN
      do I = 1 , Nbsect
         Ueff = GPES**W12 * U(I) / ( ST(I) * H(I)**W16 )
         IF( Ueff.LE.EPS6 )THEN
            RK(I) = W0
         ELSEIF( Ueff.GT.EPS6 )THEN
            RK(I)= 0.011d0 * H(I) * Ueff * &
                  ( B(I) / H(I) )**2 *( U(I) / Ueff )**2
         ENDIF
      enddo
   endif
   ! --------------------------------------------------------------------
   !
   ! * Formule Liu (1977)
   !
   if( NOPTK == 4 ) THEN
      do I = 1 , Nbsect
         Ueff = GPES**(0.5) * U(I) / ( ST(I) * H(I)**W16 )
         IF( Ueff.LE.EPS6 )THEN
            RK(I) = W0
         ELSEIF( Ueff.GT.EPS6 )THEN
            RK(I) = 0.18d0 * H(I) * Ueff * &
                   ( B(I) / H(I) )**2 * ( U(I) / Ueff )**W12
         ENDIF
      enddo
   endif
   ! --------------------------------------------------------------------
   !
   ! * ISAWA & AYA (1991)
   !
   if( NOPTK == 5 ) THEN
      do I = 1 , Nbsect
         Ueff = GPES**W12 * U(I) / ( ST(I) * H(I)**W16 )
         IF( Ueff.LE.EPS6 )THEN
            RK(I) = W0
         ELSEIF( Ueff.GT.EPS6 )THEN
            RK(I) = 2 * H(I) * Ueff * ( B(I) / H(I) )**W32
         ENDIF
      enddo
   endif
   !---------------------------------------------------------------------
   !
   !FORMULE DE MCQUIVEY ET KEEFER (1974)
   !VALABLE UNIQUEMENT POUR FROUDE<=0.5, BIEN ADAPTE POUR LES ETIAGES (UTILISEE EN INGENIERIE ENVIRONEMENTALE)
   !
   if( NOPTK == 6 ) THEN
      do I = 1 , Nbsect
         Ueff = GPES**W12 * U(I) / ( ST(I) * H(I)**W16 )
         RK(I)= 0.058d0 * U(I) * Ueff**2 / GPES
      enddo
   endif
   !
   !----------------------------------------------------------------------
   !
   !FORMULE DE KASHEFIPOUR ET FALCONER (2002)
   !
   if( NOPTK==7 ) THEN
      do I = 1 , Nbsect
         Ueff = GPES**W12 * U(I) / ( ST(I) * H(I)**W16 )
         IF( Ueff.LE.W0) THEN
            RK(I) = W0
         ELSEIF( Ueff.GT.W0) THEN
            IF( B(I).GE.50.d0 * H(I) )THEN
               RK(I) = 10.612d0 * H(I) * U(I) * ( U(I) / Ueff )
            ELSEIF( B(I).LT.50.d0 * H(I) )THEN
               EPST = 7.428d0 + 1.775d0 * ( B(I) / H(I)**(0.62) ) * &
                      ( Ueff / U(I) )**0.572d0
               RK(I) = EPST * H(I) * U(I) * ( U(I) / Ueff )
            Endif
         endif
      enddo
   endif
   !
   !-----------------------------------------------------------------------
   !
   !FORMULE DE Magazine (1988)
   !
   if( NOPTK == 8 ) THEN
      do I = 1 , Nbsect
         IF( U(I).LE.EPS6 )THEN
            RK(I) = W0
         ELSEIF( U(I).GT.EPS6 )THEN
            Ueff = GPES**W12 * U(I) / ( ST(I) * H(I)**W16 )
            RK(I)= 75.86d0 * H(I) * U(I) *( Ueff / ( 0.4d0 *U(I)))**(1.632d0)
         ENDIF
      enddo
   endif
   !-----------------------------------------------------------------------
   !
   !FORMULE DE Koussis-Rodriguez et Mirasol (1998)
   !
   if( NOPTK == 9 ) THEN
      do I = 1 , Nbsect
         Ueff  = GPES**W12 * U(I) / ( ST(I) * H(I)**W16 )
         RK(I) = 0.6d0 * H(I) * Ueff * ( B(I) / H(I) )**2
      enddo
   endif
   !-----------------------------------------------------------------------
   !
   !FORMULE DE Seo et Cheong (1998)
   if( NOPTK == 10 ) THEN
      do I = 1 , Nbsect
         Ueff = GPES**W12 * U(I) / ( ST(I) * H(I)**W16 )
         IF( Ueff.LE.EPS6 )THEN
            RK(I) = W0
         ELSEIF( Ueff.GT.EPS6 )THEN
            RK(I) = 5.92d0 * H(I) * Ueff * &
                    ( B(I) / H(I) )**(0.62d0) * ( U(I) / Ueff )**(1.43d0)
         ENDIF
      enddo
   endif
   !-----------------------------------------------------------------------
   !FORMULE de Deng et al. (2001)
   if( NOPTK == 11 ) THEN
      do I = 1 , Nbsect
         Ueff = GPES**W12 * U(I) / ( ST(I) * H(I)**W16 )
         IF( Ueff.LE.EPS6 )THEN
            RK(I) = W0
         ELSEIF( Ueff.GT.EPS6 )THEN
            EPST  = 0.145d0 + ( U(I) / Ueff ) * ( B(I) / H(I) )**(1.38d0) / 3520.d0
            RK(I) = 0.15d0 * H(I) * Ueff / ( 8.d0 * EPST ) * &
                    ( B(I) / H(I) )**(1.67d0) * ( U(I) / Ueff )**2
         ENDIF
      enddo
   endif
   ! --------------------------------------------------------------------
   !

   RETURN

END SUBROUTINE CALK
