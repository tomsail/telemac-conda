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

subroutine BOUSSI( &
                QNODE , &
                SNODE , &
                YNODE , &
                    Z , &
               SCUBE1 , &
                SCUBE , &
                SPREC , &
                QPREC , &
                   CE , &
                   ZF , &
                NSECG , &
               NSECDL , &
                    X , &
                   DX , &
                   DT , &
                   NS , &
               ERREUR &
                  )

!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : V8P4R0              EDF-CEREMA
!
!***********************************************************************

   !.. Modules importes ..
   !----------------------
   use M_PRECISION
   use M_ERREUR_T    ! ERREUR
   use M_PARAMETRE_C ! GPES, EPS3, EPS6, EPSN6
   use M_CELE_I      ! Interface de la fonction    CELE
   use M_CSUR_I      ! Interface de la fonction    CSUR
   use M_PRESD_I     ! Interface de la fonction    PRESD
   use M_DICHODM_I

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Arguments ..
   !---------------
   real(DOUBLE), dimension(:),     intent(inout) :: QNODE
   real(DOUBLE), dimension(:),     intent(in)    :: SNODE,SCUBE,SCUBE1
   real(DOUBLE), dimension(:),     intent(in)    :: SPREC,YNODE
   real(DOUBLE), dimension(:),     intent(in)    :: QPREC
   real(DOUBLE), dimension(:),     intent(in)    :: CE,X
   real(DOUBLE), dimension(:),     intent(in)    :: ZF,Z
   integer     ,                   intent(in)    :: NSECG
   integer     ,                   intent(in)    :: NSECDL
   real(DOUBLE),                   intent(in)    :: DX
   real(DOUBLE),                   intent(in)    :: DT
   integer     ,                   intent(in)    :: NS
   Type (ERREUR_T)               , intent(inout) :: ERREUR

   !.. Variables locales ..
   !-----------------------
   integer      :: ENERG
   integer      :: INFO,NOEUDC,is,ii
   real(double) :: dA1m(NS),dA1p(NS),Scubem(NS)
   real(double) :: A1(NS),A1p(NS-1),A1m(NS-1)
   real(double) :: A0(NS),A0p(NS-1),A0m(NS-1),B(NS)
   real(double) :: qn0(NS),zero,un,deux,trois
   real(double) :: ZFm(NS+1)
   real(double) :: deltax2
   real(double) :: dzb(NS)
   real(double) :: qn(NS),qo(NS),alpha(NS),H(NS),DELTAX(NS)
   !

   ENERG   = 1
   Deltax2 = DX**2
   deux    = 2._DOUBLE
   trois   = 3._DOUBLE
   un      = 1._DOUBLE
   zero    = 0._DOUBLE

   do ii = 1 , NS - 1
      ZFm(ii) = dmax1( ZF(ii) , ZF(ii+1) )
   enddo

   ZFm(NS)   = ZF(NS)
   ZFm(NS+1) = ZF(NS)

   do is = 2 , NS - 1
      dzb(is) = ( ZF(is+1) - ZF(is-1) ) / deux
   enddo

   dzb(NS) = ZF(NS) - ZF(NS-1)
   dzb(1)  = ZF(2) - ZF(1)

   !
   ! A verifier le decalage des noeuds
   do ii = 1 , NS
      SCUBEm(ii) = ( SCUBE1(ii) + SCUBE(II) ) / 2.D0
      H(ii)      = ( Z(ii) - ZF(ii) + YNODE (ii) ) / 2.D0
      NOEUDC     = NSECG + ii - 1
      qo(ii)     = QNODE(NOEUDC)
      qn(ii)     = QPREC(NOEUDC)
      alpha(ii)  = ( ( SNODE(ii) + SPREC(ii) ) / 2.D0 * H(ii)**2 - Scubem(ii) ) / 2.D0
   enddo

   do ii = 2 , NS - 1
      deltax(ii) = 0.5d0 * ( X(ii+1) - X(ii-1) )
   enddo

   deltax(1)  = X(2) - X(1)
   deltax(NS) = X(NS) - X(NS-1)

   if (ENERG.eq.0) then
   elseif (ENERG.eq.1) then
   else
      print*,'Wrong definition for variable : ENERG'
   endif

   do is = 2 , NS - 1
      A0(is) = 1.0d0 + 2.0d0 * alpha(is) / deltax(is) / SPREC(is)  &
              * ( 1.0d0 / ( deltax(is+1) + deltax(is) ) + 1.0d0 / ( deltax(is) + deltax(is-1) ) )
      A1(is) = 1.0d0 + 2.0d0 * alpha(is) / deltax(is) / SNODE(is) &
              *( 1.0d0 / ( deltax(is+1) + deltax(is) ) + 1.0d0 / ( deltax(is) + deltax(is-1) ) )
   enddo

   A0(1)  = 1.0d0 + 2.0d0 * alpha(1) / deltax(1) / SPREC(1) *1.0d0 / ( deltax(2) + deltax(1) )
   A0(NS) = 1.0d0 + 2.0d0 * alpha(NS) / deltax(NS) / SPREC(NS) * 1.0d0 / ( deltax(NS-1) + deltax(NS) )
   A1(1)  = 1.0d0 + 2.0d0 * alpha(1) / deltax(1) / SNODE(1) * 1.0d0 / ( deltax(2) + deltax(1) )
   A1(NS) = 1.0d0 + 2.0d0 * alpha(NS) / deltax(NS) / SNODE(NS) * 1.0d0 / ( deltax(NS-1) + deltax(NS) )

   do is = 2 , NS
      A0m(is-1) = -2.0d0 * alpha(is) / deltax(is-1) / SPREC(is-1) / ( deltax(is-1) + deltax(is) )
      A1m(is-1) = -2.0d0 * alpha(is) / deltax(is-1) / SNODE(is-1) / ( deltax(is-1) + deltax(is) )
   enddo

   do is=1, ns-1
      A0p(is) = -2.0d0 * alpha(is) / deltax(is) / SPREC(is+1) / ( deltax(is) + deltax(is+1) )
      A1p(is) = -2.0d0 * alpha(is) / deltax(is) / SNODE(is+1) / ( deltax(is) + deltax(is+1) )
   enddo

   do is=1, NS
      B(is) = CE(is)
   enddo

   do is = 1 , NS - 1
      dA1m(is+1) = A1m(is)
      dA1p(is)   = A1p(is)
   enddo

   dA1m(1)  = 0.0d0
   dA1p(NS) = 0.0d0

   do is = 2 , NS - 1
      qn0(is) = B(is) + A0m(is-1) * qn(is-1) + A0(is) * qn(is) + A0p(is) * qn(is+1)
   enddo

   qn0(1)  = B(1) + A0(1) * qn(1) + A0p(1) * qn(2)
   qn0(NS) = B(NS) + A0m(NS-1) * qn(NS-1) + A0(NS) * qn(NS)

   call dgtsl( NS , dA1m , A1 , dA1p , qn0 , INFO )

   do is = 1 , NS
      NOEUDC        = NSECG + is - 1
      QNODE(NOEUDC) = qn0(is)
   enddo

   return

end subroutine BOUSSI
